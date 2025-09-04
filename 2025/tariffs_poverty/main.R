#------------------------------------------------------------------------------
# main.R 
# 
# Contains calculations for TBL's Sept. 2025 blog post on tariffs and poverty
#------------------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
library(quantregForest)

# TODO trade shock business...
# TODO ask codex to reorganize modularly
# TODO claude review

# Switch for loading precalculated tax offset (non-TBL users must set to TRUE)
load_precalculated_tax_offset = T

#-----------
# Read data
#-----------

# load IPUMS extract
cps = read_csv("resources/ipums_asec.csv.gz")

# read macro projections file 
macro_projections = bind_rows(
  read_csv("resources/macro_projections/historical.csv"),
  read_csv("resources/macro_projections/projections.csv")
)

# read tariff shock parameter files
crosswalk     = read_csv("resources/tariff_inputs/crosswalk.csv")
fcsuti_shares = read_csv("resources/tariff_inputs/fcsuti_shares.csv")


#-----------------------------
# Calculate CPI-U price shock
#-----------------------------

# pull out trade impact 
trade_effect = crosswalk %>% 
  filter(gtap_category == 'Trade') %>% 
  mutate(allocation = gtap_weight * tariff_price_shock) %>% 
  select(allocation) %>% 
  deframe()


cpiu_shock = crosswalk %>%
  mutate(
    trade_allocation_weight = (gtap_weight * trade_allocation) / sum(gtap_weight * trade_allocation, na.rm = T),   
    trade_price_shock       = trade_effect * trade_allocation_weight, 
    total_price_shock       = tariff_price_shock + trade_price_shock
  ) %>% 
  group_by(cpi_category) %>% 
  summarise(
    weight             = mean(cpi_weight) / 100,
    tariff_price_shock = weighted.mean(tariff_price_shock, gtap_weight),
    total_price_shock  = weighted.mean(total_price_shock, gtap_weight)
  ) %>% 
  filter(!is.na(cpi_category)) %>% 
  summarise(
    weighted.mean(total_price_shock, weight)
  ) %>% 
  deframe()



#------------------------------
# Calculate FCSUti price shock
#------------------------------

fcsuti_shares = fcsuti_shares %>% 
  
  # collapse ti into one category
  mutate(
    category = if_else(category %in% c('Telephone', 'Internet'), 'ti', category)
  ) %>% 
  group_by(tenure, category) %>% 
  summarise(
    value     = sum(value), 
    share     = mean(share), 
    threshold = mean(threshold), 
    .groups = 'drop'
  ) %>% 
  
  # calculate overall FCSUti weights
  group_by(spm_category = category) %>% 
  summarise(
    weight = weighted.mean(value, share * threshold)
  ) %>% 
  mutate(
    weight = weight / sum(weight)
  )

fcsuti_shock = crosswalk %>% 
  filter(!is.na(spm_category)) %>%
  
  # Calcualte average tariff price shock by SPM category
  group_by(spm_category) %>% 
  summarise(
    tariff_price_shock = weighted.mean(tariff_price_shock, gtap_weight)
  ) %>% 
  
  # Calculate implied change in FCSUti 
  left_join(
    fcsuti_shares, by = 'spm_category'
  ) %>% 
  summarise(
    tariff_price_shock = weighted.mean(tariff_price_shock, weight)
  )  %>% 
  deframe()



#----------
# Age data
#----------

# calculate weight adjustment factors reflecting demographic change
demographic_factors = macro_projections %>% 
  filter(year %in% c(2023, 2026)) %>% 
  select(year, contains("married")) %>% 
  pivot_longer(
    cols      = -year,
    names_sep = "_",
    names_to  = c("marital_status", "age")
  ) %>% 
  mutate(age = pmin(as.integer(age), 85)) %>% 
  group_by(year, marital_status, age) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  group_by(marital_status, age) %>% 
  mutate(index = value / value[year == 2023]) %>% 
  ungroup() %>% 
  select(year, marital_status, age, value, index)

# calculate per-capita income growth
economic_factor = macro_projections %>% 
  filter(year %in% c(2023, 2026)) %>% 
  left_join(
    demographic_factors %>% 
      group_by(year) %>% 
      summarise(pop = sum(value) / 1e9), 
    by = 'year'
  ) %>%
  mutate(
    gdp = gdp / pop,
    gdp = gdp / lag(gdp)
  ) %>% 
  filter(year == 2026) %>% 
  select(gdp) %>%
  deframe()

# join demographic growth factors on CPS data
cps = cps %>% 
  mutate(
    marital_status = if_else(MARST %in% 1:2, "married", "unmarried"), 
    age = if_else(AGE == 15 & marital_status == "married", 16, AGE) # No married 15 year olds in CBO data; assign them 16 year old growth rate 
  ) %>% 
  left_join(
    demographic_factors %>% 
      filter(year == 2026) %>% 
      select(marital_status, age, weight_index = index), 
    by = c("marital_status", "age")
  )
  
# calculate implied OASDI recipient growth
oasdi_extensive_margin_factor = cps %>% 
  filter(INCSS > 0 & INCSS != 999999) %>% 
  summarise(
    oasdi_n_growth = sum(ASECWT * weight_index) / sum(ASECWT) 
  ) %>% 
  deframe()

# calculate intensive margin growth factor for OASDI
oasdi_factor = macro_projections %>% 
  filter(year %in% c(2023, 2026)) %>% 
  mutate(oasdi = outlays_mand_oasdi / lag(outlays_mand_oasdi, 1)) %>% 
  filter(year == 2026) %>%
  select(oasdi) %>% 
  deframe() %>% 
  `/`(oasdi_extensive_margin_factor)

# calculate baseline inflation factor
baseline_cpiu_factor =  macro_projections %>% 
  filter(year %in% c(2023, 2026)) %>% 
  mutate(cpiu = cpiu / lag(cpiu, 1)) %>% 
  filter(year == 2026) %>%
  select(cpiu) %>% 
  deframe()


#--------------------
# Do OPM calculation
#--------------------

opm_microdata = cps %>%
  mutate(
    oasdi = replace_na(na_if(INCSS,   999999),  0),
    ssi   = replace_na(na_if(INCSSI,  999999),  0),
    vet   = replace_na(na_if(INCVET, 9999999),  0)
  ) %>%
  
  # Assign census-concept family IDs
  group_by(SERIAL) %>%
  mutate(
    opm_id = case_when(
      FTYPE %in% c(1, 3) ~ paste0(SERIAL, "_PF"),
      FTYPE == 4         ~ paste0(SERIAL, "_UF_", FAMUNIT),
      FTYPE %in% c(2, 5) ~ paste0(SERIAL, "_NF_", PERNUM),
      TRUE               ~ paste0(SERIAL, "_UNK_", PERNUM)
    )
  ) %>%
  ungroup() %>%
  
  # calculate unit-level sums and projected family income for 2026 (pre-tariff)
  group_by(opm_id) %>%
  mutate(
    
    opm_oasdi        = sum(oasdi),
    opm_ssi          = sum(ssi),
    opm_vet          = sum(vet),
    opm_indexed      = opm_oasdi + opm_ssi + opm_vet,
    opm_other_income = OFFTOTVAL - opm_indexed,
    
    # age forward
    opm_other_income_2026 = opm_other_income * economic_factor, 
    opm_vet_2026          = opm_vet          * economic_factor, 
    opm_ssi_2026          = opm_ssi          * economic_factor, 
    opm_oasdi_2026        = opm_oasdi        * oasdi_factor,
    
    opm_indexed_2026 = opm_oasdi_2026 + opm_ssi_2026 + opm_vet_2026,
    opm_income_2026  = opm_other_income_2026 + opm_indexed_2026,
    
    poverty_line = OFFCUTOFF * baseline_cpiu_factor
  ) %>%
  ungroup() %>%
  
  # do poverty status calculations for each scenario
  mutate(
    baseline = opm_income_2026                                     <  poverty_line,
    gross    = opm_income_2026                                     <  poverty_line * (1 + cpiu_shock),
    net      = (opm_income_2026 + (opm_indexed_2026 * cpiu_shock)) <  poverty_line * (1 + cpiu_shock)
  ) %>%
  select(weight = ASECWT, weight_index, age, baseline, gross, net) %>% 
  pivot_longer(
    cols      = c(gross, net),
    names_to  = 'scenario',
    values_to = 'reform'
  )
  

# get totals
opm_estimates = opm_microdata %>% 
  group_by(scenario) %>% 
  summarise(
    n_baseline    = sum(baseline * weight * weight_index),
    n_reform      = sum(reform * weight * weight_index),
    rate_baseline = n_baseline / sum(weight * weight_index), 
    rate_reform   = n_reform   / sum(weight * weight_index), 
    change        = n_reform - n_baseline, 
    pct_change    = n_reform / n_baseline -1
  )

opm_child_estimates = opm_microdata %>% 
  filter(age < 18) %>% 
  group_by(scenario) %>% 
  summarise(
    n_baseline    = sum(baseline * weight * weight_index),
    n_reform      = sum(reform * weight * weight_index),
    rate_baseline = n_baseline / sum(weight * weight_index), 
    rate_reform   = n_reform   / sum(weight * weight_index), 
    change        = n_reform - n_baseline, 
    pct_change    = n_reform / n_baseline -1
  )


#-------------------
# Do SPM calculation
#-------------------


if (!load_precalculated_tax_offset) {
  
  set.seed(76)
  
  # read microdata
  tax_microdata = read_csv('./resources/tax_simulator/microdata/baseline/2026.csv') %>% 
    filter(dep_status == 0) %>% 
    select(id, weight, filing_status, n_dep_ctc, agi, liab_iit_net) %>% 
    sample_frac(size = 0.25) %>%
    left_join(
      read_csv('./resources/tax_simulator/microdata/1pp/2026.csv') %>% 
        select(id, liab_iit_net_reform = liab_iit_net), 
      by = 'id'
    ) %>% 
    mutate(
      tax_change = liab_iit_net_reform - liab_iit_net, 
      married    = as.integer(filing_status == 2), 
      n_kids     = pmin(n_dep_ctc, 3)
    )

  # estimate model of tax changes given common observables
  tax_offset_model = quantregForest(
    x        = tax_microdata[c('liab_iit_net', 'married', 'n_kids')],
    y        = tax_microdata$tax_change, 
    nthreads = parallel::detectCores(),
    weights  = tax_microdata$weight,
    mtry     = 3,
    nodesize = 5
  )
  
}

# pull the Food-specific tariff shock for SNAP indexation 
spm_food_shock = crosswalk %>%
  filter(!is.na(spm_category)) %>%
  group_by(spm_category) %>%
  summarise(food_tariff = weighted.mean(tariff_price_shock, gtap_weight), .groups = "drop") %>%
  filter(spm_category == "Food") %>%
  select(food_tariff) %>%
  deframe()

# baseline SPM threshold factor
baseline_spm_factor = baseline_cpiu_factor

# build person-level rows carrying SPM-unit aggregates
spm_microdata = cps %>%
  mutate(
    
    # cash benefits observable in both OPM and SPM accounting
    oasdi = replace_na(na_if(INCSS,   999999),  0),
    ssi   = replace_na(na_if(INCSSI,  999999),  0),
    vet   = replace_na(na_if(INCVET,  9999999),  0), 
    agi   = replace_na(na_if(ADJGINC, 99999999), 0),
    
  ) %>%
  group_by(SPMFAMUNIT) %>%
  mutate(
    
    # tax-relevant attributes
    n_kids  = pmin(3, sum(AGE <= 16)),
    married = if_else(any(MARST %in% 1:2), "married", "unmarried"),
    
    # unit-level indexed cash components 
    spm_oasdi = sum(oasdi),
    spm_ssi   = sum(ssi),
    spm_vet   = sum(vet),
    
    # in-kind, taxes/credits, total resources, threshold
    spm_snap   = first(SPMSNAP),
    spm_tax    = first(SPMFEDTAXAC), 
    spm_totres = first(SPMTOTRES),
    spm_thresh = first(SPMTHRESH),
    
    # residual piece of SPM resources we can't observe directly 
    spm_resid = spm_totres - (spm_oasdi + spm_ssi + spm_vet + spm_snap - spm_tax)
  ) %>%
  ungroup() %>%
  
  # age to 2026 baseline (pre-tariff)
  mutate(
    
    spm_oasdi_2026  = spm_oasdi * oasdi_factor,
    spm_ssi_2026    = spm_ssi   * economic_factor,      
    spm_vet_2026    = spm_vet   * economic_factor,      
    spm_snap_2026   = spm_snap  * baseline_cpiu_factor, 
    spm_tax_2026    = spm_tax   * economic_factor,      
    spm_resid_2026  = spm_resid * economic_factor,
    
    spm_income_2026 = spm_resid_2026 + spm_oasdi_2026 + spm_ssi_2026 + spm_vet_2026 +
                      spm_snap_2026 - spm_tax_2026,
    
    spm_poverty_line = spm_thresh * baseline_spm_factor
  )

if (!load_precalculated_tax_offset) {
  
  # fit tax offsets 
  spm_microdata = spm_microdata %>%
    mutate(
      tax_change = predict(
        object  = tax_offset_model, 
        newdata = (.) %>% select(liab_iit_net = spm_tax_2026, n_kids, married),
        what    = function(x) mean(x)
      ), 
      
      # Scale to size of CPI-U shock (changes are estimated on 1pp shock)
      tax_change = tax_change * (cpiu_shock / 0.01)
    )
    
  # save to output
  spm_microdata %>% 
    select(tax_change) %>% 
    write_csv('./resources/tax_simulator/summary/tax_offset.csv')
  
} else {
  
  # read and append tax offsets
  spm_microdata = spm_microdata %>% 
    bind_cols(
      read_csv('./resources/tax_simulator/summary/tax_offset.csv')
    )
  
}
  
spm_microdata = spm_microdata %>% 
  mutate(

    # poverty status under baseline, gross (threshold only), and net (threshold + indexed benefits)
    baseline_spm = spm_income_2026 < spm_poverty_line,
    
    # gross: thresholds rise with FCSUti; resources don't respond
    gross_spm = spm_income_2026 < (spm_poverty_line * (1 + fcsuti_shock)),
    
    # net: thresholds rise with FCSUti; indexed resources respond to tariffs
    #  - OASDI/SSI/VET respond to overall CPI-U shock (COLA / CPI-linked parameters)
    #  - SNAP responds to the Food-specific shock
    #  - Taxes respond to indexation
    delta_income_2026 = (spm_oasdi_2026 + spm_ssi_2026 + spm_vet_2026) * cpiu_shock + 
                        spm_snap_2026                                  * spm_food_shock - 
                        tax_change,  

    net_spm  = (spm_income_2026 + delta_income_2026) < (spm_poverty_line * (1 + fcsuti_shock))
  ) %>%
  
  # reshape long in net vs gross
  select(
    weight = ASECWT, weight_index, age,
    baseline_spm, gross_spm, net_spm
  ) %>%
  pivot_longer(
    cols      = c(gross_spm, net_spm),
    names_to  = "scenario",
    values_to = "reform"
  )


spm_estimates = spm_microdata %>%
  group_by(scenario) %>%
  summarise(
    n_baseline    = sum(baseline_spm * weight * weight_index, na.rm = TRUE),
    n_reform      = sum(reform       * weight * weight_index, na.rm = TRUE),
    rate_baseline = n_baseline / sum(weight * weight_index),
    rate_reform   = n_reform   / sum(weight * weight_index),
    change        = n_reform - n_baseline,
    pct_change    = n_reform / n_baseline - 1,
    .groups = "drop"
  )

spm_child_estimates = spm_microdata %>%
  filter(age < 18) %>%
  group_by(scenario) %>%
  summarise(
    n_baseline    = sum(baseline_spm * weight * weight_index, na.rm = TRUE),
    n_reform      = sum(reform       * weight * weight_index, na.rm = TRUE),
    rate_baseline = n_baseline / sum(weight * weight_index),
    rate_reform   = n_reform   / sum(weight * weight_index),
    change        = n_reform - n_baseline,
    pct_change    = n_reform / n_baseline - 1,
    .groups = "drop"
  )

opm_estimates
opm_child_estimates
spm_estimates
spm_child_estimates

