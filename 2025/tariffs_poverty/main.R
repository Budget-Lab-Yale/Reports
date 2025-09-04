#------------------------------------------------------------------------------
# main.R 
# 
# Contains calculations for TBL's Sept. 2025 blog post on tariffs and poverty
#------------------------------------------------------------------------------

library(tidyverse)

# TODO trade shock business...

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
    trade_allocation_weight = gtap_weight / sum(gtap_weight * trade_allocation, na.rm = T),   
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


#--------------------
# Do SPM calculation
#--------------------

spm_microdata = cps %>%
  mutate(
    oasdi = replace_na(na_if(INCSS,   999999),  0),
    ssi   = replace_na(na_if(INCSSI,  999999),  0),
    vet   = replace_na(na_if(INCVET, 9999999),  0)
  ) %>%
  group_by(SPMFAMUNIT) %>%
  mutate(
    spm_oasdi          = sum(oasdi),
    spm_ssi            = sum(ssi),
    spm_vet            = sum(vet),
    spm_indexed        = spm_oasdi + spm_ssi + spm_vet,
    spm_other_resources = SPMTOTRES - spm_indexed,

    # age forward
    spm_other_resources_2026 = spm_other_resources * economic_factor,
    spm_vet_2026             = spm_vet            * economic_factor,
    spm_ssi_2026             = spm_ssi            * economic_factor,
    spm_oasdi_2026           = spm_oasdi          * oasdi_factor,

    spm_indexed_2026 = spm_oasdi_2026 + spm_ssi_2026 + spm_vet_2026,
    spm_resources_2026 = spm_other_resources_2026 + spm_indexed_2026,

    poverty_line = SPMTHRESH * baseline_cpiu_factor
  ) %>%
  ungroup() %>%

  # do poverty status calculations for each scenario
  mutate(
    baseline = spm_resources_2026                                   < poverty_line,
    gross    = spm_resources_2026                                   < poverty_line * (1 + fcsuti_shock),
    net      = (spm_resources_2026 + (spm_indexed_2026 * cpiu_shock)) < poverty_line * (1 + fcsuti_shock)
  ) %>%
  select(weight = SPMWT, weight_index, age, baseline, gross, net) %>%
  pivot_longer(
    cols      = c(gross, net),
    names_to  = 'scenario',
    values_to = 'reform'
  )


# get totals
spm_estimates = spm_microdata %>%
  group_by(scenario) %>%
  summarise(
    n_baseline    = sum(baseline * weight * weight_index),
    n_reform      = sum(reform * weight * weight_index),
    rate_baseline = n_baseline / sum(weight * weight_index),
    rate_reform   = n_reform   / sum(weight * weight_index),
    change        = n_reform - n_baseline,
    pct_change    = n_reform / n_baseline -1
  )

spm_child_estimates = spm_microdata %>%
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
