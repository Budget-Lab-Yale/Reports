#------------------------------------------------------------------------------
# process_data.R
#
# Reads and processes data inputs for the tariffs and poverty analysis.
# Returns a list of objects used in later stages of the project.
#------------------------------------------------------------------------------

process_data <- function() {
  #-----------
  # Read data
  #-----------
  # load IPUMS extract
  cps <- read_csv("resources/ipums_asec.csv.gz")

  # read macro projections file
  macro_projections <- bind_rows(
    read_csv("resources/macro_projections/historical.csv"),
    read_csv("resources/macro_projections/projections.csv")
  )

  # read tariff shock parameter files
  crosswalk <- read_csv("resources/tariff_inputs/crosswalk.csv")
  fcsuti_shares <- read_csv("resources/tariff_inputs/fcsuti_shares.csv")

  #-----------------------------
  # Calculate CPI-U price shock
  #-----------------------------
  # pull out trade impact
  trade_effect <- crosswalk %>%
    filter(gtap_category == 'Trade') %>%
    mutate(allocation = gtap_weight * tariff_price_shock) %>%
    select(allocation) %>%
    deframe()

  cpiu_shock <- crosswalk %>%
    mutate(
      trade_allocation_weight = (gtap_weight * trade_allocation) /
        sum(gtap_weight * trade_allocation, na.rm = TRUE),
      trade_price_shock = trade_effect * trade_allocation_weight,
      total_price_shock = tariff_price_shock + trade_price_shock
    ) %>%
    group_by(cpi_category) %>%
    summarise(
      weight = mean(cpi_weight) / 100,
      tariff_price_shock = weighted.mean(tariff_price_shock, gtap_weight),
      total_price_shock = weighted.mean(total_price_shock, gtap_weight)
    ) %>%
    filter(!is.na(cpi_category)) %>%
    summarise(
      weighted.mean(total_price_shock, weight)
    ) %>%
    deframe()

  #------------------------------
  # Calculate FCSUti price shock
  #------------------------------
  fcsuti_shares <- fcsuti_shares %>%
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

  fcsuti_shock <- crosswalk %>%
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
  demographic_factors <- macro_projections %>%
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
  economic_factor <- macro_projections %>%
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
  cps <- cps %>%
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
  oasdi_extensive_margin_factor <- cps %>%
    filter(INCSS > 0 & INCSS != 999999) %>%
    summarise(
      oasdi_n_growth = sum(ASECWT * weight_index) / sum(ASECWT)
    ) %>%
    deframe()

  # calculate intensive margin growth factor for OASDI
  oasdi_factor <- macro_projections %>%
    filter(year %in% c(2023, 2026)) %>%
    mutate(oasdi = outlays_mand_oasdi / lag(outlays_mand_oasdi, 1)) %>%
    filter(year == 2026) %>%
    select(oasdi) %>%
    deframe() %>%
    `/`(oasdi_extensive_margin_factor)

  # calculate baseline inflation factor
  baseline_cpiu_factor <-  macro_projections %>%
    filter(year %in% c(2023, 2026)) %>%
    mutate(cpiu = cpiu / lag(cpiu, 1)) %>%
    filter(year == 2026) %>%
    select(cpiu) %>%
    deframe()

  list(
    cps = cps,
    crosswalk = crosswalk,
    fcsuti_shock = fcsuti_shock,
    cpiu_shock = cpiu_shock,
    economic_factor = economic_factor,
    oasdi_factor = oasdi_factor,
    baseline_cpiu_factor = baseline_cpiu_factor
  )
}
