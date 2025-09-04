#------------------------------------------------------------------------------
# do_OPM.R
#
# Calculates Official Poverty Measure (OPM) estimates.
# Expects a list of processed inputs produced by process_data().
#------------------------------------------------------------------------------

do_OPM <- function(data) {
  cps <- data$cps
  cpiu_shock <- data$cpiu_shock
  economic_factor <- data$economic_factor
  oasdi_factor <- data$oasdi_factor
  baseline_cpiu_factor <- data$baseline_cpiu_factor

  opm_microdata <- cps %>%
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
      baseline = opm_income_2026 < poverty_line,
      gross    = opm_income_2026 < poverty_line * (1 + cpiu_shock),
      net      = (opm_income_2026 + (opm_indexed_2026 * cpiu_shock)) <
                 (poverty_line * (1 + cpiu_shock))
    ) %>%
    select(weight = ASECWT, weight_index, age, baseline, gross, net) %>%
    pivot_longer(
      cols = c(gross, net),
      names_to = 'scenario',
      values_to = 'reform'
    )

  # get totals
  opm_estimates <- opm_microdata %>%
    group_by(scenario) %>%
    summarise(
      n_baseline    = sum(baseline * weight * weight_index),
      n_reform      = sum(reform   * weight * weight_index),
      rate_baseline = n_baseline / sum(weight * weight_index),
      rate_reform   = n_reform   / sum(weight * weight_index),
      change        = n_reform - n_baseline,
      pct_change    = n_reform / n_baseline - 1
    )

  opm_child_estimates <- opm_microdata %>%
    filter(age < 18) %>%
    group_by(scenario) %>%
    summarise(
      n_baseline    = sum(baseline * weight * weight_index),
      n_reform      = sum(reform   * weight * weight_index),
      rate_baseline = n_baseline / sum(weight * weight_index),
      rate_reform   = n_reform   / sum(weight * weight_index),
      change        = n_reform - n_baseline,
      pct_change    = n_reform / n_baseline - 1
    )

  list(
    opm_estimates = opm_estimates,
    opm_child_estimates = opm_child_estimates
  )
}
