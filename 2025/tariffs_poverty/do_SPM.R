#------------------------------------------------------------------------------
# do_SPM.R
#
# Calculates Supplemental Poverty Measure (SPM) estimates.
# Expects a list of processed inputs produced by process_data().
#------------------------------------------------------------------------------

do_SPM <- function(data, load_precalculated_tax_offset = TRUE) {
  cps <- data$cps
  crosswalk <- data$crosswalk
  fcsuti_shock <- data$fcsuti_shock
  cpiu_shock <- data$cpiu_shock
  oasdi_factor <- data$oasdi_factor
  economic_factor <- data$economic_factor
  baseline_cpiu_factor <- data$baseline_cpiu_factor

  # baseline SPM threshold factor
  baseline_spm_factor <- baseline_cpiu_factor

  if (!load_precalculated_tax_offset) {
    set.seed(76)

    # read microdata
    tax_microdata <- read_csv('./resources/tax_simulator/microdata/baseline/2026.csv') %>%
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
    tax_offset_model <- quantregForest(
      x        = tax_microdata[c('liab_iit_net', 'married', 'n_kids')],
      y        = tax_microdata$tax_change,
      nthreads = parallel::detectCores(),
      weights  = tax_microdata$weight,
      mtry     = 3,
      nodesize = 5
    )
  }

  # pull the Food-specific tariff shock for SNAP indexation
  spm_food_shock <- crosswalk %>%
    filter(!is.na(spm_category)) %>%
    group_by(spm_category) %>%
    summarise(food_tariff = weighted.mean(tariff_price_shock, gtap_weight),
              .groups = 'drop') %>%
    filter(spm_category == 'Food') %>%
    select(food_tariff) %>%
    deframe()

  # build person-level rows carrying SPM-unit aggregates
  spm_microdata <- cps %>%
    mutate(
      # cash benefits observable in both OPM and SPM accounting
      oasdi = replace_na(na_if(INCSS,   999999),  0),
      ssi   = replace_na(na_if(INCSSI,  999999),  0),
      vet   = replace_na(na_if(INCVET,  9999999),  0),
      agi   = replace_na(na_if(ADJGINC, 99999999), 0)
    ) %>%
    group_by(SPMFAMUNIT) %>%
    mutate(
      # tax-relevant attributes
      n_kids  = pmin(3, sum(AGE <= 16)),
      married = if_else(any(MARST %in% 1:2), 'married', 'unmarried'),

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
    spm_microdata <- spm_microdata %>%
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
    spm_microdata <- spm_microdata %>%
      bind_cols(
        read_csv('./resources/tax_simulator/summary/tax_offset.csv')
      )
  }

  spm_microdata <- spm_microdata %>%
    # poverty status under baseline, gross (threshold only), and net (threshold + indexed benefits)
    mutate(
      baseline_spm = spm_income_2026 < spm_poverty_line,
      # gross: thresholds rise with FCSUti; resources don't respond
      gross_spm = spm_income_2026 < (spm_poverty_line * (1 + fcsuti_shock)),
      # net: thresholds rise with FCSUti; indexed resources respond to tariffs
      #  - OASDI/SSI/VET respond to overall CPI-U shock (COLA / CPI-linked parameters)
      #  - SNAP responds to the Food-specific shock
      #  - Taxes respond to indexation
      delta_income_2026 = (spm_oasdi_2026 + spm_ssi_2026 + spm_vet_2026) * cpiu_shock +
                          spm_snap_2026 * spm_food_shock -
                          tax_change,
      net_spm = (spm_income_2026 + delta_income_2026) <
                (spm_poverty_line * (1 + fcsuti_shock))
    ) %>%
    # reshape long in net vs gross
    select(weight = ASECWT, weight_index, age,
           baseline_spm, gross_spm, net_spm) %>%
    pivot_longer(
      cols = c(gross_spm, net_spm),
      names_to = 'scenario',
      values_to = 'reform'
    )

  spm_estimates <- spm_microdata %>%
    group_by(scenario) %>%
    summarise(
      n_baseline    = sum(baseline_spm * weight * weight_index, na.rm = TRUE),
      n_reform      = sum(reform       * weight * weight_index, na.rm = TRUE),
      rate_baseline = n_baseline / sum(weight * weight_index),
      rate_reform   = n_reform   / sum(weight * weight_index),
      change        = n_reform - n_baseline,
      pct_change    = n_reform / n_baseline - 1,
      .groups = 'drop'
    )

  spm_child_estimates <- spm_microdata %>%
    filter(age < 18) %>%
    group_by(scenario) %>%
    summarise(
      n_baseline    = sum(baseline_spm * weight * weight_index, na.rm = TRUE),
      n_reform      = sum(reform       * weight * weight_index, na.rm = TRUE),
      rate_baseline = n_baseline / sum(weight * weight_index),
      rate_reform   = n_reform   / sum(weight * weight_index),
      change        = n_reform - n_baseline,
      pct_change    = n_reform / n_baseline - 1,
      .groups = 'drop'
    )

  list(
    spm_estimates = spm_estimates,
    spm_child_estimates = spm_child_estimates
  )
}
