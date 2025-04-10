#------------------------------------------------------------------------------
# calc_metrics.R 
# 
# Calculates summary stats about parents moving in and out of CTC phase-in 
# range across consecutive years
#------------------------------------------------------------------------------



#---------------------------------------------------------------------
# Determintion of whether parents had enough income to claim full CTC
#---------------------------------------------------------------------

# Limit to nondependent individuals with CTC-eligible children and those without wage imputations
cps_ctc = cps %>%
  filter(
    year.2 < 2024, 
    dep.1 == 0, dep.2 == 0, 
    (tu_ctc_dep.1 > 0 | tu_ctc_dep.2 > 0), 
    wages_imputed == 0
  ) %>% 
  
  # Calculate 2021 CTC as if 2020 law applied
  mutate(
    year_recoded = year.1 == 2021 | year.2 == 2021, 
    year.1 = if_else(year.1 == 2021, 2020, year.1), 
    year.2 = if_else(year.2 == 2021, 2020, year.2), 
  )


# Determine earnings status for year 1
earnings_status.1 = cps_ctc %>%
  
  # Create two copies -- one for actual dependents in 1, one with dependents from 2 (for the case where 
  # the taxpayer had no dependents in year 1 but at least one in year 1)
  expand_grid(scenario = 1:2) %>%
  mutate(
    depx = if_else(scenario == 1, tu_ctc_dep.1, tu_ctc_dep.2), 
    taxsimid = row_number()
  ) %>% 
  
  # Send to TaxSim for calculation
  select(scenario, taxsimid, id, tax_unit_id.1, year = year.1, pwages = tu_earnings.1, mstat = mstat.1, depx) %>% 
  left_join(
    taxsim_calculate_taxes(., return_all_information = T), 
    by = 'taxsimid'
  ) %>%
  
  # Determine whether the credit was full or partial
  mutate(
    status.1 = if_else(
      ((v22_child_tax_credit_adjusted + v23_child_tax_credit_refundable) < (depx * 2000)) & (pwages < 100000), 
      'partial', 
      'full'
    ),
    status.1 = if_else(depx == 0, NA, status.1)
  ) %>% 
  
  # Use year-2 dependent amount for those with no dependents in year 1
  select(scenario, id, year.1 = year, tax_unit_id.1, status.1) %>% 
  pivot_wider(
    names_from  = scenario,
    values_from = status.1
  ) %>% 
  mutate(status.1 = if_else(is.na(`1`), `2`, `1`)) %>% 
  select(-`1`, -`2`)


# Repeat above but in reverse for year 2
earnings_status.2 = cps_ctc %>%
  expand_grid(scenario = 1:2) %>%
  mutate(
    depx = if_else(scenario == 1, tu_ctc_dep.1, tu_ctc_dep.2), 
    taxsimid = row_number()
  ) %>% 
  select(scenario, taxsimid, id, tax_unit_id.2, year = year.2, pwages = tu_earnings.2, mstat = mstat.2, depx) %>% 
  left_join(
    taxsim_calculate_taxes(., return_all_information = T), 
    by = 'taxsimid'
  ) %>%
  mutate(
    status.2 = if_else(
      ((v22_child_tax_credit_adjusted + v23_child_tax_credit_refundable) < (depx * 2000)) & (pwages < 100000), 
      'partial', 
      'full'
    ),
    status.2 = if_else(depx == 0, NA, status.2),
  ) %>% 
  select(scenario, id, year.2 = year, tax_unit_id.2, status.2) %>% 
  pivot_wider(
    names_from  = scenario,
    values_from = status.2
  ) %>% 
  mutate(status.2 = if_else(is.na(`2`), `1`, `2`)) %>% 
  select(-`1`, -`2`)
  
  
# Join earnings status back on CPS data
cps_ctc %<>% 
  left_join(earnings_status.1, by = c('id', 'year.1', 'tax_unit_id.1')) %>% 
  left_join(earnings_status.2, by = c('id', 'year.2', 'tax_unit_id.2'))


#---------------------
# Summary calculations
#---------------------

# Overall point-in time split, adult-level
cps_ctc %>% 
  group_by(year.1, name = status.1) %>% 
  summarise(
    value = sum(weight.1), 
    .groups = 'drop'
  ) %>% 
  pivot_wider() %>% 
  mutate(share_partial = partial / (partial + full))

# Overall point-in time split, tax unit-level
cps_ctc %>% 
  group_by(year.1, tax_unit_id.1) %>% 
  summarise(status.1 = status.1[1], weight.1 = mean(weight.1), .groups = 'drop') %>% 
  group_by(year.1, name = status.1) %>% 
  summarise(
    value = sum(weight.1), 
    .groups = 'drop'
  ) %>% 
  pivot_wider() %>% 
  mutate(share_partial = partial / (partial + full))

# Share partial in any two years
cps_ctc %>% 
  summarise(
    partial_first_year  = weighted.mean(status.1 == 'partial'),
    partial_either_year = weighted.mean(status.1 == 'partial' | status.2 == 'partial', weight.1), 
    .groups = 'drop'
  ) 

# Forward-looking transition probabilities
cps_ctc %>%
  group_by(status.1, name = paste0(status.2, '.2')) %>% 
  summarise(
    value = sum(weight.1), 
    .groups = 'drop'
  ) %>% 
  pivot_wider() %>% 
  mutate(share_full.2 = full.2 / (partial.2 + full.2))

# ...excluding 2021
cps_ctc %>%
  filter(!year_recoded) %>% 
  group_by(status.1, name = paste0(status.2, '.2')) %>% 
  summarise(
    value = sum(weight.1), 
    .groups = 'drop'
  ) %>% 
  pivot_wider() %>% 
  mutate(share_full.2 = full.2 / (partial.2 + full.2))

# Backward-looking transitions for parents of newborns
cps_ctc %>% 
  filter(had_baby.2 == 1, year.1 != 2021) %>% 
  group_by(status.2, name = paste0(status.1, '.1')) %>% 
  summarise(
    value = sum(weight.2), 
    .groups = 'drop'
  ) %>% 
  pivot_wider() %>% 
  mutate(share_full.1 = full.1 / (partial.1 + full.1))

# Forward-looking for parents of newborns
cps_ctc %>% 
  filter(had_baby.2 == 1, year.2 != 2021) %>% 
  group_by(status.1, name = paste0(status.2, '.2')) %>% 
  summarise(
    value = sum(weight.2), 
    .groups = 'drop'
  ) %>% 
  pivot_wider() %>% 
  mutate(share_full.2 = full.2 / (partial.2 + full.2))



