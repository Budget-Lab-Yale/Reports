#------------------------------------------------------------------------------
# clean_data.R 
# 
# Reads and processes ASEC panel
#------------------------------------------------------------------------------


#-----------
# Read data
#-----------

# Read ASEC panel from IPUMS
cps = read_csv('./resources/cps.csv.gz') %>% 
  filter(YEAR_1 >= 2019)


# Read tax unit IDs from raw ASEC
tax_unit_ids = seq(2019, 2024) %>% 
  map(
    .f = ~ cps_root %>% 
      file.path(paste0('pppub', .x - 2000, '.csv')) %>% 
      fread(select = c('PERIDNUM','TAX_ID')) %>% 
      tibble() %>% 
      mutate(year = .x, .before = everything())
  ) %>% 
  bind_rows()


#----------------------
# Recode NIU variables
#----------------------

# Create NIU map
niu_codes = list(
  'INCWAGE' = 99999999,
  'INCBUS'  = 99999999,
  'INCFARM' = 99999999,
  'CTCCRD'  = 999999,
  'ACTCCRD' = 99999,
  'ADJGINC' = 99999999
)

# Recode IPUMS not-in-universe observation codings to 0 for dollar-amount vars 
for (i in 1:length(niu_codes)) {
  cps %<>% 
    mutate(
      across(
        .cols = all_of(paste0(names(niu_codes[i]), c('_1', '_2'))), 
        .fns  = ~ if_else(. == niu_codes[i], 0, .)
      )
    )
} 



#------------
# Clean data
#------------

cps %<>% 
  
  # Join tax unit IDs for each year
  left_join(
    tax_unit_ids %>% 
      select(YEAR_1 = year, UH_PERIDNUM_A1_1 = PERIDNUM, tax_unit_id.1 = TAX_ID), 
    by = c('YEAR_1', 'UH_PERIDNUM_A1_1')
  ) %>% 
  left_join(
    tax_unit_ids %>% 
      select(YEAR_2 = year, UH_PERIDNUM_A1_2 = PERIDNUM, tax_unit_id.2 = TAX_ID), 
    by = c('YEAR_2', 'UH_PERIDNUM_A1_2')
  ) %>% 
  
  # Create data imputation flag
  mutate(
    wages_imputed = as.integer(
      QINCLONG_1...39  != 0 | 
      QINCLONG_2...40  != 0 | 
      QOINCWAGE_1...43 != 0 | 
      QOINCWAGE_2...44 != 0
    )
  ) %>% 
  
  # Create indicators for whether person had a new child and marital status
  mutate(
    had_baby.1 = as.integer(YNGCH_1 == 0),
    had_baby.2 = as.integer(YNGCH_2 == 0),
    mstat.1    = as.integer(FILESTAT_1 %in% c(1, 2, 3)) + 1,
    mstat.2    = as.integer(FILESTAT_2 %in% c(1, 2, 3)) + 1,
  ) %>% 

  # Calculate tax-unit-level dependents and earnings
  group_by(tax_unit_id.1) %>% 
  mutate(
    
    # Number of CTC-eligible children
    tu_ctc_dep.1  = sum(DEPSTAT_1 != 0 & AGE_1 < 17),
    
    # Income
    earnings.1    = INCWAGE_1 + INCBUS_1 + INCFARM_1,
    tu_earnings.1 = sum(earnings.1[!DEPSTAT_1]), 
    tu_agi.1      = sum(ADJGINC_1), 
    
    # CTC
    tu_ctc.1 = sum(CTCCRD_1 + ACTCCRD_1)
    
  ) %>% 
  
  group_by(tax_unit_id.2) %>% 
  mutate(
    
    # Number of CTC-eligible children
    tu_ctc_dep.2  = sum(DEPSTAT_2 != 0 & AGE_2 < 17),
    
    # Income
    earnings.2    = INCWAGE_2 + INCBUS_2 + INCFARM_2,
    tu_earnings.2 = sum(earnings.2[!DEPSTAT_2]), 
    tu_agi.2      = sum(ADJGINC_2), 
    
    # CTC
    tu_ctc.2 = sum(CTCCRD_2 + ACTCCRD_2)
    
  ) %>% 
  
  # Clean up variable names
  select(
    id = CPSIDP,
    year.1 = YEAR_1,      year.2  = YEAR_2, 
    tax_unit_id.1,        tax_unit_id.2,
    weight.1 = ASECWT_1,  weight.2 = ASECWT_2,
    age.1 = AGE_1,        age.2 = AGE_2, 
    dep.1 = DEPSTAT_1,    dep.2 = DEPSTAT_2,
    mstat.1,              mstat.2, 
    n_child.1 = NCHILD_1, n_child.2 = NCHILD_2,
    had_baby.1,           had_baby.2,
    starts_with('tu_'), 
    wages_imputed
  ) %>% 
  ungroup()

