#------------------------------------------------------------------------------
# Code for July 2025 post: Who Would Benefit from Eliminating Capital Gains 
# Taxes on Home Sales? 
# 
# John Ricco
#------------------------------------------------------------------------------

library(tidyverse)

# 2022 SCF summary extract path
your_scf_path = ''

# Read data 
scf = read_csv(your_scf_path)

scf %>% 
  
  # Assign limit based on marital status 
  mutate(
    exemption     = if_else(MARRIED == 1, 500000, 250000),
    above_limit   = KGHOUSE > exemption, 
    taxable_gains = pmax(0, KGHOUSE - exemption)
  ) %>% 
  
  # Calculate summary stats
  group_by(
    homeowner = HOUSES > 0, 
    above_limit
  ) %>% 
  summarise(
    n                 = sum(WGT), 
    avg_age           = weighted.mean(AGE, WGT), 
    avg_income        = weighted.mean(INCOME, WGT), 
    avg_wealth        = weighted.mean(NETWORTH, WGT),
    avg_home          = weighted.mean(HOUSES, WGT), 
    avg_home_equity   = weighted.mean(HOUSES - MRTHEL, WGT),
    avg_gains         = weighted.mean(KGHOUSE, WGT),
    avg_taxable_gains = weighted.mean(taxable_gains, WGT),
    avg_tax_savings   = avg_taxable_gains * 0.238,             
    .groups = 'drop'
  ) %>%
  group_by(homeowner) %>%
  mutate(
    share = n / sum(n), .after = above_limit,
  )


