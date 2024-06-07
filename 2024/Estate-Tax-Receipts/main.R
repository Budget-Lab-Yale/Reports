#----------------------------------------------------------------------
# Replication code for "Pandemic-Era Trends in Estate Tax Collections"
# 
# Author: John Ricco
#----------------------------------------------------------------------

library(tidyverse)
library(lubridate)


#---------------
# Read raw data
#---------------

# Monthly estate and gift tax collections, via the Monthly Treasury Statement, 
# downloaded from Haver 
mts = read_csv('./resources/haver-estate_mts.csv')

# CBO estate and gift projections. Pre-TCJA projections are from the June 2017 
# Outlook update; the TCJA effect size is from the legislative changes component of 
# the baseline update in the April 2018 Outlook
cbo = read_csv('./resources/cbo-estate_vintages.csv') 

# Excess mortality from Our World in Data
# https://ourworldindata.org/excess-mortality-covid
excess_deaths = read_csv('./resources/excess-mortality-raw-death-count-single-series.csv')


#------------------
# Process raw data
#------------------

# Generate FY estate and gift tax receipts
estate_fy = mts %>% 
  group_by(fy = if_else(month < 10, year, year + 1)) %>% 
  summarise(receipts = sum(receipts) / 1e3)

# Estimate annual rates of excess mortality in the US, adjusted to reflect 
# timing of estate tax payments
excess_deaths_ratio = excess_deaths %>% 
  filter(Code == 'USA') %>% 
  mutate(
    
    # Parse CY and month
    year  = as.integer(str_sub(Day, start = -4)),
    month = as.integer(str_replace(str_sub(Day, end = 2), '/', '')), 
    
    # Determine filing FY, assuming 9 months to file from death
    filing_month = if_else(month + 9 > 12, (month + 9) %% 12, month + 9), 
    filing_year  = if_else(filing_month >= 10, year, year + 1), 
    filing_fy    = if_else(filing_month >= 10, filing_year + 1, filing_year)
  ) %>% 
  
  # Get excess deaths ratio  
  group_by(filing_fy) %>% 
  summarise(
    projected = sum(projected_deaths_since_2020_all_ages), 
    actual    = sum(deaths_since_2020_all_ages)
  ) %>% 
  mutate(excess_deaths_ratio = actual / projected)

  
#------------------------------------------
# Do forecast error decomposition exercise
#------------------------------------------

# Start with actual estate tax receipts and filter to relevant period
decomp = estate_fy %>% 
  rename(level.actual = receipts) %>% 
  filter(fy >= 2018, fy < 2024) %>% 
  
  # Join CBO projections
  left_join(cbo %>% 
              rename(level.pre_tcja = pre_tcja, 
                     effect.tcja    = tcja_leg), 
            by = 'fy') %>% 
  
  # Join excess death assumptions (assumed to be 0 for pre-2020)
  left_join(excess_deaths_ratio %>% 
              select(fy = filing_fy, excess_deaths_ratio), 
            by = 'fy') %>%
  mutate(excess_deaths_ratio = replace_na(excess_deaths_ratio, 1)) %>% 
  
  # Do decomposition 
  mutate(
    
    # 1) Calculate post-TCJA projected level of receipts (excludes economic
    #    and technical updates)
    level.post_tcja = level.pre_tcja + effect.tcja, 
    
    # 2) Add excess deaths. Assumes that excess mortality risk is uniformly
    #    distributed across the wealth distribution, meaning this is an upper
    #    bound on the effect of excess mortality 
    effect.excess_deaths = level.post_tcja * (excess_deaths_ratio - 1), 
    level.excess_deaths  = level.post_tcja + effect.excess_deaths,
    
    # 3) Calculate asset price effect as residual
    effect.other = level.actual - level.excess_deaths, 
    
    # 4) Calculate no-TCJA + actual economics/demographics counterfactual
    level.no_tcja  = level.actual / level.post_tcja * level.pre_tcja, 
    effect.no_tcja = level.no_tcja - level.actual 
  ) %>% 
  select(-excess_deaths_ratio) 



#----------
# Figure 1 
#----------

# Relative effect of "other" category
decomp %>% 
  mutate(other_pct = level.actual / level.excess_deaths - 1) %>% 
  select(fy, other_pct)

# Create data file
data_F1 = decomp %>% 
  mutate(Total = level.actual - level.pre_tcja) %>% 
  select(
    `Fiscal year`                               = fy, 
    Total, 
    `1) TCJA cuts`                              = effect.tcja, 
    `2) Mortality risk`                         = effect.excess_deaths, 
    `3) Growth in top wealth and other effects` = `effect.other`, 
  ) %>% 
  write_csv('./chart_data/figure_1.csv')


# Create static image
data_F1 %>% 
  select(-Total) %>% 
  pivot_longer(cols = -`Fiscal year`) %>% 
  ggplot() + 
  geom_col(aes(x = `Fiscal year`, y = value, fill = name)) + 
  geom_point(
    data = data_F1 %>% 
      select(`Fiscal year`, Total), 
    mapping = aes(x = `Fiscal year`, y = Total, shape = 'Difference between pre-TCJA projection and actual'), 
    size = 5
  ) + 
  geom_hline(yintercept = 0) + 
  theme_bw() +
  theme(legend.position = 'top', legend.box = 'vertical') +
  labs(y = 'Estate and gift tax receipts (billions of dollars)', fill = '', shape = element_blank()) +
  scale_x_continuous(breaks = 2018:2023) + 
  ggtitle('Contribution to Difference between Pre-TCJA Projection and Actual Receipts') 


#----------
# Figure 2
#----------

# Create data file
data_F2 = mts %>%
  group_by(year) %>% 
  mutate(cumulative_receipts = cumsum(receipts) / 1e3) %>% 
  filter(year >= 2017) %>% 
  mutate(Period = case_when(
    year %in% 2017:2018 ~ 'Pre-TCJA average', 
    year %in% 2019:2020 ~ 'Post-TCJA, pre-pandemic average', 
    year %in% 2021:2022 ~ '2021-2022 average',
    T                   ~ as.character(year)
  )) %>% 
  group_by(Period, Month = month) %>% 
  summarise(`Cumulative receipts` = mean(cumulative_receipts), 
            .groups = 'drop') %>% 
  write_csv('./chart_data/figure_2.csv')


# Create static image  
data_F2 %>% 
  ggplot(aes(x = Month, y = `Cumulative receipts`, colour = Period)) + 
  geom_line() + 
  geom_point() + 
  theme_bw() +
  labs(x = 'Month', y = 'Calendar year receipts (billions of dollars)', colour = 'Period') +
  scale_x_continuous(breaks = 1:12) + 
  scale_colour_manual(values = c('#57cc99', '#0480d4', '#8fc1e3',  '#e63946', '#343a40')) +
  ggtitle('Year-to-Date Receipts by Month')

  

#----------
# Figure 3 
#----------

# Create chart data
data_F3 = estate_fy %>% 
  filter(fy > 2012, fy < 2024) %>% 
  rename(Actual = receipts) %>% 
  left_join(
    decomp %>% 
      mutate(level.no_tcja = if_else(fy < 2019, NA, level.no_tcja)) %>% 
      select(fy,
             `Pre-TCJA projection`    = level.pre_tcja, 
             `No-TCJA counterfactual` = `level.no_tcja`), 
    by = 'fy'
  ) %>%
  mutate(`Pre-TCJA projection` = if_else(fy == 2017, Actual, `Pre-TCJA projection`)) %>% 
  rename(`Fiscal year` = fy) %>% 
  write_csv('./chart_data/figure_3.csv')

# Create static chart
data_F3 %>% 
  pivot_longer(cols = -`Fiscal year`) %>% 
  ggplot(aes(x = `Fiscal year`, y = value, colour = name)) + 
  geom_point(size = 3) +
  geom_line() + 
  theme_bw() +
  labs(x = 'Fiscal year', y = 'Receipts (billions of dollars)', colour = 'Scenario') +
  scale_x_continuous(breaks = 2013:2023) + 
  scale_colour_manual(values = c('#343a40', '#57cc99', '#e63946')) +
  ggtitle('Estimated Estate and Gift Tax Receipts under a No-TCJA Counterfactual')

  
