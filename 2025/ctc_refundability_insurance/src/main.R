#------------------------------------------------------------------------------
# main.R 
# 
# Calls all analysis scripts
#------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(magrittr)
library(usincometaxes)


#----------------
# Set parameters
#----------------

# Path to folder contains raw ASEC files
cps_root = '/gpfs/gibbs/project/sarin/shared/raw_data/CPS-ASEC/v1/2025031013/historical/'


#--------------
# Run analysis
#--------------

source('./src/clean_data.R')


# Questions for ernie/harris/myself
# - earnings definition?
# - tax unit ID
# - don't think we can use AGI since it's imputed unconditionally and thus will overstate variance

# TODO
# - add 2018/2019 via non-CSV data
# - fraction of parents reducing work hours/dropping out entirely when they have a kid 