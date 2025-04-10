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
source('./src/calc_metrics.R')
