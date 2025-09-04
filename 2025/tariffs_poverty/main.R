#------------------------------------------------------------------------------
# main.R
#
# Orchestrates calculations for TBL's Sept. 2025 blog post on tariffs and poverty.
#------------------------------------------------------------------------------

library(tidyverse)
library(Hmisc)
library(quantregForest)

# Switch for loading precalculated tax offset (non-TBL users must set to TRUE)
load_precalculated_tax_offset <- TRUE

# Source modular scripts
source("process_data.R")
source("do_OPM.R")
source("do_SPM.R")

# Process inputs
inputs <- process_data()

# Run analyses
opm_results <- do_OPM(inputs)
spm_results <- do_SPM(inputs, load_precalculated_tax_offset)

# Output
opm_results$opm_estimates
opm_results$opm_child_estimates
spm_results$spm_estimates
spm_results$spm_child_estimates
