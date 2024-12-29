# setup.R
#
# This script sets up the R environment for the medical data analysis project.
# It handles package management, library paths, and dependencies required for
# synthetic data generation and survival analysis.
#
# Key components:
# - Local package library configuration
# - Installation of required packages
# - Loading of necessary libraries
# - Integration with Python via reticulate
#
# Dependencies: R (>= 4.0.0), Python (>= 3.7)
# Author: Thierry Chanet
# Last updated: 2024-12-29


### Install Packages
# Create a directory for R packages in your home directory if it doesn't exist
local_lib <- "~/Documents/code/R/library"

# Set local library path
.libPaths(c(local_lib, .libPaths()))

# Check and install only missing packages to local library
required_packages <- c("survival", "survminer", "brms", "dplyr", "ggplot2", "reticulate")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, lib = local_lib)
}
# Load all required packages from local library
for (pkg in required_packages) {
  library(pkg, character.only = TRUE)
}