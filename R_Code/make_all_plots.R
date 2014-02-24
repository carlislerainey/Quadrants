
# Clear workspace.
rm(list = ls())

# Set working directory.
setwd("~/Dropbox/Projects/Quadrants")

# Make all plots.
source("R_Code/plot_lnct.R")
source("R_Code/plot_edu.R")
source("R_Code/plot_fem.R")
source("R_Code/plot_fem_stat_gen.R")
source("R_Code/plot_fem_stat_ps.R")
source("R_Code/plot_fem_surv_gen.R")
source("R_Code/plot_fem_surv_ps.R")