
# clear memory
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Quadrants/")
#setwd("~/Quadrants/")

# parameters
random.subsample <- TRUE
n.sims <- 1000

# set seed
set.seed(8205024)

# load packages
library(foreign)
library(arm)
library(compactr)
library(knitr)

# setup data
source("R_Code/setup_data.R")

# estimate the model
source("R_Code/est_model.R")

# post-process the simulations
source("R_Code/pp_edu.R")
source("R_Code/pp_lnct.R")
source("R_Code/pp_fem.R")
source("R_Code/pp_fem_stat_gen.R")
source("R_Code/pp_fem_stat_ps.R")
source("R_Code/pp_fem_surv_gen.R")
source("R_Code/pp_fem_surv_ps.R")

# make the plots
source("R_Code/plot_edu.R")
source("R_Code/plot_lnct.R")
source("R_Code/plot_fem.R")
source("R_Code/do_gendered.R")

# robustness checks
source("R_Code/robustness_checks.R")

# create html summary of the results
setwd("~/Dropbox/Projects/Quadrants/Manuscript/Hypothesis_Tests/")
#setwd("~/Quadrants/Manuscript/Hypothesis_Tests/")
knit2html("ht.Rmd")
