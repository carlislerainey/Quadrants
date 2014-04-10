
# clear memory
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Quadrants/")
#setwd("~/Quadrants/")

# set seed
set.seed(58472570)

# load packages
library(foreign)
library(arm)
library(compactr)
library(knitr)
library(devEMF)

# Load data
#d <- read.dta("Data/Smasterimp_3-24.dta")
load("R_Images/lmer_data.RData")
load("R_Images/model_sims.RData")
load("R_Images/robustness_checks.RData")

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

# create html summary of the results
setwd("~/Dropbox/Projects/Quadrants/Manuscript/Hypothesis_Tests/")
#setwd("~/Quadrants/Manuscript/Hypothesis_Tests/")
knit2html("ht.Rmd")
