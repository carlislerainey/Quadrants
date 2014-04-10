## Before runnning, check two things:
## 1.) subsample is set to appropriate values
## 2.) working directory is correct


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
#source("R_Code/est_model.R")

# robustness checks
source("R_Code/robustness_checks.R")

