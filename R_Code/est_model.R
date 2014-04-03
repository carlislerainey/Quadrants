# Clear memory.
#rm(list = ls())

# Load packages
library(blme) 
library(arm)

# Set working directory
#setwd("~/Dropbox/Projects/Quadrants")  # wd for C's machines
#setwd("~/Quadrants")  # wd for rush

# Load data.
load("R_Images/lmer_data.RData")

# Estimate model.
m <- bglmer(y ~ c.eduimp*c.policyspecific*c.surveillance + 
              c.fem*c.gendered*c.policyspecific*c.surveillance + 
              c.lnct*c.policyspecific*c.surveillance + 
              (1 + c.eduimp + c.fem | Q) + (1 | R) +
              c.incimp + c.ageimp + c.blk + c.dem + c.rep +
              c.oe + c.randomizedanswerchoices + c.answerchoices + 
              c.dk.justtellme + c.noct + c.apolitical,
            family = "binomial", nAGQ = 0, verbose = 10)

# Display the estimates.
display(m)

# Simulate from the model.
sims <- sim(m, n = n.sims)

# Save the simulations of the fixed effects.
fe.sims <- fixef(sims)

# Save the simulations of the question-level random effects.
re.sims <- ranef(sims)$Q

# Save the model and the simulations as an image.
save(m, fe.sims, re.sims, file = "R_Images/model_sims.RData")