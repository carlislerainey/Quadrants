# Pull out coefs and ses for main model
#est <- fixef(m)
#se <- se.fixef(m)

load("R_Images/lmer_data.RData")
library(arm)
library(blme)



# Alternative Cutoff for Surveillance
m.drop.apolitical <- bglmer(y ~ c.eduimp*c.policyspecific*c.surveillance + 
                              c.fem*c.gendered*c.policyspecific*c.surveillance + 
                              c.lnct*c.policyspecific*c.surveillance + 
                              (1 + c.eduimp + c.fem | Q) + (1 | R) +
                              c.incimp + c.ageimp + c.blk + c.dem + c.rep +
                              c.oe + c.randomizedanswerchoices + c.answerchoices + 
                              c.dk.justtellme + c.noct,
                            family = "binomial", nAGQ = 0, verbose = 10,
                            subset = c.apolitical == min(c.apolitical))
display(m.drop.apolitical)

save(m.drop.apolitical, file = "R_Images/m_drop_apolitical.RData")