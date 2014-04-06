# Pull out coefs and ses for main model
#est <- fixef(m)
#se <- se.fixef(m)

load("R_Images/lmer_data.RData")
library(arm)
library(blme)

# Alternative Cutoff for Surveillance
m.alt.cutoff <- bglmer(y ~ c.eduimp*c.policyspecific*c.surveillance.alt + 
                         c.fem*c.gendered*c.policyspecific*c.surveillance.alt + 
                         c.lnct*c.policyspecific*c.surveillance.alt + 
                         (1 + c.eduimp + c.fem | Q) + (1 | R) +
                         c.incimp + c.ageimp + c.blk + c.dem + c.rep +
                         c.oe + c.randomizedanswerchoices + c.answerchoices + 
                         c.dk.justtellme + c.noct + c.apolitical,
                       family = "binomial", nAGQ = 0, verbose = 10)
display(m.alt.cutoff)
#est.ac <- fixef(m.alt.cutoff)
#se.ac <- se.fixef(m.alt.cutoff)


save(m.alt.cutoff, file = "R_Images/m_alt_cutoff.RData")