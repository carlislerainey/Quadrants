# Pull out coefs and ses for main model
#est <- fixef(m)
#se <- se.fixef(m)

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
# #est.ac <- fixef(m.alt.cutoff)
# #se.ac <- se.fixef(m.alt.cutoff)
# 
# # Alternative Cutoff for Surveillance
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
# #est.da <- c(fixef(m.drop.apolitical)[1:17], NA, fixef(m.drop.apolitical)[18:35])
# #se.da <- c(se.fixef(m.drop.apolitical)[1:17], NA, se.fixef(m.drop.apolitical)[18:35])

m.years <- bglmer(y ~ c.eduimp*c.policyspecific*c.surveillance + 
              c.fem*c.gendered*c.policyspecific*c.surveillance + 
              c.lnct*c.policyspecific*c.surveillance + 
              (1 + c.eduimp + c.fem | Q) + (1 | R) + (1 | year) + 
              c.incimp + c.ageimp + c.blk + c.dem + c.rep +
              c.oe + c.randomizedanswerchoices + c.answerchoices + 
              c.dk.justtellme + c.noct + c.apolitical,
            family = "binomial", nAGQ = 0, verbose = 10)
display(m.years)

save(m.alt.cutoff, m.drop.apolitical, m.years, file = "R_Images/robustness_checks.RData")


# n.par <- length(est)
# drop <- .2
# 
# pdf("Manuscript/Figures/robustness_checks.pdf", height = 10, width = 8)
# par(mar = c(2, 17, 1, 1))
# eplot(xlim = c(-3, 3), ylim = c(0, n.par + 1),
#       yat = 1:n.par, yticklab = names(est))
# abline(v = 0)
# for (i in 1:n.par) {
#   points(est[i], i, pch = 19)
#   lines(c(est[i] - 1.64*se[i], est[i] + 1.64*se[i]), c(i, i))
#   points(est.ac[i], i - drop, pch = 19, col = "red")
#   lines(c(est.ac[i] - 1.64*se.ac[i], est.ac[i] + 1.64*se.ac[i]), 
#         c(i - drop, i - drop), col = "red")
#   points(est.da[i], i - 2*drop, pch = 19, col = "blue")
#   lines(c(est.da[i] - 1.64*se.da[i], est.da[i] + 1.64*se.da[i]), 
#         c(i - 2*drop, i - 2*drop), col = "blue")
#   
# }
# dev.off()
