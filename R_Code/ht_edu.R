
# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Quadrants")

# load packages
library(compactr)

# load data and simulations
load("R_Images/bugs_data.RData")
load("R_Images/mcmc_sims.RData")

# choose values for the individual-level variables
f.fem <- median(c.fem)
f.eduimp <- median(c.eduimp)
f.incimp <- median(c.incimp)
f.ageimp <- median(c.ageimp)
f.blk <- median(c.blk)
f.dem <- median(c.dem)
f.rep <- median(c.rep)

# choose values for the question-level covariates
f.lnct <- median(c.lnct)
f.policyspecific <- median(c.policyspecific)
f.surveillance <- median(c.surveillance)
f.oe <- median(c.oe)
f.randomizedanswerchoices <- median(c.randomizedanswerchoices)
f.answerchoices <- median(c.answerchoices)
f.noct <- median(c.noct)
f.dk.justtellme <- median(c.dk.justtellme)
f.apolitical <- median(c.apolitical)
f.gendered <- median(c.gendered)

# store simulations
sims <- m$BUGSoutput$sims.matrix
sims.beta <- sims[, paste("beta[", 1:ncol(X), "]", sep = "")]
sims.gamma.cons <- sims[, paste("gamma.cons[", 1:ncol(Z.cons), "]", sep = "")]
sims.gamma.edu <- sims[, paste("gamma.edu[", 1:ncol(Z.edu), "]", sep = "")]
sims.gamma.fem <- sims[, paste("gamma.fem[", 1:ncol(Z.fem), "]", sep = "")]
sims.mu.cons <- sims[, paste("mu[", 1:n.questions, ",1]", sep = "")]
sims.mu.edu <- sims[, paste("mu[", 1:n.questions, ",2]", sep = "")]
sims.mu.fem <- sims[, paste("mu[", 1:n.questions, ",3]", sep = "")]

# pull out estimates
beta <- apply(sims.beta, 2, median)
gamma.cons <- apply(sims.gamma.cons, 2, median)
gamma.edu <- apply(sims.gamma.edu, 2, median)
gamma.fem <- apply(sims.gamma.fem, 2, median)
mu.cons <- apply(sims.mu.cons, 2, median)
mu.edu <- apply(sims.mu.edu, 2, median)
mu.fem <- apply(sims.mu.fem, 2, median)

#################################################################################
## Static, General
#################################################################################

# set variables
f.surveillance <- min(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.eduimp <- (c(3, 6) - mean(respondent.data$eduimp))/(2*sd(respondent.data$eduimp))

# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)


# compute the simulated probabilities
b.cons <- Z.cons.pred%*%t(sims.gamma.cons)
b.edu <- Z.edu.pred%*%t(sims.gamma.edu)
b.fem <- Z.fem.pred%*%t(sims.gamma.fem)
y.star.lo <- b.cons + b.edu*f.eduimp[1] + b.fem*f.fem + X.pred%*%t(sims.beta)
y.star.hi <- b.cons + b.edu*f.eduimp[2] + b.fem*f.fem + X.pred%*%t(sims.beta)
p.lo <- plogis(y.star.lo)
p.hi <- plogis(y.star.hi)

# compute the qis
fd.stat.gen <- p.hi - p.lo
or.stat.gen <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.stat.gen <- p.hi/p.lo

#################################################################################
## Surveillance, General
#################################################################################

# set variables
f.surveillance <- max(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.eduimp <- (c(3, 6) - mean(respondent.data$eduimp))/(2*sd(respondent.data$eduimp))

# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)


# compute the simulated probabilities
b.cons <- Z.cons.pred%*%t(sims.gamma.cons)
b.edu <- Z.edu.pred%*%t(sims.gamma.edu)
b.fem <- Z.fem.pred%*%t(sims.gamma.fem)
y.star.lo <- b.cons + b.edu*f.eduimp[1] + b.fem*f.fem + X.pred%*%t(sims.beta)
y.star.hi <- b.cons + b.edu*f.eduimp[2] + b.fem*f.fem + X.pred%*%t(sims.beta)
p.lo <- plogis(y.star.lo)
p.hi <- plogis(y.star.hi)

# compute the qis
fd.surv.gen <- p.hi - p.lo
or.surv.gen <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.surv.gen <- p.hi/p.lo

#################################################################################
## Static, Policy Specific
#################################################################################

# set variables
f.surveillance <- min(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.eduimp <- (c(3, 6) - mean(respondent.data$eduimp))/(2*sd(respondent.data$eduimp))

# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)


# compute the simulated probabilities
b.cons <- Z.cons.pred%*%t(sims.gamma.cons)
b.edu <- Z.edu.pred%*%t(sims.gamma.edu)
b.fem <- Z.fem.pred%*%t(sims.gamma.fem)
y.star.lo <- b.cons + b.edu*f.eduimp[1] + b.fem*f.fem + X.pred%*%t(sims.beta)
y.star.hi <- b.cons + b.edu*f.eduimp[2] + b.fem*f.fem + X.pred%*%t(sims.beta)
p.lo <- plogis(y.star.lo)
p.hi <- plogis(y.star.hi)

# compute the qis
fd.stat.ps <- p.hi - p.lo
or.stat.ps <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.stat.ps <- p.hi/p.lo

#################################################################################
## Surveillance, Policy Specific
#################################################################################

# set variables
f.surveillance <- max(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.eduimp <- (c(3, 6) - mean(respondent.data$eduimp))/(2*sd(respondent.data$eduimp))

# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)


# compute the simulated probabilities
b.cons <- Z.cons.pred%*%t(sims.gamma.cons)
b.edu <- Z.edu.pred%*%t(sims.gamma.edu)
b.fem <- Z.fem.pred%*%t(sims.gamma.fem)
y.star.lo <- b.cons + b.edu*f.eduimp[1] + b.fem*f.fem + X.pred%*%t(sims.beta)
y.star.hi <- b.cons + b.edu*f.eduimp[2] + b.fem*f.fem + X.pred%*%t(sims.beta)
p.lo <- plogis(y.star.lo)
p.hi <- plogis(y.star.hi)

# compute the qis
fd.surv.ps <- p.hi - p.lo
or.surv.ps <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.surv.ps <- p.hi/p.lo


#################################################################################
## save the Simulations
#################################################################################

ht.edu <- list(fd.stat.gen = fd.stat.gen, rr.stat.gen = rr.stat.gen, rr.stat.gen = rr.stat.gen,
               fd.surv.gen = fd.surv.gen, rr.surv.gen = rr.surv.gen, rr.surv.gen = rr.surv.gen,
               fd.stat.ps = fd.stat.ps, rr.stat.ps = rr.stat.ps, rr.stat.ps = rr.stat.ps,
               fd.surv.ps = fd.surv.ps, rr.surv.ps = rr.surv.ps, rr.surv.ps = rr.surv.ps)

save(ht.edu, file = "R_Images/ht_edu.RData")
