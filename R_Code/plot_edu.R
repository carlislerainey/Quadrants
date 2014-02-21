
# clear workspace
rm(list = ls())

# load data and simulations
load("R_Images/bugs_data.RData")
load("R_Images/mcmc_sims.RData")


# load packages
library(compactr)

# choose values for the individual-level variables
f.fem <- median(c.fem)
f.eduimp <- median(c.eduimp)
f.incimp <- median(c.incimp)
f.ageimp <- median(c.ageimp)
f.blk <- median(c.blk)
f.dem <- median(c.dem)
f.rep <- median(c.rep)
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)

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

# graphics parameters 
par(mfrow = c(2,2), mar = rep(0.75, 4), oma = c(3,3,1,1), family = "serif")

# vectors for computing substantive effects and axis notation
rescaled <- sort(unique(c.eduimp))
original.scale <- sort(unique(respondent.data$eduimp))
xticklab0 <- 1:7
xat0 <- (xticklab0 - mean(respondent.data$eduimp))/(2*sd(respondent.data$eduimp))
lo <- rescaled[original.scale == 3]
hi <- rescaled[original.scale == 6]

#################################################################################
## Static, General
#################################################################################

# set variables
name <- "Static General Questions"
f.surveillance <- min(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.eduimp <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)


# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)

## draw the plot
nq <- length(question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])
eplot(xlim = mm(f.eduimp),
      ylim = c(0, 1),
      xlab = "Education",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]) {
  b.cons <- Z.cons.pred%*%gamma.cons + mu.cons[q]
  b.edu <- Z.edu.pred%*%gamma.edu + mu.edu[q]
  b.fem <- Z.fem.pred%*%gamma.fem + mu.fem[q]
  y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
  lines(f.eduimp, plogis(y.star), col = rgb(.7,.7,.7, .5))
} 
# lines for the average
b.cons <- Z.cons.pred%*%gamma.cons
b.edu <- Z.edu.pred%*%gamma.edu
b.fem <- Z.fem.pred%*%gamma.fem
y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
p <- plogis(y.star)
lines(f.eduimp, p, col = "black", lwd = 3)
p.hi <- p[xat0 == hi]
p.lo <- p[xat0 == lo]
fd <- round(p.hi - p.lo, 2)
or <- round((p.hi/(1 - p.hi))/(p.lo/(1 - p.lo)), 2)
rr <- round(p.hi/p.lo, 2)
text(xat0[1], .80, paste("FD = ", fd, "\nOR = ", or, "\nRR = ", rr, sep = ""), pos = 4)

#################################################################################
## Surveillance, General
#################################################################################

# set variables
name <- "Surveillance General Questions"
f.surveillance <- max(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.eduimp <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)


# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)

## draw the plot
nq <- length(question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])
eplot(xlim = mm(f.eduimp),
      ylim = c(0, 1),
      xlab = "Education",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]) {
  b.cons <- Z.cons.pred%*%gamma.cons + mu.cons[q]
  b.edu <- Z.edu.pred%*%gamma.edu + mu.edu[q]
  b.fem <- Z.fem.pred%*%gamma.fem + mu.fem[q]
  y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
  lines(f.eduimp, plogis(y.star), col = rgb(.7,.7,.7, .5))
} 
# lines for the average
b.cons <- Z.cons.pred%*%gamma.cons
b.edu <- Z.edu.pred%*%gamma.edu
b.fem <- Z.fem.pred%*%gamma.fem
y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
p <- plogis(y.star)
lines(f.eduimp, p, col = "black", lwd = 3)
p.hi <- p[xat0 == hi]
p.lo <- p[xat0 == lo]
fd <- round(p.hi - p.lo, 2)
or <- round((p.hi/(1 - p.hi))/(p.lo/(1 - p.lo)), 2)
rr <- round(p.hi/p.lo, 2)
text(xat0[1], .80, paste("FD = ", fd, "\nOR = ", or, "\nRR = ", rr, sep = ""), pos = 4)

#################################################################################
## Static, Policy
#################################################################################

# set variables
name <- "Static Policy Questions"
f.surveillance <- min(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.eduimp <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)


# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)

## draw the plot
nq <- length(question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])
eplot(xlim = mm(f.eduimp),
      ylim = c(0, 1),
      xlab = "Education",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]) {
  b.cons <- Z.cons.pred%*%gamma.cons + mu.cons[q]
  b.edu <- Z.edu.pred%*%gamma.edu + mu.edu[q]
  b.fem <- Z.fem.pred%*%gamma.fem + mu.fem[q]
  y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
  lines(f.eduimp, plogis(y.star), col = rgb(.7,.7,.7, .5))
} 
# lines for the average
b.cons <- Z.cons.pred%*%gamma.cons
b.edu <- Z.edu.pred%*%gamma.edu
b.fem <- Z.fem.pred%*%gamma.fem
y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
p <- plogis(y.star)
lines(f.eduimp, p, col = "black", lwd = 3)
p.hi <- p[xat0 == hi]
p.lo <- p[xat0 == lo]
fd <- round(p.hi - p.lo, 2)
or <- round((p.hi/(1 - p.hi))/(p.lo/(1 - p.lo)), 2)
rr <- round(p.hi/p.lo, 2)
text(xat0[1], .80, paste("FD = ", fd, "\nOR = ", or, "\nRR = ", rr, sep = ""), pos = 4)

#################################################################################
## Surveillance, Policy
#################################################################################

# set variables
name <- "Surveillance Policy Questions"
f.surveillance <- max(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.eduimp <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)


# create prediction matrices
X.pred <- cbind(f.incimp, f.ageimp, f.blk, f.dem, f.rep)
Z.cons.pred <- cbind(1, f.lnct, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                     f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance,
                     f.lnct*f.policyspecific, f.lnct*f.surveillance, f.lnct*f.policyspecific*f.surveillance,
                     f.oe, f.randomizedanswerchoices, f.answerchoices, f.dk.justtellme, f.noct, f.apolitical)
Z.edu.pred <- cbind(1, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance)
Z.fem.pred <- cbind(1, f.gendered, f.policyspecific, f.surveillance, f.policyspecific*f.surveillance,
                    f.gendered*f.policyspecific, f.gendered*f.surveillance, f.gendered*f.policyspecific*f.surveillance)

## draw the plot
nq <- length(question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])
eplot(xlim = mm(f.eduimp),
      ylim = c(0, 1),
      xlab = "Education",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in question.data$Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]) {
  b.cons <- Z.cons.pred%*%gamma.cons + mu.cons[q]
  b.edu <- Z.edu.pred%*%gamma.edu + mu.edu[q]
  b.fem <- Z.fem.pred%*%gamma.fem + mu.fem[q]
  y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
  lines(f.eduimp, plogis(y.star), col = rgb(.7,.7,.7, .5))
} 
# lines for the average
b.cons <- Z.cons.pred%*%gamma.cons
b.edu <- Z.edu.pred%*%gamma.edu
b.fem <- Z.fem.pred%*%gamma.fem
y.star <- b.cons + b.edu*f.eduimp + b.fem*f.fem + X.pred%*%beta
p <- plogis(y.star)
lines(f.eduimp, p, col = "black", lwd = 3)
p.hi <- p[xat0 == hi]
p.lo <- p[xat0 == lo]
fd <- round(p.hi - p.lo, 2)
or <- round((p.hi/(1 - p.hi))/(p.lo/(1 - p.lo)), 2)
rr <- round(p.hi/p.lo, 2)
text(xat0[1], .80, paste("FD = ", fd, "\nOR = ", or, "\nRR = ", rr, sep = ""), pos = 4)