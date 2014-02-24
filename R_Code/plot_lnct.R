
# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Quadrants")

# load packages
library(compactr)

# load data and simulations
load("R_Images/lmer_data.RData")
load("R_Images/model_sims.RData")

# read original data
d <- read.dta("Data/Smasterimp_1-22.dta")

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

# function to create X.pred for fixed coefficients
create.X.pred <- function() {
  X <- cbind(1,                                    
             f.eduimp,                                         
             f.policyspecific,                                 
             f.surveillance,                                   
             f.fem,                                            
             f.gendered,                                       
             f.lnct,                                           
             f.incimp,                                         
             f.ageimp,                                         
             f.blk,                                            
             f.dem,                                            
             f.rep,                                            
             f.oe,                                             
             f.randomizedanswerchoices,                        
             f.answerchoices,                                  
             f.dk.justtellme,                                  
             f.noct,                                           
             f.apolitical,                                     
             f.eduimp*f.policyspecific,                        
             f.eduimp*f.surveillance,                          
             f.policyspecific*f.surveillance,                  
             f.fem*f.gendered,                                 
             f.policyspecific*f.fem,                           
             f.policyspecific*f.gendered,                      
             f.surveillance*f.fem,                             
             f.surveillance*f.gendered,                        
             f.policyspecific*f.lnct,                          
             f.surveillance*f.lnct,                            
             f.eduimp*f.policyspecific*f.surveillance,         
             f.policyspecific*f.fem*f.gendered,                
             f.surveillance*f.fem*f.gendered,                  
             f.policyspecific*f.surveillance*f.fem,            
             f.policyspecific*f.surveillance*f.gendered,       
             f.policyspecific*f.surveillance*f.lnct,           
             f.policyspecific*f.surveillance*f.fem*f.gendered)
}


# function to create Z.pred for random coefficients
create.Z.pred <- function() {
  X <- cbind(1,                                    
             f.eduimp,                                                                           
             f.fem)
}

# pull out relevant parameters
gamma <- ranef(m)$Q
beta <- fixef(m)


# graphics parameters 
png("Manuscript/Hypothesis_Tests/Figures/lnct.png", height = 400, width = 600)
par(mfrow = c(2,2), mar = rep(0.75, 4), oma = c(3,3,1,1), family = "serif")

# vectors for computing substantive effects and axis notation
original.scale <- xticklab0 <- seq(0, 9, length.out = 10)
rescaled <- xat0 <- (xticklab0 - mean(d$lnct))/(2*sd(d$lnct))
lo <- rescaled[1]
hi <- rescaled[length(xat0)]

#################################################################################
## Static, General
#################################################################################

# set variables
name <- "Static-General Questions"
f.surveillance <- min(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.lnct <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = mm(f.lnct),
      ylim = c(0, 1),
      xlab = "log(Media Count)",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.lnct, p, col = "black", lwd = 3)
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
name <- "Surveillance-General Questions"
f.surveillance <- max(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.lnct <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = mm(f.lnct),
      ylim = c(0, 1),
      xlab = "log(Media Count)",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.lnct, p, col = "black", lwd = 3)
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
name <- "Static-Policy Questions"
f.surveillance <- min(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.lnct <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = mm(f.lnct),
      ylim = c(0, 1),
      xlab = "log(Media Count)",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.lnct, p, col = "black", lwd = 3)
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
name <- "Surveillance-Policy Questions"
f.surveillance <- max(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.lnct <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = mm(f.lnct),
      ylim = c(0, 1),
      xlab = "log(Media Count)",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.lnct, p, col = "black", lwd = 3)
p.hi <- p[xat0 == hi]
p.lo <- p[xat0 == lo]
fd <- round(p.hi - p.lo, 2)
or <- round((p.hi/(1 - p.hi))/(p.lo/(1 - p.lo)), 2)
rr <- round(p.hi/p.lo, 2)
text(xat0[1], .80, paste("FD = ", fd, "\nOR = ", or, "\nRR = ", rr, sep = ""), pos = 4)

dev.off()