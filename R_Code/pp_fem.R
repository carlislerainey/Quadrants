
# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Quadrants")

# load packages
library(arm)
library(foreign)

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

# set values of variable of interest
f.fem <- (c(0, 1) - mean(d$fem))/(2*sd(d$fem))

#################################################################################
## Static, General
#################################################################################

# set variables
f.surveillance <- min(c.surveillance)
f.policyspecific <- min(c.policyspecific)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

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

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

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

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

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

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
fd.surv.ps <- p.hi - p.lo
or.surv.ps <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.surv.ps <- p.hi/p.lo


#################################################################################
## save the Simulations
#################################################################################

pp.fem <- list(fd.stat.gen = fd.stat.gen, rr.stat.gen = rr.stat.gen, or.stat.gen = or.stat.gen,
                fd.surv.gen = fd.surv.gen, rr.surv.gen = rr.surv.gen, or.surv.gen = or.surv.gen,
                fd.stat.ps = fd.stat.ps, rr.stat.ps = rr.stat.ps, or.stat.ps = or.stat.ps,
                fd.surv.ps = fd.surv.ps, rr.surv.ps = rr.surv.ps, or.surv.ps = or.surv.ps)

save(pp.fem, file = "R_Images/pp_fem.RData")