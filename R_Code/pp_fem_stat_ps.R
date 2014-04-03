# 
# # clear workspace
# rm(list = ls())
# 
# # set working directory
# setwd("~/Dropbox/Projects/Quadrants")
# 
# # load packages
# library(arm)
# library(foreign)
# 
# # load data and simulations
# load("R_Images/lmer_data.RData")
# load("R_Images/model_sims.RData")
# 
# # read original data
# d <- read.dta("Data/Smasterimp_1-22.dta")

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
f.surveillance <- min(c.surveillance)
f.policyspecific <- max(c.policyspecific)

#################################################################################
## Non-gendered
#################################################################################

# set variables
f.gendered <- min(c.gendered)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
p.hi.ngend <- p.hi
p.lo.ngend <- p.lo
fd.ngend <- p.hi - p.lo
or.ngend <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.ngend <- p.hi/p.lo

#################################################################################
## Gendered
#################################################################################

# set variables
f.gendered <- max(c.gendered)

# create prediction matrix
X.pred <- create.X.pred()

# compute the simulated probabilities
y.star <- X.pred%*%t(fe.sims)
p.lo <- plogis(y.star[1, ] )
p.hi <- plogis(y.star[2,])

# compute the qis
p.hi.gend <- p.hi
p.lo.gend <- p.lo
fd.gend <- p.hi - p.lo
or.gend <- (p.hi/(1 - p.hi))/(p.lo/(1 - p.lo))
rr.gend <- p.hi/p.lo


#################################################################################
## save the Simulations
#################################################################################

pp.fem.stat.ps <- list(p.hi.ngend = p.hi.ngend, p.lo.ngend = p.lo.ngend, fd.ngend = fd.ngend, rr.ngend = rr.ngend, or.ngend = or.ngend,
                        p.hi.gend = p.hi.gend, p.lo.gend = p.lo.gend, fd.gend = fd.gend, rr.gend = rr.gend, or.gend = or.gend)
save(pp.fem.stat.ps, file = "R_Images/pp_fem_stat_ps.RData")