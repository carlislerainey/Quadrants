# 
# # clear workspace
# rm(list = ls())
# 
# # set working directory
# setwd("~/Dropbox/Projects/Quadrants")
# 
# # load packages
# library(compactr)
# library(foreign)
# library(arm)
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
#png("Manuscript/Hypothesis_Tests/Figures/fem.png", height = 400, width = 600)
emf("Manuscript/Hypothesis_Tests/Figures/fem.emf", height = 4, width = 6)

par(mfrow = c(2,2), mar = rep(0.75, 4), oma = c(3,3,1,1), family = "serif")

# vectors for computing substantive effects and axis notation
rescaled <- sort(unique(c.fem))
original.scale <- xticklab0 <- 0:1
rescaled <- xat0 <- (xticklab0 - mean(d$fem))
lo <- rescaled[original.scale == 0]
hi <- rescaled[original.scale == 1]
xticklab0 <- c("Male", "Female")

#################################################################################
## Static, General
#################################################################################

# set variables
name <- "Static-General Questions"
f.surveillance <- min(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.fem <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = rev(mm(f.fem)),
      ylim = c(0, 1),
      xlab = "Gender",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])) {
  y.star <- X.pred%*%beta + Z.pred%*%t(gamma[q, ])
  lines(f.fem, plogis(y.star), col = rgb(.7,.7,.7))
} 
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.fem, p, col = "black", lwd = 3)
fd <- round(quantile(pp.fem$fd.stat.gen, 0.5), 2)
lwr <- round(quantile(pp.fem$fd.stat.gen, 0.05), 2)
upr <- round(quantile(pp.fem$fd.stat.gen, 0.95), 2)
text(mean(xat0), .975, paste("FD = ", 
                             sprintf("%.2f", fd, 2), 
                             " [",
                             sprintf("%.2f", lwr, 2),
                             ", ",
                             sprintf("%.2f", upr, 2),
                             "]", sep = ""), 
     cex = .8)
#################################################################################
## Surveillance, General
#################################################################################

# set variables
name <- "Surveillance-General Questions"
f.surveillance <- max(c.surveillance)
f.policyspecific <- min(c.policyspecific)
f.fem <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = rev(mm(f.fem)),
      ylim = c(0, 1),
      xlab = "Gender",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])) {
  y.star <- X.pred%*%beta + Z.pred%*%t(gamma[q, ])
  lines(f.fem, plogis(y.star), col = rgb(.7,.7,.7))
} 
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.fem, p, col = "black", lwd = 3)
fd <- round(quantile(pp.fem$fd.surv.gen, 0.5), 2)
lwr <- round(quantile(pp.fem$fd.surv.gen, 0.05), 2)
upr <- round(quantile(pp.fem$fd.surv.gen, 0.95), 2)
text(mean(xat0), .975, paste("FD = ", 
                             sprintf("%.2f", fd, 2), 
                             " [",
                             sprintf("%.2f", lwr, 2),
                             ", ",
                             sprintf("%.2f", upr, 2),
                             "]", sep = ""), 
     cex = .8)
#################################################################################
## Static, Policy
#################################################################################

# set variables
name <- "Static-Policy Questions"
f.surveillance <- min(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.fem <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = rev(mm(f.fem)),
      ylim = c(0, 1),
      xlab = "Gender",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])) {
  y.star <- X.pred%*%beta + Z.pred%*%t(gamma[q, ])
  lines(f.fem, plogis(y.star), col = rgb(.7,.7,.7))
} 
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.fem, p, col = "black", lwd = 3)
fd <- round(quantile(pp.fem$fd.stat.ps, 0.5), 2)
lwr <- round(quantile(pp.fem$fd.stat.ps, 0.05), 2)
upr <- round(quantile(pp.fem$fd.stat.ps, 0.95), 2)
text(mean(xat0), .975, paste("FD = ", 
                             sprintf("%.2f", fd, 2), 
                             " [",
                             sprintf("%.2f", lwr, 2),
                             ", ",
                             sprintf("%.2f", upr, 2),
                             "]", sep = ""), 
     cex = .8)
#################################################################################
## Surveillance, Policy
#################################################################################

# set variables
name <- "Surveillance-Policy Questions"
f.surveillance <- max(c.surveillance)
f.policyspecific <- max(c.policyspecific)
f.fem <- xat0 #seq(min(c.eduimp), max(c.eduimp), length.out = 100)

X.pred <- create.X.pred()
Z.pred <- create.Z.pred()

## draw the plot
nq <- length(unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific]))
eplot(xlim = rev(mm(f.fem)),
      ylim = c(0, 1),
      xlab = "Gender",
      ylab = "Pr(Correct)",
      ylabpos = 2, 
      xat = xat0,
      xticklab = xticklab0,
      main = paste(name, " (N = ", nq, ")", sep = ""))
# lines for each question
for (q in unique(Q[c.surveillance == f.surveillance & c.policyspecific == f.policyspecific])) {
  y.star <- X.pred%*%beta + Z.pred%*%t(gamma[q, ])
  lines(f.fem, plogis(y.star), col = rgb(.7,.7,.7))
} 
# lines for the average
y.star <- X.pred%*%beta
p <- plogis(y.star)
lines(f.fem, p, col = "black", lwd = 3)
fd <- round(quantile(pp.fem$fd.surv.ps, 0.5), 2)
lwr <- round(quantile(pp.fem$fd.surv.ps, 0.05), 2)
upr <- round(quantile(pp.fem$fd.surv.ps, 0.95), 2)
text(mean(xat0), .975, paste("FD = ", 
                             sprintf("%.2f", fd, 2), 
                             " [",
                             sprintf("%.2f", lwr, 2),
                             ", ",
                             sprintf("%.2f", upr, 2),
                             "]", sep = ""), 
     cex = .8)
dev.off()
