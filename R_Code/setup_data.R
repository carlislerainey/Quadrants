
################################################################################
### Load and setup the data for the JAGS model
################################################################################

# Clear memory.
rm(list = ls())

### Set working directory. Simply direct R to the folder where the analysis
### files are held. It is important that the file have the same structure as the
### original replication folder. Simply change the directory below to where
### you've saved (or unzipped) the "Quadrants" folder.
setwd("~/Dropbox/Projects/Quadrants") # wd for Carlisle's machine
#setwd("~/Quadrants")  # wd for rush

# Open necessary packages
library(foreign)  # work with Stata data sets
library(arm)  # a variety of useful functions

# Open data set. This command looks in the Data folder of the working directory 
# to find the main data file, which we call Smasterimp.
d <- read.dta("Data/Smasterimp_1-22.dta")
# uncomment the followin line to create a subsampled data set
#d <- d[d$rid %in% sample(unique(d$rid), 10), ]  

# bring counts into the working environment
n <- nrow(d)
n.questions <- length(unique(d$Q))
n.respondents <- length(unique(d$rid))

# create and index for the questions
d$Q <- as.numeric(as.factor(d$Q))
Q.table <- cbind(1:length(unique(d$Q)), unique(d$Q))
Q <- d$Q
for (i in 1:nrow(Q.table)) {
  Q[d$Q == Q.table[i, 2]] <- Q.table[i, 1]
}
# create an index and matching table for the questions
S.table <- cbind(1:length(unique(d$S)), unique(d$S))
S <- d$S
for (i in 1:nrow(S.table)) {
  S[d$S == S.table[i, 2]] <- S.table[i, 1]
}
# create an index and matching table for the respondents
R.table <- cbind(1:length(unique(d$rid)), unique(d$rid))
R <- d$rid
pb <- txtProgressBar(min = 0, max = nrow(R.table), style = 3)
for (i in 1:nrow(R.table)) {
  R[d$rid == R.table[i, 2]] <- R.table[i, 1]
  setTxtProgressBar(pb, i)
}
d$R <- R

# create the individual-level variables
respondent.data <- aggregate(cbind(fem, eduimp, incimp, ageimp, blk, dem, rep) ~ R, data = d, FUN = mean)
c.fem <- rescale(respondent.data$fem)
c.eduimp <- rescale(respondent.data$eduimp)
c.incimp <- rescale(respondent.data$incimp)
c.ageimp <- rescale(respondent.data$ageimp)
c.blk <- rescale(respondent.data$blk)
c.dem <- rescale(respondent.data$dem)
c.rep <- rescale(respondent.data$rep)
X <- cbind(c.incimp, c.ageimp, c.blk, c.dem, c.rep)

# create the question-level variables
question.data <- aggregate(cbind(lnct, policyspecific, surveillance, oe, randomizedanswerchoices, answerchoices,
                                 noct, dk_justtellme, apolitical, gendered) ~ Q, data = d, FUN = mean)
c.lnct <- rescale(question.data$lnct)
c.policyspecific <- rescale(question.data$policyspecific)
c.surveillance <- rescale(question.data$surveillance)
c.oe <- rescale(question.data$oe)
c.randomizedanswerchoices <- rescale(question.data$randomizedanswerchoices)
c.answerchoices <- rescale(question.data$answerchoices)
c.noct <- rescale(question.data$noct)
c.dk.justtellme <- rescale(question.data$dk_justtellme)
c.apolitical <- rescale(question.data$apolitical)
c.gendered <- rescale(question.data$gendered)
# covariates used to model the intercept
Z.cons <- cbind(1, c.lnct, c.gendered, c.policyspecific, c.surveillance, c.policyspecific*c.surveillance,
           c.gendered*c.policyspecific, c.gendered*c.surveillance, c.gendered*c.policyspecific*c.surveillance,
           c.lnct*c.policyspecific, c.lnct*c.surveillance, c.lnct*c.policyspecific*c.surveillance,
           c.oe, c.randomizedanswerchoices, c.answerchoices, c.dk.justtellme, c.noct, c.apolitical)
# covariates used to model the coefficient for education
Z.edu <- cbind(1, c.policyspecific, c.surveillance, c.policyspecific*c.surveillance)
# covariates used to model the coefficient for gender
Z.fem <- cbind(1, c.gendered, c.policyspecific, c.surveillance, c.policyspecific*c.surveillance,
                c.gendered*c.policyspecific, c.gendered*c.surveillance, c.gendered*c.policyspecific*c.surveillance)

# create the outcome variables
y <- d$knowcor

# save the objects
save(y, X, Z.cons, Z.edu, Z.fem, n, n.respondents, n.questions, R, Q, respondent.data, question.data,  # save these listed objects
     list = ls(patter = "c\\."),  # and all the objects that begin with "c."
     file = "R_Images/bugs_data.RData")