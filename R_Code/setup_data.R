## set up data for lmer

################################################################################
### Load and setup the data for the JAGS model
################################################################################

# Clear memory.
# rm(list = ls())

### Set working directory. Simply direct R to the folder where the analysis
### files are held. It is important that the file have the same structure as the
### original replication folder. Simply change the directory below to where
### you've saved (or unzipped) the "Quadrants" folder.
#setwd("~/Dropbox/Projects/Quadrants") # wd for Carlisle's machine
#setwd("~/Quadrants")  # wd for rush

# Open necessary packages
library(foreign)  # work with Stata data sets
library(arm)  # a variety of useful functions

# Open data set. This command looks in the Data folder of the working directory 
# to find the main data file, which we call Smasterimp.
d <- read.dta("Data/Smasterimp_3-24.dta")
# uncomment the followin line to create a subsampled data set
if (random.subsample == TRUE) {
  d <- d[d$rid %in% sample(unique(d$rid), 123), ]  
}

# create and index for the questions
d$Q <- as.numeric(as.factor(d$QID))
Q.table <- cbind(1:length(unique(d$QID)), unique(d$QID))
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
c.fem <- rescale(d$fem)
c.eduimp <- rescale(d$eduimp)
c.incimp <- rescale(d$incimp)
c.ageimp <- rescale(d$ageimp)
c.blk <- rescale(d$blk)
c.dem <- rescale(d$dem)
c.rep <- rescale(d$rep)
c.lnct <- rescale(d$lnct)
c.policyspecific <- rescale(d$policyspecific)
c.surveillance <- rescale(d$surveillance)
c.surveillance.alt <- rescale(d$dayssince2 <= 42) # alternative cutoff for surveillance variable
c.oe <- rescale(d$oe)
c.randomizedanswerchoices <- rescale(d$randomizedanswerchoices)
c.answerchoices <- rescale(d$answerchoices)
c.noct <- rescale(d$noct)
c.dk.justtellme <- rescale(d$dk_justtellme)
c.apolitical <- rescale(d$apolitical)
c.gendered <- rescale(d$gendered)
y <- d$knowcor

# save the objects
save(y, Q, R,   # save these listed objects
     list = ls(patter = "c\\."),  # and all the objects that begin with "c."
     file = "R_Images/lmer_data.RData")
