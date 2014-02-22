#####################################################################################
### Estimate the Models (this script is designed to be run in the HP computers at UB)
####################################################################################

# Clear memory.
rm(list = ls())

# Load packages
library(R2jags)  # use R to interact with JAGS

# Set working directory
#setwd("~/Dropbox/Projects/Quadrants")
setwd("~/Quadrants")  # wd for rush

# Load data.
load("R_Images/bugs_data.RData")

# Set model parameters
K <- 3  # number of question-level REs
b0 <- rep(0, ncol(X))  # prior mean for beta
B0 <- .1*diag(ncol(X))  # prior precision for beta
g0.cons <- rep(0, ncol(Z.cons))
G0.cons <- .1*diag(ncol(Z.cons))
g0.edu <- rep(0, ncol(Z.edu))
G0.edu <- .1*diag(ncol(Z.edu))
g0.fem <- rep(0, ncol(Z.fem))
G0.fem <- .1*diag(ncol(Z.fem))
W <- diag(K) # parameter for Wishart prior
mu0 <- c(0,0,0)

# Create data object for JAGS call.
data <- list("n", "n.respondents", "n.questions", "y", "c.eduimp", "c.fem", "X", "Z.cons", "Z.edu", "Z.fem", "R", "Q", "W", "K", 
             "b0", "B0", "g0.cons", "G0.cons", "g0.edu", "G0.edu", "g0.fem", "G0.fem", "mu0") 


# Create function to provide initial values.
inits <- function () {
  list(beta.raw = rnorm(ncol(X), 0, 1),
       alpha = rnorm(n.respondents, 0, 1),
       sigma.alpha = runif(1, 1, 3),
       Tau = rWishart(1,K+1,diag(K))[,,1], 
       #gamma.cons.raw = rnorm(ncol(Z.cons), 0, 1), 
       #gamma.edu.raw = rnorm(ncol(Z.edu), 0, 1), 
       #gamma.fem.raw = rnorm(ncol(Z.fem), 0, 1), 
       mu = matrix(rnorm(3*n.questions, 0, 1), nrow = n.questions, ncol = 3),
       xi.beta.raw = runif(1, 0, 1),
       xi.gamma.cons.raw = runif(1, 0, 1),
       xi.gamma.edu.raw = runif(1, 0, 1),
       xi.gamma.fem.raw = runif(1, 0, 1)
       )
}

# Create vector of parameter names to track.
parameters <- c("sigma.alpha", "beta",
                "eta.cons", "eta.edu", "eta.fem", "sigma.eta", "mu",
                "gamma.cons", "gamma.edu", "gamma.fem", 
                "beta.raw", "gamma.cons.raw", "gamma.edu.raw", "gamma.fem.raw",
                "xi.beta.raw", "xi.gamma.cons.raw", "xi.gamma.edu.raw", "xi.gamma.fem.raw"#,
                ) 

# Estimate the JAGS model (and print the run time).
print(system.time(
  m <- jags.parallel(data, inits, parameters, model = "BUGS/model.bugs", n.chains = 3, n.iter = 1000, DIC = FALSE)
))

# Plot the estimates and convergence diagnostics.
#plot(m)
#print(parameters)

# Save the simulations.
save(m, file = "R_Images/mcmc_sims.RData")
