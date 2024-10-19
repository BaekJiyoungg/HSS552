######################################
## SDT R-code.R                     ##
######################################

N.present = 100 # Number of trials on which a signal was present
N.absent = 100 # Number of trials on which no signal was present.
N.hits = 85 # Correct responses to "present" trials.
N.falsealarms = 12 # Incorrect responses to "absent" trials.

posterior.density = function(parameters, h, fa, Np, Na){
  # Function to calculate posterior density. "parameters"
  # is a 2-vector, with elements "d'" for d-prime and "C"
  # for criterion. "h" and "fa" are counts of hits and false
  # alarms. "Np" are "Na" are the number of trials with 
  # target and no-target
  
  # The model-predicted probability of a false alarm.
  prob.fa = pnorm(-parameters["C"])
  # The model-predicted probability of a hit
  prob.h = pnorm(parameters["d'"]-parameters["C"])
  
  # The log-likelihood of observing "fa" false alarms.
  loglike.fa = dbinom(x = fa, size = Na, prob = prob.fa, log = TRUE)
  # The log-likelihood of observing "h" hits.
  loglike.h = dbinom(x=h, size = Np, prob = prob.h, log = TRUE)
  
  # The prior log-likelihood of the parameters under a
  # very simple prior of N(0, 4) for both parameters.
  loglike.prior = dnorm(parameters, mean = 0, sd = 4, log = TRUE)
  
  # Return the posterior density : exp(sum).
  exp(loglike.fa + loglike.h + sum(loglike.prior))
  
}

# Number of samples
nmc = 500

# Create a vector to hold the samples.
samples = array(dim = c(2, nmc), dimnames = list(c("C", "d;"), NULL))

# Initial guess
samples[,1] = c(0.5, 1.0)

# Sample!
for (i in 2:nmc){
  proposal = samples[, i-1] + rnorm(n = 2, mean = 0, sd = 0.1)
  new.likelihood = posterior.density(parameters = proposal, 
                                     h = N.hits,
                                     fa = N.falsealarms,
                                     Np = N.present,
                                     Na = N.absent)
  old.likelihood = posterior.density(parameters = samples[, i-1],
                                     h = N.hits,
                                     fa = N.falsealarms,
                                     Np = N.present,
                                     Na = N.absent)
  likelihood.ratio = new.likelihood / old.likelihood
  if (runif(1) < likelihood.ratio){
    samples[, i] = proposal
  }else{
    samples[, i] = samples[, i-1]
  }
}