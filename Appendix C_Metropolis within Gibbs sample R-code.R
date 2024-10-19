######################################
## Metropolis within Gibbs amples   ##
######################################

# Number of samples
nmc = 1000
# Number of parameters ; d prime and criterion
n.pars = 2
# Create a vector to hold the samples
samples = array(dim = c(n.pars, nmc), dimnames = list(c("d'", "C"), NULL))

# Initial guess
samples[, 1] = c(1, 0.5)

# Samples!
for(i in 2:nmc){
  samples[, i] = samples[, i-1]
  for (j in rownames(samples)) {
    proposal = samples[, i]
    proposal[j] = proposal[j] + rnorm(n = 1, mean = 0, sd = 0.1)
    new.likelihood = posterior.density(parameters = proposal,
                                       h = N.hits,
                                       fa = N.falsealarms,
                                       Np = N.present,
                                       Na = N.absent)
    old.likelihood = posterior.density(parameters = samples[, i],
                                       h = N.hits,
                                       fa = N.falsealarms,
                                       Np = N.present,
                                       Na = N.absent)
    likelihood.ratio = new.likelihood / old.likelihood
    if (runif(1) < likelihood.ratio) {
      samples[, i] = proposal
    }
  }
}