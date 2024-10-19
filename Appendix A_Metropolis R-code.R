######################################
## Metropolis R-code.R                     ##
######################################
samples = numeric(500) # 500 samples
samples[1] = 110 # the initial guess
for (i in 2:500)
{
  proposal = samples[i-1] + rnorm(1, 0, 5) # Proposal value
  if ((dnorm(proposal, 100, 15) / dnorm(samples[i-1], 100, 15)) > runif(1))
    samples[i] = proposal # Accept proposal
  else(samples[i] = samples[i-1]) # Reject proposal
}