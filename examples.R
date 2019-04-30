# ----------------------------------------------------------------------------
# Initial setup

library(rstan)
# Set auto_write to false because I want to always search from scratch
rstan_options(auto_write = FALSE)
options(mc.cores = parallel::detectCores())

util <- new.env()
source('utility.R', local=util)

# ----------------------------------------------------------------------------
# Find normal distribution parameters with targets:
# P[x < -2.0] ~ 0.01
# P[x > 2.0] ~ 0.01

targets = list(
    bound_L = -2,   # LOWER quantile boundary
    bound_U = 2,    # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

result = util$tuneParams(distribution='normal', targets)
result$params
result$quantiles
result$histogram

# ----------------------------------------------------------------------------
# Find beta distribution parameters with targets:
# P[x < 0.5] ~ 0.01
# P[x > 0.99] ~ 0.01

targets = list(
    bound_L = 0.5,  # LOWER quantile boundary
    bound_U = 0.99, # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

result = util$tuneParams(distribution='beta', targets)
result$params
result$quantiles
result$histogram

# ----------------------------------------------------------------------------
# Find inv_gamma distribution parameters with targets:
# P[x < 1.0] ~ 0.01
# P[x > 10] ~ 0.01

targets = list(
    bound_L = 1,    # LOWER quantile boundary
    bound_U = 10,   # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

result = util$tuneParams(distribution='inv_gamma', targets)
result$params
result$quantiles
result$histogram
