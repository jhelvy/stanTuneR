# ----------------------------------------------------------------------------
# Initial setup

# Load libraries
library(shiny)
library(rstan)

# Stan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load the functions
funcs <- new.env()
source('functions.R', local=funcs)

# ----------------------------------------------------------------------------
# Find normal distribution parameters with targets:
# P[x < -2.0] ~ 0.01
# P[x > 2.0] ~ 0.01

targets = list(
    bound_L = -2,   # LOWER quantile boundary
    bound_U = 2,    # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

results = funcs$tuneParams(distribution='normal', targets)
results$params
results$quantiles
results$histogram

# ----------------------------------------------------------------------------
# Find lognormal distribution parameters with targets:
# P[x < 0.1] ~ 0.01
# P[x > 4.0] ~ 0.01

targets = list(
    bound_L = 0.1,  # LOWER quantile boundary
    bound_U = 4,    # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

results = funcs$tuneParams(distribution='lognormal', targets)
results$params
results$quantiles
results$histogram

# ----------------------------------------------------------------------------
# Find beta distribution parameters with targets:
# P[x < 0.5] ~ 0.01
# P[x > 0.99] ~ 0.01

targets = list(
    bound_L = 0.5,  # LOWER quantile boundary
    bound_U = 0.99, # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

results = funcs$tuneParams(distribution='beta', targets)
results$params
results$quantiles
results$histogram

# ----------------------------------------------------------------------------
# Find gamma distribution parameters with targets:
# P[x < 1.0] ~ 0.01
# P[x > 10] ~ 0.01

targets = list(
    bound_L = 1,    # LOWER quantile boundary
    bound_U = 10,   # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

results = funcs$tuneParams(distribution='gamma', targets)
results$params
results$quantiles
results$histogram

# ----------------------------------------------------------------------------
# Find inv_gamma distribution parameters with targets:
# P[x < 1.0] ~ 0.01
# P[x > 10] ~ 0.01

targets = list(
    bound_L = 1,    # LOWER quantile boundary
    bound_U = 10,   # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary

results = funcs$tuneParams(distribution='inv_gamma', targets)
results$params
results$quantiles
results$histogram
