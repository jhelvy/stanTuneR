# stanTuner
This code uses the algebra solver in [Stan](https://mc-stan.org/) to find the parameters of a distribution that produce a desired tail behavior. Here's how to use it:
1. Select a distribution.
2. Define the quantile boundaries and the amount of probability density that you wish to have above and below those boundaries.
3. Let Stan go find the parameters that produce the desired distribution.

Check out the `examples.R` file for a few examples.

Currently supported distributions:
* Normal
* Beta
* Inverse Gamma

# Example:
Let's say I want to find the parameters of a normal distribution such that P[x < -2.0] ~ 0.01 and P[x > 2.0] ~ 0.01. That is, I want a normal distribution where 98% of the probability density is between (-2, 2).

Use the `targets` argument to set these boundaries:
```
targets = list(
    bound_L = -2,   # LOWER quantile boundary
    bound_U = 2,    # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary
```

Then use the `tuneParams` function to find the parameters:
```
result = util$tuneParams(distribution='normal', targets)
```

View the resulting parameters and verify that the quantiles of 10,000 draws from the resulting distribution match your criteria:
```
result$params
result$quantiles
```

Finally, view a histogram of the resulting distribution:
```
result$histogram
```
