README
================
John Helveston
4/30/2019

# stanTuner

This code uses the algebra solver in [Stan](https://mc-stan.org/) to
find the parameters of a distribution that produce a desired tail
behavior. This can be a useful tool when choosing parameters for prior
distributions. Here’s how to use it:

1.  Choose a distribution.
2.  Define the quantile boundaries and the amount of probability density
    that you wish to have above and below those boundaries.
3.  Let Stan go find the parameters that produce the desired
    distribution.

Currently supported distributions:

  - Normal
  - Beta
  - Inverse Gamma

# Required libraries:

If you don’t want to use the shiny app interface, you only need to have
the `rstan` library installed (and obviously also have
[Stan](https://mc-stan.org/) installed).

If you want to use the shiny app interface, you will also need to
install the `shiny` and `shinycssloaders` libraries

# Using the Shiny app

To use the Shiny app, just run the following code in R:

    library(shiny)
    runGitHub('jhelvy/stanTuner')

# Example without the Shiny app

(see the `examples.R` file for more examples)

Let’s say I want to find the parameters of a normal distribution such
that P\[x \< -2.0\] ~ 0.01 and P\[x \> 2.0\] ~ 0.01. That is, I want a
normal distribution where 98% of the probability density is between (-2,
2).

First, load the `rstan` library, tweak some settings, and source the
`functions.R` file (all functions are loaded in a new environment called
`funcs`):

``` r
# Load libraries
library(shiny)
library(rstan)

# Stan settings
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Load the functions
funcs <- new.env()
source('functions.R', local=funcs)
```

Then use the `targets` argument to set the desired tail properties:

``` r
targets = list(
    bound_L = -2,   # LOWER quantile boundary
    bound_U = 2,    # UPPER quantile boundary
    dens_L  = 0.01, # Target density below LOWER quantile boundary
    dens_U  = 0.01) # Target density above UPPER quantile boundary
```

Then use the `tuneParams` function to find the parameters:

``` r
results = funcs$tuneParams(distribution='normal', targets)
```

    ## 
    ## SAMPLING FOR MODEL 'model' NOW (CHAIN 1).
    ## Chain 1: Iteration: 1 / 1 [100%]  (Sampling)
    ## Chain 1: 
    ## Chain 1:  Elapsed Time: 0 seconds (Warm-up)
    ## Chain 1:                2.4e-05 seconds (Sampling)
    ## Chain 1:                2.4e-05 seconds (Total)
    ## Chain 1:

View the resulting parameters and verify that the quantiles of 10,000
draws from the resulting distribution match your criteria:

``` r
results$params
```

    ## $mu
    ## [1] 0
    ## 
    ## $sigma
    ## [1] 0.85972

``` r
results$quantiles
```

    ##        1%       99% 
    ## -1.998884  1.999871

Finally, view a histogram of the resulting distribution:

``` r
results$histogram
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Author and License

  - Author: John Paul Helveston (www.jhelvy.com)
  - Date First Written: Tuesday, April 30, 2019
  - License: GPL-3
