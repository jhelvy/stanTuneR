# Main function for finding distribution parameters
tuneParams <- function(distribution, targets) {
    # Create and fit a stan model file
    generateStanCode(distribution, targets)
    # Get the parameters (rounded to nearest 5th decimal space)
    fit <- stan(file='./model.stan', iter=1, warmup=0, chains=1,
                algorithm="Fixed_param")
    params <- extract(fit)
    params$lp__ <- NULL
    for (i in 1:length(params)) {params[[i]] <- round(params[[i]], 5)}
    # Summarize and store results
    results <- summarizeResults(params, distribution, targets)
    return(results)
}

# -----------------------------------------------------------------------
# Functions for summarizing results
# -----------------------------------------------------------------------

summarizeResults <- function(params, distribution, targets) {
    draws     <- getDraws(distribution, params)
    quantiles <- quantile(draws, c(targets$dens_L, 1-targets$dens_U))
    histogram <- makeHistogram(draws, quantiles)
    return(list(
        params=params, draws=draws, quantiles=quantiles, histogram=histogram))
}

getDraws = function(distribution, params) {
    N <- 10^5
    if (distribution == 'normal') {
        draws <- rnorm(N, mean=params$mu, sd=params$sigma)
    } else if (distribution == 'beta') {
        draws <- rbeta(N, shape1=params$alpha, shape2=params$beta)
    } else if (distribution == 'inv_gamma') {
        draws <- 1 / rgamma(N, shape=params$alpha, rate=params$beta)
    }
  return(draws)
}

makeHistogram = function(draws, quantiles) {
    # Drop extreme values for better plotting
    draws = draws[which(draws > quantile(draws, 0.001))]
    draws = draws[which(draws < quantile(draws, 0.999))]
    # Make the plot
    histogram = ggplot(data.frame(draws), aes(x=draws)) +
        geom_histogram(bins=30, fill='#DCBCBC', color='#C79999') +
        geom_vline(xintercept=quantiles,
                   color='#B97C7C', linetype='dashed', size=1) +
        # Color scheme copied from betanalpha (Thanks Michael!)
        theme_bw() +
        labs(x='x', y='Count')
    return(histogram)
}

# -----------------------------------------------------------------------
# Functions for generating Stan code
# -----------------------------------------------------------------------

generateStanCode <- function(distribution, targets) {
    if (distribution == 'normal') {
        stanCode = generateStanCode_normal(targets)
    } else if (distribution == 'beta') {
        stanCode = generateStanCode_beta(targets)
    } else if (distribution == 'inv_gamma') {
        stanCode = generateStanCode_inv_gamma(targets)
    }
    saveStanCode(stanCode)
}

saveStanCode <- function(stanCode) {
    file <- file('./model.stan')
    writeLines(stanCode, file)
    close(file)
}

# Code for generating Stan model for a Normal distribution
generateStanCode_normal <- function(targets) {

    mu_guess    <- 0
    sigma_guess <- 1

    functionsCode <- paste(
    'functions {',
    '  // Differences between Normal tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = normal_cdf(theta[1], y[1], y[2]) - ', targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - normal_cdf(theta[2], y[1], y[2]) - ', targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    sep='\n')

    transformedDataCode <- paste(
    'transformed data {',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",

    '  // Initial guess at Normal parameters',
    paste('  real mu_guess = ', mu_guess, ';', sep=''),
    paste('  real sigma_guess = ', sigma_guess, ';', sep=''),
    "  vector[2] y_guess = [mu_guess, sigma_guess]';",

    '  // Find Normal density parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',

    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',

    '}',
    sep='\n')

    generatedQuantitiesCode <- paste(
    'generated quantities {',
    '  real mu = y[1];',
    '  real sigma = y[2];',
    '}',
    sep='\n')

    stanCode <- paste(
    functionsCode,
    transformedDataCode,
    generatedQuantitiesCode,
    '\n',
    sep='\n')

    return(stanCode)
}

# Code for generating Stan model for a Beta distribution
generateStanCode_beta <- function(targets) {

    alpha_guess <- 0.5
    beta_guess  <- 0.5

    functionsCode <- paste(
    'functions {',
    '  // Differences between Beta tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = beta_cdf(theta[1], y[1], y[2]) - ', targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - beta_cdf(theta[2], y[1], y[2]) - ', targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    sep='\n')

    transformedDataCode <- paste(
    'transformed data {',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",

    '  // Initial guess at Normal parameters',
    paste('  real alpha_guess = ', alpha_guess, ';', sep=''),
    paste('  real beta_guess = ', beta_guess, ';', sep=''),
    "  vector[2] y_guess = [alpha_guess, beta_guess]';",

    '  // Find Beta density parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',

    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',

    '}',
    sep='\n')

    generatedQuantitiesCode <- paste(
    'generated quantities {',
    '  real alpha = y[1];',
    '  real beta = y[2];',
    '}',
    sep='\n')

    stanCode <- paste(
    functionsCode,
    transformedDataCode,
    generatedQuantitiesCode,
    '\n',
    sep='\n')

    return(stanCode)
}

# Code for generating Stan model for an inverse Gamma distribution
generateStanCode_inv_gamma <- function(targets) {

    alpha_guess <- 5
    beta_guess  <- 5

    functionsCode <- paste(
    'functions {',
    '  // Differences between inverse Gamma tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = inv_gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - ', targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - inv_gamma_cdf(theta[2], exp(y[1]), exp(y[2])) - ', targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    sep='\n')

    transformedDataCode <- paste(
    'transformed data {',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",

    '  // Initial guess at inverse Gamma parameters',
    paste('  real alpha_guess = ', alpha_guess, ';', sep=''),
    paste('  real beta_guess = ', beta_guess, ';', sep=''),
    "  vector[2] y_guess = [log(alpha_guess), log(beta_guess)]';",

    '  // Find inverse Gamma density parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',

    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',

    '}',
    sep='\n')

    generatedQuantitiesCode <- paste(
    'generated quantities {',
    '  real alpha = exp(y[1]);',
    '  real beta = exp(y[2]);',
    '}',
    sep='\n')

    stanCode <- paste(
    functionsCode,
    transformedDataCode,
    generatedQuantitiesCode,
    '\n',
    sep='\n')

    return(stanCode)
}

