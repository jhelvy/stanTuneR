# Main function for finding distribution parameters
tuneParams <- function(distribution, targets) {
    # Create and fit a stan model file
    generateStanCode(distribution, targets)
    # Find the parameters
    fit <- stan(file='./model.stan', iter=1, warmup=0, chains=1,
                algorithm="Fixed_param")
    # Summarize return results
    results <- summarizeResults(fit, distribution, targets)
    return(results)
}

# -----------------------------------------------------------------------
# Functions for summarizing results
# -----------------------------------------------------------------------

summarizeResults <- function(fit, distribution, targets) {
    results <- extract(fit)
    results$lp__ <- NULL
    # Extract the draws
    draws <- as.vector(results$y_sim)
    results$y_sim <- NULL
    # Round parameters to nearest 6 decimal places
    for (i in 1:length(results)) {results[[i]] <- round(results[[i]], 6)}
    # Get the quantiles
    quantiles <- quantile(draws, c(targets$dens_L, 1-targets$dens_U))
    # Make a histogram of the draws
    histogram <- makeHistogram(draws, quantiles)
    # Return the results
    return(list(
        params=results, draws=draws, quantiles=quantiles, histogram=histogram))
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
    stanCodeGenerator <- getStanCodeGenerator()
    stanCode <- stanCodeGenerator[[distribution]](targets)
    file <- file('./model.stan')
    writeLines(stanCode, file)
    close(file)
}

# Creates a list of functions indexed by the distribution name
getStanCodeGenerator <- function() {
    return(list(
        normal    = getStanCode_normal,
        lognormal = getStanCode_lognormal,
        beta      = getStanCode_beta,
        gamma     = getStanCode_gamma,
        inv_gamma = getStanCode_inv_gamma))
}

# Code for generating Stan model for a Normal distribution
getStanCode_normal <- function(targets) {

    mu_guess    <- 0
    sigma_guess <- 1

    return(paste(
    'functions {',
    '  // Differences between tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = normal_cdf(theta[1], y[1], y[2]) - ',
          targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - normal_cdf(theta[2], y[1], y[2]) - ',
          targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    'transformed data {',
    '  // Number of simulated observations in generated quantities',
    '  int<lower=0> N = 10000;',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",
    '  // Initial guess at parameters',
    paste('  real mu_guess = ', mu_guess, ';', sep=''),
    paste('  real sigma_guess = ', sigma_guess, ';', sep=''),
    "  vector[2] y_guess = [mu_guess, sigma_guess]';",
    '  // Find parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',
    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',
    '}',
    'generated quantities {',
    '  real mu = y[1];',
    '  real sigma = y[2];',
    '  // Simulate data',
    '  real y_sim[N];',
    '  for (n in 1:N)',
    '    y_sim[n] = normal_rng(mu, sigma);',
    '}',
    sep='\n'))
}

# Code for generating Stan model for a Normal distribution
getStanCode_lognormal <- function(targets) {

    mu_guess    <- 0
    sigma_guess <- 1

    return(paste(
    'functions {',
    '  // Differences between tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = lognormal_cdf(theta[1], y[1], y[2]) - ',
          targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - lognormal_cdf(theta[2], y[1], y[2]) - ',
          targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    'transformed data {',
    '  // Number of simulated observations in generated quantities',
    '  int<lower=0> N = 10000;',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",
    '  // Initial guess at parameters',
    paste('  real mu_guess = ', mu_guess, ';', sep=''),
    paste('  real sigma_guess = ', sigma_guess, ';', sep=''),
    "  vector[2] y_guess = [mu_guess, sigma_guess]';",
    '  // Find parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',
    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',
    '}',
    'generated quantities {',
    '  real mu = y[1];',
    '  real sigma = y[2];',
    '  // Simulate data',
    '  real y_sim[N];',
    '  for (n in 1:N)',
    '    y_sim[n] = lognormal_rng(mu, sigma);',
    '}',
    sep='\n'))
}

# Code for generating Stan model for a Beta distribution
getStanCode_beta <- function(targets) {

    alpha_guess <- 0.5
    beta_guess  <- 0.5

    return(paste(
    'functions {',
    '  // Differences between tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = beta_cdf(theta[1], y[1], y[2]) - ',
          targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - beta_cdf(theta[2], y[1], y[2]) - ',
          targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    'transformed data {',
    '  // Number of simulated observations in generated quantities',
    '  int<lower=0> N = 10000;',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",
    '  // Initial guess at parameters',
    paste('  real alpha_guess = ', alpha_guess, ';', sep=''),
    paste('  real beta_guess = ', beta_guess, ';', sep=''),
    "  vector[2] y_guess = [alpha_guess, beta_guess]';",
    '  // Find parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',
    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',
    '}',
    'generated quantities {',
    '  real alpha = y[1];',
    '  real beta = y[2];',
    '  // Simulate data',
    '  real y_sim[N];',
    '  for (n in 1:N)',
    '    y_sim[n] = beta_rng(alpha, beta);',
    '}',
    sep='\n'))
}

# Code for generating Stan model for a Gamma distribution
getStanCode_gamma <- function(targets) {
    
    alpha_guess <- 5
    beta_guess  <- 1
    
    return(paste(
        'functions {',
        '  // Differences between tail probabilities and target probabilities',
        '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
        '    vector[2] deltas;',
        paste('    deltas[1] = gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - ',
              targets$dens_L, ';', sep=''),
        paste('    deltas[2] = 1 - gamma_cdf(theta[2], exp(y[1]), exp(y[2]))',
              ' - ', targets$dens_U, ';', sep=''),
        '    return deltas;',
        '  }',
        '}',
        'transformed data {',
        '  // Number of simulated observations in generated quantities',
        '  int<lower=0> N = 10000;',
        '  // Target quantiles',
        paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
        paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
        "  vector[2] theta = [l, u]';",
        '  // Initial guess at parameters',
        paste('  real alpha_guess = ', alpha_guess, ';', sep=''),
        paste('  real beta_guess = ', beta_guess, ';', sep=''),
        "  vector[2] y_guess = [log(alpha_guess), log(beta_guess)]';",
        '  // Find parameters that ensures target density values',
        '  vector[2] y;',
        '  real x_r[0];',
        '  int x_i[0];',
        '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',
        '}',
        'generated quantities {',
        '  real alpha = exp(y[1]);',
        '  real beta = exp(y[2]);',
        '  // Simulate data',
        '  real y_sim[N];',
        '  for (n in 1:N)',
        '    y_sim[n] = gamma_rng(alpha, beta);',
        '}',
        sep='\n'))
}

# Code for generating Stan model for an Inverse Gamma distribution
getStanCode_inv_gamma <- function(targets) {

    alpha_guess <- 5
    beta_guess  <- 1

    return(paste(
    'functions {',
    '  // Differences between tail probabilities and target probabilities',
    '  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {',
    '    vector[2] deltas;',
    paste('    deltas[1] = inv_gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - ',
          targets$dens_L, ';', sep=''),
    paste('    deltas[2] = 1 - inv_gamma_cdf(theta[2], exp(y[1]), exp(y[2]))',
          ' - ', targets$dens_U, ';', sep=''),
    '    return deltas;',
    '  }',
    '}',
    'transformed data {',
    '  // Number of simulated observations in generated quantities',
    '  int<lower=0> N = 10000;',
    '  // Target quantiles',
    paste('  real l = ', targets$bound_L, '; // Lower quantile', sep=''),
    paste('  real u = ', targets$bound_U, '; // Upper quantile', sep=''),
    "  vector[2] theta = [l, u]';",
    '  // Initial guess at parameters',
    paste('  real alpha_guess = ', alpha_guess, ';', sep=''),
    paste('  real beta_guess = ', beta_guess, ';', sep=''),
    "  vector[2] y_guess = [log(alpha_guess), log(beta_guess)]';",
    '  // Find parameters that ensures target density values',
    '  vector[2] y;',
    '  real x_r[0];',
    '  int x_i[0];',
    '  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);',
    '}',
    'generated quantities {',
    '  real alpha = exp(y[1]);',
    '  real beta = exp(y[2]);',
    '  // Simulate data',
    '  real y_sim[N];',
    '  for (n in 1:N)',
    '    y_sim[n] = inv_gamma_rng(alpha, beta);',
    '}',
    sep='\n'))
}

