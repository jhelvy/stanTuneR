functions {
  // Differences between tail probabilities and target probabilities
  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;
    deltas[1] = normal_cdf(theta[1], y[1], y[2]) - 0.01;
    deltas[2] = 1 - normal_cdf(theta[2], y[1], y[2]) - 0.01;
    return deltas;
  }
}
transformed data {
  // Number of simulated observations in generated quantities
  int<lower=0> N = 10000;
  // Target quantiles
  real l = -2; // Lower quantile
  real u = 2; // Upper quantile
  vector[2] theta = [l, u]';
  // Initial guess at parameters
  real mu_guess = 0;
  real sigma_guess = 1;
  vector[2] y_guess = [mu_guess, sigma_guess]';
  // Find parameters that ensures target density values
  vector[2] y;
  real x_r[0];
  int x_i[0];
  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);
}
generated quantities {
  real mu = y[1];
  real sigma = y[2];
  // Simulate data
  real y_sim[N];
  for (n in 1:N)
    y_sim[n] = normal_rng(mu, sigma);
}
