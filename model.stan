functions {
  // Differences between tail probabilities and target probabilities
  vector tail_delta(vector y, vector theta, real[] x_r, int[] x_i) {
    vector[2] deltas;
    deltas[1] = inv_gamma_cdf(theta[1], exp(y[1]), exp(y[2])) - 0.01;
    deltas[2] = 1 - inv_gamma_cdf(theta[2], exp(y[1]), exp(y[2])) - 0.01;
    return deltas;
  }
}
transformed data {
  // Number of simulated observations in generated quantities
  int<lower=0> N = 10000;
  // Target quantiles
  real l = 1; // Lower quantile
  real u = 10; // Upper quantile
  vector[2] theta = [l, u]';
  // Initial guess at parameters
  real alpha_guess = 5;
  real beta_guess = 5;
  vector[2] y_guess = [log(alpha_guess), log(beta_guess)]';
  // Find parameters that ensures target density values
  vector[2] y;
  real x_r[0];
  int x_i[0];
  y = algebra_solver(tail_delta, y_guess, theta, x_r, x_i);
}
generated quantities {
  real alpha = exp(y[1]);
  real beta = exp(y[2]);
  // Simulate data
  real y_sim[N];
  for (n in 1:N)
    y_sim[n] = inv_gamma_rng(alpha, beta);
}


