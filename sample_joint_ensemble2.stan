data {
  int<lower = 1> N;
}

generated quantities {
  real<lower=0, upper=1> theta = beta_rng(1,1);
  real<lower=0> lambda = fabs(normal_rng(0, 6.44787));

  int y[N] = rep_array(0, N);

  for (i in 1:N){
    if (!bernoulli_rng(theta)){
      y[i] = poisson_rng(lambda);
    }
  }
}
