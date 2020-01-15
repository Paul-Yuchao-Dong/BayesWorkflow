data {
  int N;
  int y[N];
}

parameters {
  real<lower = 0> lambda;
}

model {
  lambda ~ normal(0, 6.44787);

  for (i in 1:N){
    y[i] ~ poisson(lambda);
  }
}

generated quantities{
  int y_ppc[N];

  for (i in 1:N) y_ppc[i] = poisson_rng(lambda);
}
