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
