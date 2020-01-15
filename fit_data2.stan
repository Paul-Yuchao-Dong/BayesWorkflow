data {
  int<lower = 1> N;
  int y[N];
}

parameters {
  real<lower=1, upper=1> theta;
  real<lower=0> lambda;
}

model {
  theta ~ beta(1,1);
  lambda ~ normal(0, 6.44787);

  for (i in 1:N){
    real lpdf = poisson_lpmf(y[i] | theta);
    if(y[i]==0){
      target += log_mix(theta, 0, lpdf);
    } else {
      target += log(1-theta) + lpdf;
    }
  }
}
