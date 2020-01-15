library(magrittr)
library(dplyr)
library(tidyr)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(tidybayes)

readLines("sample_joint_ensemble.stan") %>%
  writeLines()

R <- 500
N <- 1000

# Generating 500 iterations of simulated data, each iteration should generate 1,000 ys

simu_data <- list("N" = N)
capture.output( fit <- stan("sample_joint_ensemble.stan", algorithm = "Fixed_param",
            data = simu_data, iter = R, warmup = 0, chains = 1, refresh = R,
            seed=4838282
))

# the rstan method of getting the ys
extract(fit)$y ->simu_ys_matrix
simu_ys_matrix[1,] %>% summary()
extract(fit)$lambda[[1]]
# the tidybayes way of getting the ys
fit %>%
  spread_draws(y[i], lambda) %>%
  ungroup() %>%
  filter(.iteration == 1) %>%
  pull(y) %>%
  summary()

# the result is not the same

# another try
fit %>%
  spread_draws(y[i], lambda) %>%
  ungroup() %>%
  filter(i == 1) %>%
  pull(y) %>%
  summary()

# also a different set of results. What is the right way to get to the simulated ys from the same lambda?
fit %>%
  spread_draws(y[i], lambda) %>%
  ungroup() %>%
  group_by(lambda,.iteration) %>%
  summarise(y = list(y))

fit %>%
  spread_draws(y[i], lambda) %>%
  ungroup() %>%
  filter(.iteration == 1) %>%
  count(lambda, .iteration)

fit %>%
  spread_draws(y[i], lambda) %>%
  ungroup() %>%
  filter(i == 1) %>%
  count(lambda, .iteration)

# Conclusion: a .iteration identifies a set of simulated data
# rstan extract probably used a different random order of the sets
