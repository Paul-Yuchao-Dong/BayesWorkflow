library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(foreach)
library(doParallel)
util <- new.env()
source("./R/stan_utility.R", local = util)
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

# Step 1. Conceptual Analysis. 1000 detectors were directed at the source and set to record number of incident particles over  an interval of time. Intensity of particles constant, and each detector identical

# Step 2. Define observations.

writeLines(readLines("./fit_data.stan", n= 4))

## integer counts for each of N = 1000 detectors
# Step 3. Identify relevant statistics.

## histogram of detector counts as summary stat!
## Fake domain expertise: counts over 25 would be extreme

# Step 4. Build a model
## got the prior on lambda halfNormal(0, 6.44787)

# Step 5. Identify new summary stat
## histogram summary seems well suited for the iid Poisson model

# Step 6. Analyze the joint ensemble.
## 1,000 draws from the joint ensemble. Each of which simulates a ground truth and observed values for the N = 1,000 detectors.

R <- 1000
N <- 1000
simu_data <- list("N" = N)
fit <- stan("./sample_joint_ensemble.stan", data = simu_data, algorithm = "Fixed_param", iter = R, warmup = 0, chains = 1, refresh=R, seed = 4838282)

simu_lambdas <- extract(fit)$lambda
simu_ys <- extract(fit)$y

## Analyze the prior predictive distribution
B <- 40
idx <- rep(0:B, each = 2)
x <- sapply(1:length(idx), function(b) if(b%%2==0) idx[b]+0.5 else idx[b]-0.5)

hist(simu_ys[1,])
hist(simu_ys[2,], add = T)
hist(simu_ys[3,], add = T)

