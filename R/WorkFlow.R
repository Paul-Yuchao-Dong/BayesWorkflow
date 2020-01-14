library(magrittr)
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

length(simu_ys[simu_ys > 25]) / length(simu_ys)
dim(simu_ys)

## Evaluate simulated fits

tryCatch({
  registerDoParallel(makeCluster(detectCores()))

  simu_list <- t(data.matrix(data.frame(simu_lambdas, simu_ys)))

  util <- new.env()
  source("./R/stan_utility.R", local = util)

  ensemble_output <- foreach(simu = simu_list, .combine = 'cbind') %dopar% {
    simu_lambda <- simu[1]
    simu_y <- simu[2:(N+1)]

  ensemble_output <- foreach(simu=simu_list,
                             .combine='cbind') %dopar% {
   simu_lambda <- simu[1]
   simu_y <- simu[2:(N + 1)];

    capture.output(library(rstan))
    capture.output(fit <- sampling(fit_model, data = input_data,seed=4938483))

    warning_code <- util$check_all_diagnostics(fit, quiet = TRUE)
    sbc_rank <- sum(simu_lambda<extract(fit)$lambda[seq(1,4000-8,8)])
   ### I don't understand why this rank is expected to be Uniform[1,500], shouldn't fit process make it Normal(250, sigma)?
    s <- summary(fit, probs = c(), pars = "lambda")$summary
    post_mean_lambda <- s[,1]
    post_sd_lambda <- s[,3]

   warning_code <- util$check_all_diagnostics(fit, quiet=TRUE)

    z_score <- (post_mean_lambda - simu_lambda) / post_sd_lambda
    shrinkage <- 1-(post_sd_lambda / prior_sd_lambda)^2
    c(warning_code, sbc_rank, z_score, shrinkage)
  }

   # Compute posterior sensitivities
   s <- summary(fit, probs = c(), pars='lambda')$summary
   post_mean_lambda <- s[,1]
   post_sd_lambda <- s[,3]

   prior_sd_lambda <- 6.44787

   z_score <- (post_mean_lambda - simu_lambda) / post_sd_lambda
   shrinkage <- 1 - (post_sd_lambda / prior_sd_lambda)**2

   c(warning_code, sbc_rank, z_score, shrinkage)
                             }
}, finally={ stopImplicitCluster() })
## warning code
sum(ensemble_output[1,])

## simulation based calibration

sbc_rank <- ensemble_output[2,]
sbc_hist <- hist(sbc_rank, seq(0, 500, 25) - 0.5,
                 col=c_dark, border=c_dark_highlight, plot=FALSE)
plot(sbc_hist, main="", xlab="Prior Rank", yaxt='n', ylab="")
low <- qbinom(0.005, R, 1 / 20)
mid <- qbinom(0.5, R, 1 / 20)
high <- qbinom(0.995, R, 1 / 20)
bar_x <- c(-10, 510, 500, 510, -10, 0, -10)
bar_y <- c(high, high, mid, low, low, mid, high)

polygon(bar_x, bar_y, col=c("#DDDDDD"), border=NA)
segments(x0=0, x1=500, y0=mid, y1=mid, col=c("#999999"), lwd=2)

plot(sbc_hist, col=c_dark, border=c_dark_highlight, add=T)

z_score <- ensemble_output[3,]
shrinkage <- ensemble_output[4,]

plot(shrinkage, z_score)
## z_score concentrated around 0, which is good
## shrinkage towards one meaning the post_sd_lambda is much smaller than the prior, effective fitting
## if there's a question that can be quatified as
