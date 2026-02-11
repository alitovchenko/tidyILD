# Create ema_example dataset for tidyILD (run from package root)
# setwd("path/to/tidyILD"); source("data-raw/create_ema_example.R")
set.seed(12345)
n_id <- 10L
n_obs_per <- 14L
id <- rep(seq_len(n_id), each = n_obs_per)
base_time <- rep(seq(0, by = 3600, length.out = n_obs_per), n_id)
jitter <- runif(length(base_time), -600, 600)
base_time <- base_time + jitter
base_time <- pmax(0, base_time)
time <- as.POSIXct(base_time, origin = "1970-01-01")
person_eff <- rep(rnorm(n_id, 0, 1), each = n_obs_per)
time_eff <- base_time / 3600 * 0.1
mood <- person_eff + time_eff + rnorm(n_id * n_obs_per, 0, 0.5)
stress <- 0.3 * mood + rnorm(n_id * n_obs_per, 0, 0.4)
ema_example <- data.frame(id = id, time = time, mood = mood, stress = stress)
save(ema_example, file = "data/ema_example.rda", compress = "xz")
