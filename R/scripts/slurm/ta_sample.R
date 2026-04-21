library(cmdstanr)

# add feature matrices
dat$N_feat <- 2
dat$Feature_matrix <- array(NA, dim = c(dat$N, dat$N_coconut, dat$N_feat))

dat$Feature_matrix[, , 1] <- coconut_ta
dat$Feature_matrix[, , 2] <- coconut_pv

# standardize features within collections, ignore pv
for (i in 1:dat$N) {
    for (j in c(1:dat$N_feat)) {
        # check if level is level with no point value variation
        if (dat$level[i] %in% c(1, 5, 10) & j == 2) { # change index to align with pv matrix
            dat$Feature_matrix[i, 1:dat$C[i], j] <- 0
        } else {
            dat$Feature_matrix[i, 1:dat$C[i], j] <- standardize(dat$Feature_matrix[i, 1:dat$C[i], j])
        }
    }
}

# compile model
m_parallel <- cmdstan_model(
    "/homes/lcwatson/R/coconuts_thesis/scripts/parallel_rl_model.stan",
    cpp_options = list(stan_threads = TRUE)
)

# need 16 cores for 4 parallel chains and 4 threads per chain
ta_fit <- m_parallel$sample(
    dat,
    chains = 2, parallel_chains = 2, threads_per_chain = 8,
    refresh = 50, iter_warmup = 500, adapt_delta = 0.99, iter_sampling = 1500,
    output_dir = "/fastscratch/lcwatson", # write to fastscratch on beocat for faster I/O
    output_basename = "ta_model",
    seed = 888
)
