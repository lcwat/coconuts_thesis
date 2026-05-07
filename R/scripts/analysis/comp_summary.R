# this script takes the summaries from beocat and checks the computational 
# quality of the runs (chain diagnostics, convergence, etc.)


# load libraries ----------------------------------------------------------

library(tidyverse)
library(posterior)
library(bayesplot)

# load model objects ------------------------------------------------------

diagnostics <- read_rds('R/cmdstan_output/model_diagnostic_summaries_final.rds')

print(diagnostics$nn)
print(diagnostics$ta)
print(diagnostics$clst)
print(diagnostics$nn_ta)
print(diagnostics$nn_clst)
print(diagnostics$nn_ta_clst)

# no divergences across all of the models, good ebfmi

# posterior inspection ----------------------------------------------------

# look at the chains for different sets of pars
nn_draws <- read_csv('R/cmdstan_output/nn_draws.csv')

# summary
nn_summary <- summarize_draws(nn_draws)

# good Rhats and several hundred effective samples of the posterior for all pars

color_scheme_set('mix-red-orange')

# trace plots
nn_trace <- nn_draws |>
  mcmc_trace() 

ggsave(
  filename = 'fig_output/participants/cog_models/nn_trace.pdf', plot = nn_trace, 
  device = 'pdf', width = 22, height = 25, units = 'in'
)

# visually the chains appear to have converged well for all pars, matching rhats

ta_draws <- read_csv('R/cmdstan_output/ta_draws.csv')

ta_summary <- summarize_draws(ta_draws)

# all rhats appear below 1.02, most below 1.01, good ESS as well for all pars

# trace plots
ta_trace <- ta_draws |> 
  mcmc_trace()

ggsave(
  filename = 'fig_output/participants/cog_models/ta_trace.pdf', plot = ta_trace, 
  device = 'pdf', width = 22, height = 25, units = 'in'
)

# clst
clst_draws <- read_csv('R/cmdstan_output/clst_draws.csv')

clst_summary <- summarize_draws(clst_draws)

# all rhats appear below 1.02, most below 1.01, good ESS as well for all pars

# trace plots
clst_trace <- clst_draws |> 
  mcmc_trace()

ggsave(
  filename = 'fig_output/participants/cog_models/clst_trace.pdf', plot = clst_trace, 
  device = 'pdf', width = 22, height = 25, units = 'in'
)

# nn + ta
nn_ta_draws <- read_csv('R/cmdstan_output/nn_ta_draws.csv')

nn_ta_summary <- summarize_draws(nn_ta_draws)

# all rhats appear below 1.02, most below 1.01, good ESS as well for all pars

# trace plots
nn_ta_trace <- nn_ta_draws |> 
  mcmc_trace()

ggsave(
  filename = 'fig_output/participants/cog_models/nn_ta_trace.pdf', plot = nn_ta_trace, 
  device = 'pdf', width = 22, height = 25, units = 'in'
)

# nn + clst
nn_clst_draws <- read_csv('R/cmdstan_output/nn_clst_draws.csv')

nn_clst_summary <- summarize_draws(nn_clst_draws)

# trace plots
nn_clst_trace <- nn_clst_draws |> 
  mcmc_trace()

ggsave(
  filename = 'fig_output/participants/cog_models/nn_clst_trace.pdf', plot = nn_clst_trace, 
  device = 'pdf', width = 22, height = 25, units = 'in'
)

# full model: nn + ta + clst
nn_ta_clst_draws <- read_csv('R/cmdstan_output/nn_ta_clst_draws.csv')

nn_ta_clst_summary <- summarize_draws(nn_ta_clst_draws)

# trace plots
nn_ta_clst_trace <- nn_ta_clst_draws |> 
  mcmc_trace()

ggsave(
  filename = 'fig_output/participants/cog_models/nn_ta_clst_trace.pdf', plot = nn_ta_clst_trace, 
  device = 'pdf', width = 22, height = 25, units = 'in'
)

# computational check: looks good! most of the issues cropped up on the efficiency end
# but no clear divergences or pathological chain behavior (all rhats < 1.01). really good
# ESS on the nn model family, confident that this posterior estimation is good