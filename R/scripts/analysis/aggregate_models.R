# model aggregate performance metrics with a series of models
# Luke Watson
# Feb 2026


# load libraries ----------------------------------------------------------

library(tidyverse)
library(tidybayes)
library(bayesplot)
library(posterior)
library(brms)
library(marginaleffects)

# source theme
source('R/scripts/src/project_theme.R')
fig_path <- 'fig_output/participants/models/' # set path for fig output

# set opts
options("marginaleffects_posterior_interval" = "hdi")
options("marginaleffects_posterior_center" = "mean")


# load data ---------------------------------------------------------------

# performance_and_rmi <- read_csv('data/clean_datasets/cleaned_metrics_summary.csv')

performance_and_rmi <- read_csv('data/clean_datasets/agg_summary_data.csv')

performance_and_rmi <- performance_and_rmi |> 
  mutate(
    level_string = fct_reorder(factor(str_c('level ', level)), level)
  )
#
# clean data --------------------------------------------------------------

# create new vector of subject ids
sub <- vector('numeric')

k = 1

# recode subjects for easier computation
for(i in 1:nrow(performance_and_rmi)) {
  
  if(i > 1) {
    if(performance_and_rmi$subject[i] != performance_and_rmi$subject[i-1]) {
      k = k+1
    }
  }
  
  sub[i] = k
}

performance_and_rmi <- performance_and_rmi |> 
  mutate(
    subject = sub
  )

# factor cats
performance_and_rmi <- performance_and_rmi |> 
  mutate(
    subject = factor(subject), 
    level = factor(level)
  )

# add level order
performance_and_rmi <- performance_and_rmi |> 
  group_by(subject) |> 
  arrange(min_time, .by_group = T) |> 
  mutate(
    level_order = 1:length(level)
  )

# standardize variables
performance_and_rmi <- performance_and_rmi |> 
  mutate(
    s_total_time = (true_time - mean(true_time)) / sd(true_time), 
    s_rmi = (rmi - mean(rmi)) / sd(rmi),
    s_time_in_game = (level_order - mean(level_order)) / sd(level_order)
  )

# glimpse changes
glimpse(performance_and_rmi)
summary(performance_and_rmi)

# save again
write_csv(performance_and_rmi, 'data/clean_datasets/agg_summary_data.csv')

# generative model --------------------------------------------------------

# depending on specific level and how long player has been playing, their 
# performance should improve with time in game and rmi 

# Time to complete <- RMI * Time in game + (RMI * Time in game | Subject) + 
#   (RMI * Time in game | Level)

# estimand ----------------------------------------------------------------

# do players improve with more experience in the game? 
# specifically, do their times to complete decrease and RMI increase as a 
# function of in game experience?

# does a higher degree of traplining result in better performance?


# varying level intercept -------------------------------------------------

# start with the simplest instantiation of the model, vary mean performance 
# estimates by the level id

# look at the raw mean estimates for each level
performance_and_rmi |> 
  group_by(level) |> 
  summarize(
    mean = mean(true_time)
  ) |> 
  ggplot(
    aes(x = level, y = mean)
  ) +
  geom_bar(stat = 'identity', fill = 'firebrick') +
  
  project_theme()

# now look at performance dists
performance_and_rmi |> 
  ggplot(aes(x = true_time, y = level)) +
  ggridges::geom_density_ridges(fill = 'firebrick') +
  project_theme()
  
# fit a model with a Gamma likelihood, not quite sure what this looks like, 
# been seeing online that model gets reparameterized to have a mean and shape
# parameter G(mu, shape) where mu = shape * rate or shape / scale

#' time[i] ~ Gamma(mu[i], shape)
#' log(mu[i]) = (bar alpha + zeta[level][i] * sigma[alpha])/shape
#' bar alpha ~ Normal(5, 3)   // weakly informed intercept, log(150) ~ 5, log(25) ~ 3
#' zeta[j] ~ Normal(0, 1)   // offsets for levels from bar alpha
#' sigma[alpha] ~ Exponential(1)    // variance for offsets
#' shape ~ Gamma(.001, .001)

# check out priors
get_prior(
  data = performance_and_rmi, 
  family = Gamma(link='log'), 
  s_total_time ~ 1 + (1 | level)
) |> 
  View()

# fit model
b1.1 <- brm(
  data = performance_and_rmi, 
  family = Gamma(link = 'log'), 
  true_time ~ 1 + (1 | level), # vary means by level
  prior = c(
    prior(normal(5, 3), class = Intercept, lb = 0), # intercept prior is on log scale
    prior(exponential(1), class = sd), 
    prior(normal(0, 1), class = sd, coef = Intercept, group = level)
  ), 
  iter = 4000, warmup = 2000, chains = 4, cores = 4, 
  control = list(adapt_delta = .95), # avoid divergence
  sample_prior = T,
  seed = 888, 
  file = 'R/fits/b1.1'
)

# level variances seemed to have a bit of trouble converging
print(b1.1)

b1.1$prior

# plot chains
color_scheme_set("orange")

as_draws_df(b1.1) |> 
  mcmc_trace(
    pars = vars(b_Intercept:`r_level[10,Intercept]`),
    facet_args = list(ncol = 4), 
    linewidth = 0.15
  ) +
  theme(legend.position = "none")

# good convergence

# see how model predictions align with data
nd <- tibble(level = factor(1:10))

f <- fitted(
  b1.1,
  newdata = nd, 
  re_formula = ~ (1 | level)
) |> 
  as_tibble() |> 
  bind_cols(nd)

# plot over sample data
f |> 
  ggplot(aes(x = level)) +
  
  # plot orig data in background
  geom_jitter(
    data = performance_and_rmi, 
    aes(y = true_time), 
    width = .1, shape = 1, 
    alpha = .2, color = clrs[5]
  ) +
  
  # plot model estimates in foreground
  geom_pointinterval(
    aes(y = Estimate, ymin = Q2.5, ymax = Q97.5), 
    size = 5, color = clrs[4]
  ) +
  
  project_theme()

# looks to be right on the money! but there is a lot more variation to be had in there

posterior_summary(b1.1)

# compare prior to posterior
tibble(
  prior = prior_draws(b1.1) |> pull(sd_level__Intercept), 
  post = as_draws_df(b1.1) |> pull(sd_level__Intercept)
) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(aes(x = value, fill = name)) +
  
  geom_density(alpha = .4) +
  
  project_theme()

# could prob do a much narrower prior for intercept

tibble(
  prior = prior_draws(b1.1) |> pull(Intercept), 
  post = as_draws_df(b1.1) |> pull(Intercept)
) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(aes(x = value, fill = name)) +
  
  geom_density(alpha = .4) +
  
  project_theme()

# could improve the shape prior, perhaps to .1, .1 to be more informative, 
# but the model had no issue estimating this, had largest ESS
tibble(
  x = seq(0.1, 100.1), 
  dens = dgamma(x, .1, .1)
) |> 
  ggplot(aes(x = x, y = dens)) +
  geom_line()

# varying levels and subjects ---------------------------------------------

# vary levels and subject means for each level's performance at same time

# plot out subject means for each level
performance_and_rmi |>
  
  ggplot(aes(x = subject, y = true_time, fill = subject)) +
  
  geom_bar(stat = 'identity') + 
  
  scale_fill_viridis_d(guide = 'none', option = 'magma') +
  
  project_theme() +
  
  facet_wrap(~level)

# see updated default priors
get_prior(
  data = performance_and_rmi, 
  family = Gamma(link='log'), 
  s_total_time ~ 1 + (1 | level) + (1 | subject)
) |> 
  View()

# fit model
b2.1 <- brm(
  data = performance_and_rmi, 
  family = Gamma(link = 'log'), 
  true_time ~ 1 + (1 | level) + (1 | subject), # vary means by level and subject
  prior = c(
    prior(normal(5, 2), class = Intercept), # intercept prior is on log scale
    prior(exponential(1), class = sd), # variances
    prior(gamma(.1, .1), class = shape) # narrower shape prior
  ), 
  iter = 4000, warmup = 2000, chains = 4, cores = 4, 
  control = list(adapt_delta = .95), # avoid divergence
  sample_prior = T,
  seed = 888, 
  file = 'R/fits/b2.1'
)

print(b2.1)

# sampling was much better for this model
b2.1$prior
as_draws_df(b2.1) |> 
  mcmc_trace(
    pars = vars(b_Intercept:`r_subject[87,Intercept]`),
    facet_args = list(ncol = 4), 
    linewidth = 0.15
  ) +
  theme(legend.position = "none")

ggsave(
  'fig_output/participants/models/b2.1_trace_plot.pdf', device = 'pdf', 
  width = 10, height = 25, units = 'in'
)

# compare variance estimates between levels and subject groups
as_draws_df(b2.1) |> 
  select(starts_with('sd_')) |> 
  set_names(str_c("sigma[", c("level", "subject"), "]")) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(
    aes(x = value, y = fct_reorder(name, value))
  ) +
  stat_halfeye(fill = 'firebrick') +
  scale_y_discrete(labels = ggplot2:::parse_safe) +
  scale_alpha_continuous("CI width", range = c(0.7, 0.15)) +
  xlim(0, NA) +
  labs(y = 'parameter') +
  project_theme()

# subject variance seems to be more stable (maybe more groups?) while level variance
# seems to be greater (larger variability in environment affecting performance
# compared to individual performance differences)

# probe model for individual subject and level effects
subj_nd <- performance_and_rmi |> distinct(subject)
level_nd <- tibble(level = 1:10)

# subjects, averaged across levels
f <- fitted(
  b2.1, 
  newdata = subj_nd, 
  re_formula = ~ (1 | subject)
) |> 
  as_tibble() |> 
  bind_cols(subj_nd)

# plot
f |> 
  left_join(performance_and_rmi |> select(subject, true_time)) |> 
  ggplot(
    aes(x = fct_reorder(subject, Estimate))
  ) +
  
  # points in background
  geom_jitter(
    aes(y = true_time), 
    width = .1, alpha = .2, color = clrs[5], shape = 1
  ) +
  
  # estimates in foreground
  geom_pointinterval(
    aes(y = Estimate, ymin = Q2.5, ymax = Q97.5), 
    color = clrs[4], size = 4
  ) + 
  labs(y = 'Time (s)') +
  project_theme()

# levels
f <- fitted(
  b2.1, 
  newdata = level_nd, 
  re_formula = ~ (1 | level)
) |> 
  as_tibble() |> 
  bind_cols(level_nd)

# plot
f |> 
  mutate(level = factor(level)) |> 
  left_join(performance_and_rmi |> select(subject, level, true_time)) |> 
  ggplot(
    aes(x = fct_reorder(level, Estimate))
  ) +
  
  # points in background
  geom_jitter(
    aes(y = true_time), 
    width = .2, alpha = .2, color = clrs[5], shape = 1
  ) +
  
  # estimates in foreground
  geom_pointinterval(
    aes(y = Estimate, ymin = Q2.5, ymax = Q97.5), 
    color = clrs[4], size = 4
  ) + 
  labs(y = 'Time (s)', x = 'level') +
  scale_y_continuous(limits = c(0, 500)) +
  project_theme()

# maybe try the simulation plots for new players from model estimates
n_players <- 100

set.seed(888)

f <- as_draws_df(b2.1) |> 
  slice_sample(n = n_players) |> 
  
  # simulate players
  mutate(
    p_sim_mean = rnorm(n(), mean = b_Intercept, sd = sd_subject__Intercept)
  ) |> 
  pivot_longer(`r_level[1,Intercept]`:`r_level[10,Intercept]`) |> 
  mutate(
    fitted = exp(p_sim_mean + value)
  ) |> 
  mutate(
    level = str_extract(name, '\\d+')
  ) |> 
  select(.draw:level)

# plot 
f |> 
  ggplot(aes(x = level, y = fitted, group = .draw)) +
  geom_line(alpha = .5, color = clrs[5]) +
  project_theme()

# projects the same up and down relationship 

# rmi varying by level and subject ----------------------------------------

# see how rmi relates to performance across levels
performance_and_rmi |> 
  ggplot(aes(x = rmi, y = true_time)) +
  geom_point(shape = 1, alpha = .6, color = clrs[4]) +
  geom_smooth(method = 'lm', se = F, color = clrs[5]) +
  project_theme() +
  facet_wrap(~level)

# look at the priors
get_prior(
  data = performance_and_rmi, 
  true_time ~ 1 + rmi + (rmi | level) + (rmi | subject), 
  family = Gamma(link = 'log')
) |> 
  View()

# fit model
b3.1 <- brm(
  data = performance_and_rmi, 
  family = Gamma(link = 'log'), 
  true_time ~ 1 + rmi + (rmi | level) + (rmi | subject), # vary means by level and subject
  prior = c(
    prior(normal(5, 2), class = Intercept), # intercept prior is on log scale
    prior(normal(0, 3), class = b), # rmi prior
    prior(exponential(1), class = sd), # variances
    prior(gamma(.1, .1), class = shape) # narrower shape prior
  ), 
  iter = 4000, warmup = 2000, chains = 4, cores = 4, 
  control = list(adapt_delta = .95), # avoid divergence
  sample_prior = T,
  seed = 888, 
  file = 'R/fits/b3.1'
)

# check out results
print(b3.1)

# appears to be good sampling again, high ESS and rhats of 1

b3.1$prior

# look at parameter distributions
as_draws_df(b3.1) |> 
  names()

# could look at the average or marginal of rmi on performance averaging over levels
# and subjects
nd <- tibble(rmi = seq(0, 1, by = .1))

f <- fitted(
  b3.1, 
  newdata = nd, 
  re_formula = NA
) |> 
  as_tibble() |> 
  bind_cols(nd)

f |> 
  ggplot(aes(x = rmi)) +
  
  # data in background
  geom_point(
    data = performance_and_rmi, 
    aes(y = true_time), 
    alpha = .6, color = clrs[4], shape = 1
  ) +
  
  # error ribbon
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5), 
    fill = clrs[5], alpha = .5
  ) +
  
  # mean line
  geom_line(
    aes(y = Estimate), 
    color = clrs[5]
  ) +
  
  project_theme()

# predict rmi across level and average over subjects 
nd <- expand_grid(
  rmi = seq(0, 1, by = .1), 
  level = 1:10
)

f <- fitted(
  b3.1, 
  newdata = nd, 
  re_formula = ~ (rmi | level)
) |> 
  as_tibble() |> 
  bind_cols(nd)

f |> 
  mutate(level = factor(level)) |> 
  ggplot(aes(x = rmi)) +
  
  # data in background
  geom_point(
    data = performance_and_rmi, 
    aes(y = true_time), 
    alpha = .6, color = clrs[4], shape = 1
  ) +
  
  # error ribbon
  geom_ribbon(
    aes(ymin = Q2.5, ymax = Q97.5), 
    fill = clrs[5], alpha = .5
  ) +
  
  # mean line
  geom_line(
    aes(y = Estimate), 
    color = clrs[5]
  ) +
  
  labs(y = 'Time (s)') +
  
  project_theme() +
  
  facet_wrap(~level)

# regularization seems to temper the relationship between rmi and time for certain
# levels, which is okay! should signal improved generalization

# posterior predictive check 
# posterior predict adds variance from shape and sigma[pars] and can be used
# to generate model derived data to compare to actual observed data
yrep <- posterior_predict(b4.1, ndraws = 500)

# ppc 
ppc_dens_overlay(performance_and_rmi$true_time, yrep[1:100,]) +
  project_theme()

# looks great

# try by level
ppc_dens_overlay_grouped(
  performance_and_rmi$true_time, yrep[1:50,], group = performance_and_rmi$level
) + 
  project_theme()

# looks good again, model appears to struggle a bit on level 9

# final model: in-game time * rmi predicting performance ------------------

# plot out raw data
performance_and_rmi |> 
  mutate(
    rmi_binned = case_when(
      rmi < .1 ~ .1, 
      rmi < .2 & rmi >= .1 ~ .2, 
      rmi < .3 & rmi >= .2 ~ .3,
      rmi < .4 & rmi >= .3 ~ .4,
      rmi < .5 & rmi >= .4 ~ .5, 
      rmi < .6 & rmi >= .5 ~ .6, 
      rmi < .7 & rmi >= .6 ~ .7, 
      rmi < .8 & rmi >= .7 ~ .8,
      rmi > .8 ~ .9
    )
  ) |> 
  group_by(rmi_binned, level_order) |> 
  mutate(
    mean_time = mean(log(true_time)), 
    count = n()
  ) |> 
  ungroup() |> 
  ggplot() +
  geom_tile(
    aes(x = rmi_binned, y = level_order, fill = mean_time)
  ) +
  facet_wrap(~level)

# a little tough to see, but looks like some relationship between the two esp
# for level 2 and 6 where experience and routineness allow players to capitalize
# on the ideal structure of the environment for traplining but should also
# be wary of limited sampling of these high performers

# by modeling both at the same time, can also control for any confounding influence
# of learning with rmi
performance_and_rmi |> 
  ggplot() +
  geom_point(
    aes(x = level_order, y = rmi), shape = 1, color = clrs[3], alpha = .4
  ) +
  geom_smooth(
    aes(x = level_order, y = rmi), color = clrs[3], se = F, method = 'lm'
  ) +
  facet_wrap(~level)

# positive relationships between experience and rmi on some levels but not others (6, 7, 3, 10)

# look at the priors
get_prior(
  data = performance_and_rmi, 
  true_time ~ 1 + s_rmi * s_time_in_game + (s_rmi * s_time_in_game | level) + (s_rmi * s_time_in_game | subject), 
  family = Gamma(link = 'log')
) |> 
  View()

# couple of additional priors for main effect of level order and also interaction
# at population and group levels
b4.1 <- brm(
  data = performance_and_rmi, 
  family = Gamma(link = 'log'), 
  true_time ~ 1 + s_rmi * s_time_in_game + (s_rmi * s_time_in_game | level) + 
    (s_rmi * s_time_in_game | subject), 
  prior = c(
    prior(normal(5, 2), class = Intercept), # intercept prior is on log scale
    prior(normal(0, 3), class = b), # rmi prior
    prior(exponential(1), class = sd), # variances
    prior(gamma(.1, .1), class = shape) # narrower shape prior
  ), 
  iter = 4000, warmup = 2000, chains = 4, cores = 4, 
  control = list(adapt_delta = .95), # avoid divergence
  sample_prior = T,
  seed = 888, 
  file = 'R/fits/b4.1'
)

b4.1 <- read_rds('R/fits/b4.1.rds')

print(b4.1)

# parameter summaries
as_draws_df(b4.1) |> 
  select(starts_with('b_')) |> 
  pivot_longer(everything()) |> 
  group_by(name) |> 
  mean_hdi()

b4.1$prior

# rmi still appears to improve performance even after conditioning on time in game
# and by groups like subjects and levels

# compare fixed estimates
as_draws_df(b4.1) |> 
  select(starts_with('b_s')) |> 
  set_names(str_c("b[", c("RMI", "Time", "`RMI x Time`"), "]")) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(
    aes(x = value, y = fct_reorder(name, value))
  ) +
  stat_halfeye(
    fill = clrs[3], point_interval = 'mean_hdi'
  ) +
  geom_vline(xintercept = 0, alpha = .5) +
  scale_y_discrete(labels = ggplot2:::parse_safe) +
  labs(y = 'parameter') +
  project_theme()

ggsave(
  paste0(fig_path, 'b4.1_beta_slabs.png'), device = 'png', 
  width = 6, height = 5, units = 'in'
)

# look at variances for groups, then look into marginal effects, then ppc and 
# maybe waic comp between 3.1 and 4.1
as_draws_df(b4.1) |> 
  select(starts_with('sd_')) |> 
  set_names(str_c("sigma[", c('level[intercept]', 'level[rmi]', 'level[time]', 'level[ixn]', 'subject[intercept]', 'subject[rmi]', 'subject[time]', 'subject[ixn]'), "]")) |> 
  pivot_longer(everything()) |> 
  
  # plot
  ggplot(
    aes(x = value, y = fct_reorder(name, value))
  ) +
  stat_halfeye(fill = clrs[2]) +
  scale_y_discrete(labels = ggplot2:::parse_safe) +
  xlim(0, NA) +
  labs(y = 'parameter') +
  project_theme()

ggsave(
  paste0(fig_path, 'b4.1_variance_slabs.pdf'), device = 'pdf', 
  width = 6, height = 5, units = 'in'
)

# marginal of rmi
nd <- tibble(
  s_rmi = seq(min(performance_and_rmi$s_rmi), max(performance_and_rmi$s_rmi), length.out = 100), 
  s_time_in_game = 0
)

f <- fitted(
  b4.1, 
  newdata = nd, 
  re_formula = NA, 
  probs = c(.025, .1, .9, .975)
) |> 
  as_tibble() |> 
  bind_cols(nd)

# me
marginal_f <- predictions(
  b4.1, 
  newdata = datagrid(
    s_rmi = seq(min(performance_and_rmi$s_rmi), max(performance_and_rmi$s_rmi), length.out = 30), 
    s_time_in_game = 0
  ), 
  re_formula = NA
)

marginal_f |>
  ggplot(aes(x = s_rmi)) +
  
  # data in background
  geom_point(
    data = performance_and_rmi, 
    aes(y = true_time), 
    alpha = .4, shape = 1, color = clrs[3]
  ) +
  
  # ribbon
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high), 
    alpha = .4, fill = clrs[2]
  ) +
  
  # means
  geom_line(
    aes(y = estimate), 
    color = clrs[2], linewidth = 1
  ) +
  
  scale_x_continuous(n.breaks = 10, labels = seq(0, 1, by = .1)) +
  
  labs(x = 'RMI', y = 'Time (s)', caption = 'Ribbon represents 95% C.I.') +
  
  project_theme()

ggsave(
  paste0(fig_path, 'b4.1_grand_rmi_marginal.png'), device = 'png', 
  width = 6, height = 5, units = 'in'
)

# a lot of the variance is captured and held in the random effects, need to 
# show this with some plots like the spaghetti plots and rmi by level

# rmi by level
marginal_f <- predictions(
  b4.1, 
  newdata = datagrid(
    s_rmi = seq(min(performance_and_rmi$s_rmi), max(performance_and_rmi$s_rmi), length.out = 30), 
    s_time_in_game = 0, 
    level = 1:10
  ), 
  re_formula = ~ (s_rmi | level) # average over subjects to estimate level effects
)

marginal_f |> 
  mutate(
    level_string = fct_reorder(str_c('level ', level), level)
  ) |> 
  ggplot(aes(x = s_rmi)) +
  
  # points in background
  geom_point(
    data = performance_and_rmi |> mutate(level_string = fct_reorder(str_c('level ', level), level)), 
    aes(y = true_time), 
    alpha = .4, shape = 1, color = clrs[3]
  ) + 
  
  # ribbons
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high), 
    alpha = .4, fill = clrs[2]
  ) +
  
  # lines
  geom_line(
    aes(y = estimate), 
    color = clrs[2]
  ) +
  
  scale_x_continuous(n.breaks = 5, labels = seq(0, 1, .2)) +
  
  labs(x = 'RMI', y = 'Time (s)', caption = 'Ribbons represent 95% C.I.') +
  
  project_theme() +
  
  theme(
    axis.text.x = element_text(size = 8, angle = 25)
  ) +
  
  facet_wrap(~ level_string)

ggsave(
  paste0(fig_path, 'b4.1_rmi_marginal_by_level.png'), device = 'png', 
  width = 6, height = 6, units = 'in'
)

performance_and_rmi |> 
  ggplot(aes(x = rmi)) +
  geom_density(fill = clrs[4]) +
  geom_vline(aes(xintercept = mean(rmi)), linetype = 3) +
  project_theme() +
  facet_wrap(~level)

# now subject spaghetti
marginal_f <- predictions(
  b4.1, 
  newdata = datagrid(
    s_rmi = seq(min(performance_and_rmi$s_rmi), max(performance_and_rmi$s_rmi), length.out = 20), 
    s_time_in_game = 0, 
    level = 1:10, 
    subject = unique(performance_and_rmi$subject)
  ), 
  re_formula = NULL, # use everything
  vcov = F
)

marginal_f |> 
  mutate(
    level_string = fct_reorder(str_c('level ', level), level)
  ) |> 
  ggplot(aes(x = s_rmi, y = estimate, color = as.factor(subject))) +
  
  # plot spaghetti
  geom_line(
    linewidth = .3, alpha = .4
  ) + 
  
  scale_x_continuous(n.breaks = 10, labels = seq(0, 1, .1)) +
  scale_color_viridis_d(guide = 'none', option = 'magma') +
  
  project_theme() +
  
  labs(x = 'RMI', y = 'Time (s)') +
  
  theme(
    axis.text.x = element_text(size = 8, angle = 90)
  ) +
  
  facet_wrap(~ level_string)

ggsave(
  paste0(fig_path, 'b4.1_rmi_spaghetti.pdf'), device = 'pdf', 
  width = 6, height = 6, units = 'in'
)
  

# marginal effects --------------------------------------------------------

b4.1 <- read_rds('R/fits/b4.1.rds')

# calculate ame of rmi

# marginal slopes of rmi
avg_slopes(b4.1, variables = 's_rmi') # one unit change at the mean of s_rmi

(rmi_ame <- avg_slopes(
  b4.1, 
  newdata = datagrid(
    level = 1:10, 
    subject = unique(performance_and_rmi$subject)
  ), 
  re_formula = NULL
))

# one unit (sd) change of rmi at the mean results in a 2.62s (95% C.I. = [0.86, 4.41])
# improvement in performance after accounting for group differences. 

# one level change in the middle of the game results in a 0.58s (95% C.I. = [-1.03, 2.15])
# improvement in performance after accounting for group differences. 

# get mean prediction
(rmi_predictions <- avg_predictions(
  b4.1
))

# average time to complete level was 161s (95% C.I. = [160, 163]) after accounting
# for group differences. 

# get slope changes at different values of srmi
srmi25 = quantile(performance_and_rmi$s_rmi, .25)
srmi50 = quantile(performance_and_rmi$s_rmi, .50)
srmi75 = quantile(performance_and_rmi$s_rmi, .75)

# slope draws
rmi_slopes <- slopes(
  b4.1, 
  newdata = datagrid(
    level = 1:10
  ), 
  re_formula = ~(s_rmi | level)
) |> 
  posterior_draws()

rmi_slopes |> 
  filter(term == 's_rmi') |> 
  mutate(
    level_string = fct_reorder(str_c('level ', level), level)
  ) |> 
  ggplot(aes(x = draw)) +
  stat_halfeye(
    fill = clrs[2], point_interval = 'median_hdi', .width = c(.8, .95)
  ) +
  geom_vline(xintercept = 0, alpha = .7) +
  labs(
    x = 'Posterior slope estimate', 
    y = 'Density'
  ) +
  scale_x_continuous(n.breaks = 7) +
  project_theme() +
  facet_wrap(~level_string)

ggsave(
  paste0(fig_path, 'b4.1_post_slope_level.pdf'), device = 'pdf', width = 8, height = 6, 
  units = 'in'
)


# model comparison --------------------------------------------------------

# get model fits
b3.1 <- read_rds('R/fits/b3.1.rds')
b4.1 <- read_rds('R/fits/b4.1.rds')

# add loo
b3.1 <- add_criterion(b3.1, 'loo')
b4.1 <- add_criterion(b4.1, 'loo')

# compare 
(loo_modelcomp <- loo_compare(b3.1, b4.1, criterion = 'loo'))

# 3rd model without level order and ixn fits better, which makes sense given that
# experience in game didn't appear to affect performance 

# rmi models --------------------------------------------------------------

# see how rmi varies by in-game experience
performance_and_rmi |> 
  ggplot(aes(x = level_order, y = rmi)) +
  geom_point(shape = 1, color = clrs[5]) +
  geom_smooth(color = clrs[4], method = 'lm', se = F) +
  project_theme() +
  facet_wrap(~level)

# use beta distribution
get_prior(
  data = performance_and_rmi, 
  formula = rmi ~ 1 + level_order + (level_order | subject) + (level_order | level), 
  family = beta_binomial()
) |> 
  View()

b_r.2 <- brm(
  data = performance_and_rmi, 
  formula = bf(
    rmi ~ 1 + s_time_in_game + (s_time_in_game | subject) + (s_time_in_game | level), 
    phi ~ 1 + s_time_in_game + (s_time_in_game | subject) + (s_time_in_game | level)
  ), 
  prior = c(
    prior(normal(.5, .3), class = 'Intercept'),
    prior(normal(0, 3), class = 'b'), 
    prior(exponential(1), class = 'sd')
  ), 
  family = Beta(), 
  chains = 4, iter = 4000, warmup = 2000, cores = 4, 
  control = list(adapt_delta = .95), # avoid divergence
  sample_prior = T,
  backend = 'cmdstanr',
  seed = 888, 
  file = 'R/fits/b_r.2'
)

print(b_r.2)

# summary
as_draws_df(b_r.2) |> 
  select(starts_with('b_')) |> 
  pivot_longer(everything()) |> 
  group_by(name) |> 
  mean_hdi()

avg_slopes(
  b_r.2, 
  newdata = datagrid(
    level = 1:10, 
    subject = unique(performance_and_rmi$subject)
  ), 
  re_formula = NULL
)

(level_slopes <- slopes(
  b_r.2, 
  newdata = datagrid(
    level = 1:10
  ), 
  re_formula = ~(s_time_in_game | level)
))

subj_slopes <- slopes(
  b_r.2, 
  newdata = datagrid(
    subject = unique(performance_and_rmi$subject)
  ), 
  re_formula = ~(s_time_in_game | subject)
)

subj_slopes |> 
  arrange(desc(estimate)) |> 
  View()

# good convergence

# ppc
yrep <- posterior_predict(b_r.2, ndraws = 1000)

# ppc 
ppc_dens_overlay(performance_and_rmi$rmi, yrep[sample(1:1000, 100),]) +
  scale_color_manual('', values = c(clrs[5], '#bedded')) +
  ylab('density') +
  scale_x_continuous('rmi', breaks = seq(0, 1, .2), limits = c(0, 1)) +
  project_theme()

ggsave(
  'fig_output/participants/models/br.2_ppc_overall.png', device = 'png', 
  width = 6, height = 4, units = 'in'
)

# try by level
ppc_dens_overlay_grouped(
  performance_and_rmi$rmi, yrep[sample(1:1000, 50),], group = performance_and_rmi$level_string
) + 
  scale_color_manual('', values = c(clrs[5], '#bedded')) +
  ylab('density') +
  scale_x_continuous('rmi', breaks = seq(0, 1, .2), limits = c(0, 1)) +
  project_theme() +
  theme(
    axis.text.x = element_text(size = 8, angle = 25)
  )

ggsave(
  'fig_output/participants/models/br.2_ppc_levels.png', device = 'png', 
  width = 6, height = 6, units = 'in'
)

# get coef draws in long form
b_r.2 |> 
  as_draws_df() |> 
  select(starts_with('b_')) |> 
  set_names(
    str_c(
      "b[", 
      c('Intercept', 'phi[Intercept]', '`Time in game`', 'phi[`Time in game`]'),
      "]")
  ) |> 
  pivot_longer(everything()) |>
  mutate(
    par = if_else(
      str_detect(name, 'phi'), 'precision', 'mean'
    )
  ) |> 
  
  ggplot(aes(x = value, y = name)) +
  
  stat_halfeye(fill = clrs[5], size = 1.5) +
  
  geom_vline(xintercept = 0, linetype = 3) +
  
  scale_y_discrete('parameter', labels = ggplot2:::parse_safe) +
  
  project_theme() +
  
  facet_wrap(~par, scales = 'free')

ggsave(
  paste0(fig_path, 'br.2_beta_slabs.png'), device = 'png', 
  width = 6, height = 3, units = 'in'
)

b_r.2 |> 
  as_draws_df() |> 
  select(starts_with('sd_')) |> 
  set_names(
    str_c(
      "sd[", 
      c(
        'level[intercept]', 'level[`time in game`]', 
        'subject[intercept]', 'subject[`time in game`]',
        'phi[level[intercept]]', 'phi[level[`time in game`]]',
        'phi[subject[intercept]]', 'phi[subject[`time in game`]]'
      ),
      "]"
    )
  ) |> 
  pivot_longer(everything()) |>
  mutate(
    par = if_else(
      str_detect(name, 'phi'), 'precision', 'mean'
    )
  ) |> 
  
  ggplot(aes(x = value, y = name)) +
  
  stat_halfeye(fill = clrs[5], size = 1.5) +
  
  geom_vline(xintercept = 0, linetype = 3) +
  
  scale_y_discrete('parameter', labels = ggplot2:::parse_safe) +
  
  project_theme() +
  
  facet_wrap(~par, scales = 'free')

ggsave(
  paste0(fig_path, 'br.2_variance_slabs.png'), device = 'png', 
  width = 6, height = 3, units = 'in'
)

# what does this positive mean (time in game) coef mean?
# me
marginal_f <- predictions(
  b_r.2, 
  newdata = datagrid(
    s_time_in_game = scale(1:10)[,1]
  ),
  re_formula = NA
)

# collapse across subj and level
marginal_f |>
  ggplot(aes(x = s_time_in_game)) +
  
  # data in background
  geom_point(
    data = performance_and_rmi, 
    aes(y = rmi), 
    alpha = .4, shape = 1, color = clrs[5]
  ) +
  
  # ribbon
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high), 
    alpha = .4, fill = clrs[4]
  ) +
  
  # means
  geom_line(
    aes(y = estimate), 
    color = clrs[4], linewidth = 1
  ) +
  
  scale_x_continuous(breaks = scale(1:10)[,1], labels = seq(1, 10, 1)) +
  
  labs(x = 'Time in game (level)', y = 'RMI') +
  
  project_theme()

ggsave(
  paste0(fig_path, 'br.2_conditional_overall.png'), device = 'png', 
  width = 6, height = 4, units = 'in'
)

# now by level
marginal_f <- predictions(
  b_r.2, 
  newdata = datagrid(
    s_time_in_game = scale(1:10)[,1], 
    level = 1:10
  ),
  re_formula = ~(s_time_in_game | level)
)

# plot
marginal_f |>
  mutate(
    level_string = fct_reorder(factor(str_c('level ', level)), level)
  ) |> 
  ggplot(aes(x = s_time_in_game)) +
  
  # data in background
  geom_point(
    data = performance_and_rmi, 
    aes(y = rmi), 
    alpha = .4, shape = 1, color = clrs[5]
  ) +
  
  # ribbon
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high), 
    alpha = .4, fill = clrs[4]
  ) +
  
  # means
  geom_line(
    aes(y = estimate), 
    color = clrs[4], linewidth = 1
  ) +
  
  scale_x_continuous(breaks = scale(1:10)[,1], labels = seq(1, 10, 1)) +
  
  labs(x = 'Time in game (level)', y = 'RMI') +
  
  project_theme() +
  
  theme(
    axis.text.x = element_text(size = 8, angle = 25)
  ) +
  
  facet_wrap(~level_string)

ggsave(
  paste0(fig_path, 'br.2_conditional_by_level.png'), device = 'png', 
  width = 6, height = 6, units = 'in'
)

# mean differences in rmi across levels
conditional_f <- predictions(
  b_r.2, 
  newdata = datagrid(
    level = 1:10
  ), 
  re_formula = ~(1 | level)
)

conditional_f |> 
  ggplot(aes(x = factor(level))) + 
  
  geom_col(aes(y = estimate), fill = clrs[5]) + 
  
  geom_point(
    data = performance_and_rmi, 
    aes(x = level, y = rmi), 
    position = position_jitterdodge(.2), 
    shape = 1, 
    color = clrs[4], 
    alpha = .3
  ) +
  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) + 
  
  labs(
    x = 'level', 
    y = 'RMI'
  ) +
  
  project_theme()

ggsave(
  paste0(fig_path, 'br.2_mean_rmi_by_level.png'), device = 'png', 
  width = 6, height = 4, units = 'in'
)

