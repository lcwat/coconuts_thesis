# loo model comparison between the heuristic computational models

# load libraries ----------------------------------------------------------

library(tidyverse)
library(loo)

# load objects ------------------------------------------------------------

loos <- read_rds('R/cmdstan_output/model_loos_final.rds')

# model comparison --------------------------------------------------------

(comp <- loo_compare(loos))

loos$nn
loos$ta
loos$clst
loos$nn_ta
loos$nn_clst
loos$nn_ta_clst

# all pareto ks look good, nn model is closest to zero in terms of elpd

# compute loo weights using model stacking
(weights <- loo_model_weights(loos, method = 'pseudobma'))

# models using distance outperform others, but particularly the nn ta model seems
# to fit performance the best, nn clst was the second best

# may be worthwhile to fit the final full model, clst is very close to zero, but 
# enough to improve the predictive accuracy.

# update: it does improve prediction accuracy according to elpd loo! quite handily 
# outperforms all other models 

# Method: stacking
# ------
#   weight
# nn         0.000 
# ta         0.001 
# clst       0.003 
# nn_ta      0.000 
# nn_clst    0.049 
# nn_ta_clst 0.947 