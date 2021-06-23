## Load ----

options(java.parameters = "-Xmx5g")
library(bartMachine)
library(tidyverse)
library(nonlinvarsel)

boston <- MASS::Boston

## Fit ----

fit_bart <- bartMachine(X = boston %>% select(-medv),
                        y = boston %>% pluck("medv"),
                        num_trees = 200,
                        num_burn_in = 5000,
                        num_iterations_after_burn_in = 5000,
                        seed = 111211211)

## Variable select ----

var_sel <- nonlinvarsel::vsa(X = boston %>% select(-medv), 
                             fhat = fit_bart$y_hat_train)

plot(var_sel)