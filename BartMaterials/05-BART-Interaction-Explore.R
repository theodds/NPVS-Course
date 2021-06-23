## Packages ----

options(java.parameters = "-Xmx5g")
library(bartMachine)
library(BART)
library(tidyverse)

## Load Data ----

boston <- MASS::Boston

## Fit a BART ----

boston_bart <- bartMachine(X = boston %>% select(-medv), 
                           y = boston$medv,
                           num_trees = 50,
                           num_burn_in = 4000,
                           num_iterations_after_burn_in = 4000,
                           seed = digest::digest2int("bart interact"))

## Do Interaction Detection ----

boston_interact <- interaction_investigator(
  bart_machine = boston_bart,
  num_var_plot = 10,
  num_replicates_for_avg = 25
)