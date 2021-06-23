## Load ----

options(java.parameters = "-Xmx4g")
library(dartMachine)
library(caret)
library(tidyverse)

rmse <- function(x,y) sqrt(mean((x-y)^2))
boston <- MASS::Boston

## Fit DART ----

boston_dart <- bartMachine(X = boston %>% select(-medv), y = boston$medv, 
                           alpha_0 = 1.6, do_ard = TRUE, do_prior = FALSE,
                           serialize = TRUE, seed = 4744777)

## Plot Variable Importance ----

boston_dart_props <- get_var_props_over_chain(boston_dart)
ggplot() + geom_col(aes(x = names(boston_dart_props), y = boston_dart_props)) + 
  theme_bw()

## Plot Inclusion Probabilities ----

var_counts <- get_var_counts_over_chain(boston_dart)
probs      <- colMeans(var_counts > 0)
ggplot() + geom_col(aes(x = names(boston_dart_props), y = probs)) + 
  theme_bw()

## Comparing Goodness of Fit on train/test split for BART and DART ----

set.seed(774837)

folds <- caret::createFolds(1:nrow(boston), k = 5)

boston_train <- boston[-folds[[1]],]
boston_test  <- boston[folds[[1]],]

lm_boston <- lm(medv ~ ., data = boston_train)
cor(boston_test$medv, predict(lm_boston, boston_test))

fitted_dart <- bartMachine(X = boston_train %>% select(-medv),
                           y = boston_train$medv, num_trees = 200, 
                           num_burn_in = 4000, num_iterations_after_burn_in = 4000, 
                           alpha_0 = 1.6, do_ard = TRUE, do_prior = FALSE, 
                           seed = 112231)

fitted_bart <- bartMachine(X = boston_train %>% select(-medv),
                           y = boston_train$medv, num_trees = 200, 
                           num_burn_in = 4000, num_iterations_after_burn_in = 4000, 
                           seed = 112231)


cor(boston_test$medv, predict(fitted_dart, boston_test %>% select(-medv)))
cor(boston_test$medv, predict(fitted_bart, boston_test %>% select(-medv)))


colMeans(get_var_counts_over_chain(fitted_dart) > 0)
colMeans(get_var_counts_over_chain(fitted_bart) > 0)

## What if we eliminate the variables DART does not like? ----

fitted_subset <- bartMachine(
  X = boston_train %>% select(-medv, -chas, -zn),
  y = boston_train$medv, 
  num_trees = 200, 
  num_burn_in = 4000, 
  num_iterations_after_burn_in = 4000, 
  seed = 112231)

mu_hat_subset <- predict(
  fitted_subset, boston_test %>% select(-medv, -chas, -zn)
)

cor(boston_test$medv, mu_hat_subset)