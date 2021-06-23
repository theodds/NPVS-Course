## Load ----

options(java.parameters = "-Xmx4g")
library(bartMachine)
library(caret)
library(tidyverse)

rmse <- function(x,y) sqrt(mean((x-y)^2))

## Examine the Boston dataset ----

?MASS::Boston
boston <- MASS::Boston
View(MASS::Boston)

## Split into training and test sets ----

set.seed(774837)
folds <- caret::createFolds(1:nrow(MASS::Boston), k = 5)
boston_train <- MASS::Boston[-folds[[1]],]
boston_test  <- MASS::Boston[folds[[1]],]

## Fit linear model and BART to training set ----

lm_boston <- lm(medv ~ ., data = boston_train)
fitted_bart <- bartMachine(X = boston_train %>% select(-medv),
                           y = boston_train$medv, num_trees = 200, 
                           num_burn_in = 4000, 
                           num_iterations_after_burn_in = 4000, 
                           seed = 112231)

## Evaluate on testing set ----

cor(boston_test$medv, predict(fitted_bart, boston_test %>% 
                                select(-medv)))
cor(boston_test$medv, predict(lm_boston, boston_test))

## Looking at variable selection on full dataset and visualizing ----

bart_boston <- bartMachine(X = boston %>% select(-medv), y = boston$medv, 
                           num_trees = 40, num_burn_in = 5000, 
                           num_iterations_after_burn_in = 5000,
                           serialize = TRUE, seed = 77777)
var_props <- get_var_props_over_chain(bart_boston)

ggplot() + geom_col(aes(x = names(var_props), y = var_props)) + theme_bw() + 
  xlab("Variable") + ylab("Importance") + ylim(0, max(var_props))
