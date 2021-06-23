## Load ----

library(tidyverse)
library(BART)

## Functions ----

f_fried <- function(x) 10 * sin(pi * x[,1] * x[,2]) + 
  20 * (x[,3] - 0.5)^2 + 
  10 * x[,4] + 5 * x[,5]

gen_data <- function(n_train, n_test, P, sigma) {
  X <- matrix(runif(n_train * P), nrow = n_train)
  mu <- f_fried(X)
  X_test <- matrix(runif(n_test * P), nrow = n_test)
  mu_test <- f_fried(X_test)
  Y <- mu + sigma * rnorm(n_train)
  Y_test <- mu_test + sigma * rnorm(n_test)
  
  return(list(X = X, Y = Y, mu = mu, X_test = X_test,
              Y_test = Y_test, mu_test = mu_test))
}

## Simulate dataset ----

set.seed(1234)
sim_data <- gen_data(250, 100, 1000, 1)

## Metric for evaluating goodness ----

rmse <- function(x,y) sqrt(mean((x - y)^2))

## FB Variable Selection with Dirichlet prior ----

set.seed(digest::digest2int("Using BART package"))

fit_bart <- wbart(x.train = sim_data$X, y.train = sim_data$Y, 
                  ndpost = 4000, nskip = 4000)
fit_dart <- wbart(x.train = sim_data$X, y.train = sim_data$Y, 
                  sparse = TRUE, ndpost = 4000, nskip = 4000)

## Collecting predictions ----

predicted_bart <- predict(fit_bart, sim_data$X_test)
predicted_dart <- predict(fit_dart, sim_data$X_test)
mu_hat_bart <- colMeans(predicted_bart)
mu_hat_dart <- colMeans(predicted_dart)

## Out-of-sample RMSE for the two methods ----

rmse(mu_hat_bart, sim_data$mu_test)
rmse(mu_hat_dart, sim_data$mu_test)

## Figure: accuracy of predictions ----

df_for_plot <- data.frame(mu = c(sim_data$mu_test, sim_data$mu_test),
                          mu_hat = c(mu_hat_bart, mu_hat_dart),
                          method = rep(c("BART", "DART"), 
                                       each = length(sim_data$mu_test)))

qf <- function(x) quantile(x, c(0.025, 0.975))
limits_bart <- t(apply(predicted_bart, 2, qf))
limits_dart <- t(apply(predicted_dart, 2, qf))

ggplot(df_for_plot, aes(x = mu, y = mu_hat)) + geom_point() + 
  geom_errorbar(aes(ymin = c(limits_bart[,1], limits_dart[,1]), 
                    ymax = c(limits_bart[,2], limits_dart[,2])), 
                color = "skyblue2", alpha = 0.5) + 
  facet_wrap(~method) + geom_abline(slope = 1, intercept = 0) + theme_bw()

## Figure: posterior inclusion probabilities ----

var_inc_df <- data.frame(
  inclusion_prob = colMeans(cbind(fit_bart$varcount, fit_dart$varcount) > 0),
  var            = rep(1:1000, 2),
  method         = rep(c("BART", "DART"), each = 1000)
)
var_inc_df$active = var_inc_df$var <= 5

ggplot(var_inc_df, aes(x = var, y = inclusion_prob, color = active)) + 
  geom_point(alpha=3) + facet_wrap(~method) + theme_bw()

## Zooming in ----

ggplot(var_inc_df, aes(x = var, y = inclusion_prob, color = active)) + 
  geom_point(alpha=3) + facet_wrap(~method) + theme_bw() + 
  xlim(0,20)
