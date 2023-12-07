library(tidyverse)
# install.packages("glmnet")
library(glmnet)
library(broom)
n <- 100

make_df <- function(n) {
  tibble(
    x1 = runif(n, 0, 5),
    x2 = runif(n, 0, 5),
    x3 = 2*x1 + runif(n, -0.1, 0.1),
    x4 = -x1  + runif(n, -0.1, 0.1),
    eps = rnorm(n, 0, 1),
    y = 5 + 8*x1 - 5*x2 + x3 + 4*x4 + eps
  )
}
form <- y ~ x1 + x2 + x3 + x4
df <- make_df(n)
X <- select(df, x1, x2, x3, x4). # %>% as.matrix
y <- df$y
summary(lm(form, df))
# alpha == 0 Ridge puro; alpha == 1 Lasso puro.
lasso <- glmnet(X, y, family="gaussian", alpha=1)
summary(lasso)
tidy(lasso) %>% filter(step==100)
coef(lasso)
lasso$lambda
# tibble(coef(lasso))
cv.lasso <- cv.glmnet(X, y, family="gaussian", alpha=0)
names(cv.lasso)
summary(cv.lasso)
nreps <- 3
for (i in 1:nreps) {
  df <- make_df(n)
  X <- model.matrix(form, df)
  y <- df$y
  modelos <- list(
    minsq = lm(form, df),
    # X ya tiene la columna del intercept
    ridge = glmnet(X[,2:5], y, family="gaussian", alpha=0, nlambda=500),  # Ridge
    lasso = glmnet(X[,2:5], y, family="gaussian", alpha=1, nlmabda=500)  # Lasso
  )
} 

fit <- modelos$ridge
coefs <- coef(fit)
# Predict the response variable
y_pred <- predict(fit, newx = x, s = 0.1)  # adjust s (lambda) as needed

# Calculate the residuals
residuals <- y - y_pred

# Calculate the RSS
RSS <- sum(residuals^2)

# Calculate the degrees of freedom
df <- length(y) - length(coef(fit, s = 0.1))  # adjust s (lambda) as needed

# Estimate the variance
variance <- RSS / df