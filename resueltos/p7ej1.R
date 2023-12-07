# librarian: Install, Update, Load Packages from CRAN & others in one step
# https://cran.r-project.org/web/packages/librarian/index.html
install.packages("librarian")
librarian::shelf(tidyverse, glmnet, broom)

#### STOP ####
# Primero lea https://glmnet.stanford.edu/articles/glmnet.html
# (intro oficial a GLMNet, de los lindos de Friedman, Hastie, Tibshirani et al)
# al menos hasta "Linear Regression: family = 'gaussian'"
# (y recomiendo también "Assessing models on test data).
#### SIGA ####

n <- 100
generar_df <- function(n) {
  # Para repetir sistemáticamente mejor tener un "factory", que  cree el df.
  # Las funciones de R devuelven el último objeto evaluado por defecto.
  tibble(
    x1 = runif(n, 0, 5),
    x2 = runif(n, 0, 5),
    x3 = 2 * x1 + runif(n,-0.1, 0.1),
    x4 = -x1 + runif(n,-0.1, 0.1),
    eps = rnorm(n, 0, 1),
    y = 5 + 8 * x1 - 5 * x2 + x3 + 4 * x4 + eps
  )
}
df <- generar_df(n)

formula.lm <- y ~ x1 + x2 + x3 + x4
formula.glmnet <-
  update.formula(formula.lm, . ~ . + 0)  # Quito el intercept
X <- model.matrix(formula.glmnet, df)
y <- df$y
X %>% as_tibble %>% head

# Ajusto una vez cada algoritmo
fit.minsq <- lm(formula.lm, df)
fit.lasso <- cv.glmnet(X, y, alpha = 1)
fit.ridge <- cv.glmnet(X, y, alpha = 0)

# Num coef significativo
alfa <- 0.05
coef.minsq <- tidy(fit.minsq)
sum(coef.minsq$p.value < alfa)

# En ridge, digamos > 1e-5 apra distinguir de cero
eps <- 1e-5
coef.ridge <- coef(fit.ridge, s = "lambda.1se")
sum(abs(coef.ridge) > eps)

# Num coef nonzero p/ lambda
coef.lasso <- coef(fit.lasso, s = "lambda.1se")
sum(coef.lasso != 0)

# Junto los coeficientes para comparar
coefs <- tibble(
  term = coef.minsq$term,
  minsq = coef.minsq$estimate,
  lasso = as.vector(coef.lasso),
  ridge = as.vector(coef.ridge)
)
colnames(coefs) <- c("term", "minsq", "lasso", "ridge")
coefs

# Ahora, repetimos `nsims=1000` veces y calculamos el desvio estandar empiricamente
nsims <- 1000
all.coefs <- list()
all.signifs <- list()
for (i in 1:nsims) {
  print(i)
  df <- generar_df(n)
  X <- model.matrix(formula.glmnet, df)
  y <- df$y
  
  fit.minsq <- lm(formula.lm, df)
  fit.lasso <- cv.glmnet(X, y, alpha = 1)
  fit.ridge <- cv.glmnet(X, y, alpha = 0)
  
  coef.minsq <- tidy(fit.minsq)
  coef.ridge <- coef(fit.ridge, s = "lambda.1se")
  coef.lasso <- coef(fit.lasso, s = "lambda.1se")
  
  coefs <- tibble(
    term = coef.minsq$term,
    minsq = coef.minsq$estimate,
    lasso = as.vector(coef.lasso),
    ridge = as.vector(coef.ridge)
  )
  colnames(coefs) <- c("term", "minsq", "lasso", "ridge")
  all.coefs[[i]] <- coefs
  
  # Num coef significativo
  signifs <- c(
    minsq = sum(coef.minsq$p.value < alfa),
    ridge = sum(abs(coef.ridge) > eps),
    lasso = sum(coef.lasso != 0)
  )
  all.signifs[[i]] <- signifs
}

all.signifs <- bind_rows(all.signifs)
colMeans(all.signifs)

all.coefs <- bind_rows(all.coefs, .id = "nsim") %>%
  mutate(nsim = strtoi(nsim))
all.coefs %>%
  group_by(term) %>%
  summarise(
    mean.minsq = mean(minsq),
    std.minsq = sd(minsq),
    mean.lasso = mean(lasso),
    std.lasso = sd(lasso),
    mean.ridge = mean(ridge),
    std.ridge = sd(ridge)
  )
