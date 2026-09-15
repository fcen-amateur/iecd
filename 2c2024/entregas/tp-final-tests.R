# particiones
stopifnot(
  particiones(t=3, n=4) == 2,
  particiones(t=24, n=12) == 67,
  particiones(t=55, n=10) == 1,
  particiones(t=45, n=30) == 1938
)

# dTmas, pTmas
n <- 15
t <- 34
stopifnot(
  dTmas(24, 12) == 67 / 2 ^ 12,
  dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16,
  sum(dTmas(0:21, 6)) == 1,
  dTmas(0:2, 55) == 2 ^ -55,
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n),
  pTmas(c(21, 13, 8, 5, 3), 13) * 2^13 == c(386, 88, 25, 10, 5)
)

# mi.wilcox.test
## test greater
set.seed(1234)
n <- 11
X <- rnorm(n)
theta0 <- -1
alternative <- "greater"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(unname(mi_wilcox$statistic), unname(R_wilcox$statistic)),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)


## test less
set.seed(7645)
n <- 14
X <- runif(n, -5, 0)
theta0 <- -1
alternative <- "less"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(unname(mi_wilcox$statistic), unname(R_wilcox$statistic)),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)

## test two.sided
set.seed(7645)
n <- 13
X <- rcauchy(n)
theta0 <- 0
alternative <- "two.sided"

R_wilcox <- wilcox.test(X, alternative=alternative, mu = theta0)
mi_wilcox <- mi.wilcox.test(X, alternative=alternative, mu = theta0)
stopifnot(
  identical(unname(mi_wilcox$statistic), unname(R_wilcox$statistic)),
  identical(mi_wilcox$alternative, R_wilcox$alternative),
  isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value)),
  identical(class(R_wilcox), class(mi_wilcox))
)
