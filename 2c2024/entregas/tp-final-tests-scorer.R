n <- 15
t <- 34

puntaje <-
  setNames(vector("numeric", 3),
           c("particiones", "[d|p]Tmas", "mi.wilcox"))
# particiones
puntaje["particiones"] <- sum(
  c(
    particiones(t = 3, n = 4) == 2,
    particiones(t = 24, n = 12) == 67,
    particiones(t = 55, n = 10) == 1,
    particiones(t = 45, n = 30) == 1938,
    particiones(t, n) == particiones(n * (n + 1) / 2 - t, n),
    all(sapply(0:8, function(t) {
      particiones(t, n)
    }) == c(1, 1, 1, 2, 2, 3, 4, 5, 6)),
    all(sapply(0:2, function(t) {
      particiones(t, 1024)
    }) == 1),
    all(sapply(32:34, function(t) {
      particiones(t, 11)
    }) == c(69, 70, 69))
  )
)


# dTmas, pTmas
puntaje["[d|p]Tmas"] <- sum(c(
  dTmas(24, 12) == 67 / 2 ^ 12,
  all(dTmas(0:10, 4) == c(1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1) / 16),
  sum(dTmas(0:21, 6)) == 1,
  all(dTmas(0:2, 55) == 2 ^ -55),
  dTmas(t, n) == dTmas(n * (n + 1) / 2 - t, n),
  pTmas(t, n) == 1 - pTmas(n * (n + 1) / 2 - (t + 1), n),
  all(pTmas(c(13, 21, 8, 5, 3), 13) * 2 ^ 13 == c(88, 386, 25, 10, 5)),
  all(pTmas(t:(t + 5), n) >= pTmas(t:(t+5), (n+1)))
))

# mi.wilcox.test
score_test <- function(
  rdist,
  n,
  mu = 0,
  seed=NULL,
  alternative = c("two.sided", "less", "greater"),
  mi.test=mi.wilcox.test,
  R.test=wilcox.test
) {
  X <- rdist(n)
  alternative <- match.arg(alternative)
  R_wilcox <- R.test(X, alternative = alternative, mu = mu)
  mi_wilcox <- mi.test(X, alternative = alternative, mu = mu)
  print(mi_wilcox)
  print(R_wilcox)
  if  (all(
    identical(as.numeric(unname(mi_wilcox$statistic)), unname(R_wilcox$statistic)),
    identical(mi_wilcox$alternative, R_wilcox$alternative),
    identical(class(R_wilcox), class(mi_wilcox))
  )) {
    if (isTRUE(all.equal(mi_wilcox$p.value, R_wilcox$p.value))) {
      return(5)
    } else {
      return(1.5)
    }
  } else {return(0)}
}

set.seed(7645)
n <- 13
X <- rcauchy(n)
theta0 <- 0
alternative <- "two.sided"
score_wilcoxon <- c(
  greater=score_test(rnorm, 11, -1, 1234, "greater"),
  less=score_test(function(n) { runif(n, -5, 0) }, 14, -1, 7645, "less"),
  two.sided=score_test(rcauchy, 13, 0, 7645, "two.sided")
)
puntaje["mi.wilcox"] <- ceiling(sum(score_wilcoxon))
print(score_wilcoxon)
print(puntaje)
