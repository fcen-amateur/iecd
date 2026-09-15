objs <- list(mtcars, 1:5, sum, lm(mpg ~ cyl, mtcars), t.test)
for (obj in objs) { print(class(obj)) }

mu <- 1
sigma_sq <- 1
n <- 30
set.seed(1957)
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))
# Ejecuto el test
mu_0 <- 0
alfa <- 0.05
test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu0,
  conf.level = 1 - alfa
)




mi.t.test <- function(x, mu0 = 0, alfa = 0.05) {
  n <- length(x)
  parameter <- n - 1
  estimate <- mean(x)
  stderr <- sd(x) / sqrt(n)
  statistic <- (estimate - mu0) / stderr
  conf.int <- estimate + qt(c(alfa / 2, 1 - alfa / 2), df = parameter) * stderr
  p.value.izq <- pt(statistic, df = parameter)
  p.value <- 2 * min(p.value.izq, 1 - p.value.izq)
  list(
    parameter=parameter,
    estimate=estimate,
    stderr=stderr,
    statistic=statistic,
    conf.int=conf.int,
    p.value=p.value
  )
}

mi.t.test <- function(x, mu = 0, conf.level = 0.95) {
  stopifnot(is.numeric(x))
  stopifnot(is.scalar(mu))
  stopifnot(is.scalar(conf.level), (conf.level > 0), (conf.level < 1))
  alfa <- 1 - conf.level
  n <- length(x)
  rv <- list(
    parameter = c(df = n - 1),
    estimate = c(`mean of x` = mean(x)),
    stderr = sd(x) / sqrt(n),
    null.value = c(mean = mu),
    alternative = "two.sided",
    method = "One Sample t-test",
    # Stack Overflow: How to convert variable (object) name into String
    # https://stackoverflow.com/a/14577878
    data.name = deparse(substitute(x))
  )
  rv$statistic <- setNames((rv$estimate - mu) / rv$stderr, "t")
  rv$conf.int <- rv$estimate + qt(c(alfa / 2, 1 - alfa/2), df = rv$parameter) * rv$stderr
  attr(rv$conf.int, "conf.level") <- conf.level
  pval_izq <- pt(rv$statistic, df = rv$parameter)
  rv$p.value <- 2 * min(pval_izq, 1 - pval_izq)
  structure(rv, class = "htest")
}

(R_test_t <- t.test(X))
(mi_test_t <- mi.t.test(X))

# ======= VIEJO ======== #
install.packages("sloop")
library(sloop)

otype(4)
otype(t.test)

model <- lm(mpg ~ cyl, mtcars)
c(otype(lm), class(lm))
c(otype(model), class(model))
otype(tidyr::tibble)
otype(tidyr::tibble(mtcars))
class(mtcars)
class(tidyr::tibble(mtcars))
otype(broom::glance(model))
otype(mtcars)
# A base object:
is.object(1:10)
#> [1] FALSE
sloop::otype(1:10)
#> [1] "base"

# An OO object
is.object(mtcars)
#> [1] TRUE
sloop::otype(mtcars)
#> [1] "S3"
otype(data.frame(mtcars))
otype(mpg ~ cyl)
class(mpg ~ cyl)
otype(lm(mpg ~ cyl, mtcars))

f <- factor(c("a", "b", "c"))

typeof(f)
#> [1] "integer"
attributes(f)
#> $levels
#> [1] "a" "b" "c"
#>
#> $class
#> [1] "factor"
#>
unclass(f)
#> [1] 1 2 3
#> attr(,"levels")
#> [1] "a" "b" "c"
#>
print.tdh <-
  function(tdh)
    print(c("Impresion especial para tests", tdh))
print.tdh("ququ")

frase <- "Abarajame la bañera"
print(frase)
help(attr)

# {X}_n=10 de una Normal(mu, 1). Test para mu = 0 vs. mu != 0
mu0 <- 1
sigma_sq <- 1
alfa <- 0.05
X <- rnorm(30, mean = mu, sd = sqrt(sigma_sq))
test_t <- t.test(X,
                 alternative = "two.sided",
                 mu = mu0,
                 conf.level = 1 - alfa)
suelto <- unclass(test_t)

perro <- function(nombre) {
  structure(list(nombre = nombre, ladridos = 0), class = "perro")
}
ladrar <- function(perro, ladridos = 1) {
  perro$ladridos = perro$ladridos + ladridos
  print(strrep("Guau!", ladridos))
  return(perro)
}
print.perro <-
  function(perro) {
    paste("Guau! Me llamo",
          perro$nombre,
          "y he ladradado",
          perro$ladridos,
          "veces.")
  }
rocco <- new_perro("Rocco")
jauria = apply(c("Guason", "Bobby", "Conan"), 0, perro)
rocco <- ladrar(rocco, 5)
print(rocco)


print(rocco)
print(molly)

# test t,  alt=two.sided
is.scalar <- function(x)
  is.numeric(x) & length(x) == 1L

mi.t.test <- function(x, mu0 = 0, alfa = 0.95) {
  n <- length(x)
  parameter <- n - 1
  estimate <- mean(x)
  stderr <- sd(x) / sqrt(n)
  statistic <- (estimate - mu0) / stderr
  conf.int <- estimate + qt(c(alfa / 2, 1 - alfa / 2), df = parameter) * stderr
  p.value.izq <- pt(statistic, df = parameter)
  p.value <- 2 * min(p.value.izq, 1 - p.value.izq)
  list(
    parameter=parameter,
    estimate=estimate,
    stderr=stderr,
    statistic=statistic,
    conf.int=conf.int,
    p.value=p.value
  )
}

(R_test_t <- t.test(X))
(mi_test_t <- mi.t.test(X))

(R_test_t <- t.test(X))
(mi_test_t <- mi.t.test(X))
mi.t.test <- function(x, mu = 0, conf.level = 0.95) {
  stopifnot(is.numeric(x))
  stopifnot(is.scalar(mu))
  stopifnot(is.scalar(conf.level), (conf.level > 0), (conf.level < 1))
  alfa <- 1 - conf.level
  n <- length(x)
  rv <- list(
    parameter = c(df = n - 1),
    estimate = c(`mean of x` = mean(x)),
    stderr = sd(x) / sqrt(n),
    null.value = c(mean = mu),
    alternative = "two.sided",
    method = "One Sample t-test",
    # Stack Overflow: How to convert variable (object) name into String
    # https://stackoverflow.com/a/14577878
    data.name = deparse(substitute(x))
  )
  rv$statistic <- setNames((rv$estimate - mu) / rv$stderr, "t")
  rv$conf.int <- rv$estimate + qt(c(alfa / 2, 1 - alfa/2), df = rv$parameter) * rv$stderr
  attr(rv$conf.int, "conf.level") <- conf.level
  pval_izq <- pt(rv$statistic, df = rv$parameter)
  rv$p.value <- 2 * min(pval_izq, 1 - pval_izq)
  structure(rv, class = "htest")
  
}
(R_t_test <- t.test(X))
(mi_t_test <- mi.t.test(X))

stopifnot(capture.output(R_t_test) == capture.output(mi_t_test))

binom.test(sum(X < 0), length(X))

list(a=4, b = a -5)
