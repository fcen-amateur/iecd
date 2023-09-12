#  KDE: Estimación de densidad por núcleos en R
ind <- function(x, min, max) {
  (min <= x) & (x <= max)
}

triangular <- function(x) {
  (1 - abs(x)) * ind(x, -1, 1)
}

xs <- seq(from = -1.5, to = 1.5, length.out = 200)

triangular(xs)
plot(xs, triangular(xs), type = "l")
curve(triangular, from=-2, to=2)

gaussiano <- function(x) {
  dnorm(x, mean = 0, sd = 1)
}
plot(xs, gaussiano(xs), type = "l")

rectangular <- function(x) {
  ind(x, -1, 1) / 2
}
plot(xs, rectangular(xs), type = "l")

epanechnikov <- function(x) {
  3 / 4 * (1 - x**2) * ind(x, -1, 1)
}
plot(xs, epanechnikov(xs), type = "l")

nucleos <- list(
  triangular = triangular,
  gaussiano = gaussiano,
  rectangular = rectangular,
  epanechnikov = epanechnikov
)

par(mfrow = c(2, 2))

for (nombre in names(nucleos)) {
  K <- nucleos[[nombre]]
  plot(xs, K(xs), type = "l", main=nombre)
  # curve(K, from = -2, to = 2)
}

hacer_kde <- function(datos, h = 1, nucleo = "gaussiano") {
  kde <- function(xs) {
    n <- length(datos)
    K <- nucleos[[nucleo]]
    m <- length(xs)
    fs <- vector(length=m)
    for (i in seq.int(m)) {
      fs[i] <- sum(K((xs[i] - datos) / h)) / (n * h)
    }
    fs
  }
  return(kde)
}

mykde(xs)
head(cars)
attach(cars)
cars
dens <- density(speed, bw=1)
mykde <- hacer_kde(speed, 1, "gaussiano")
mykde(dens$x)
dens(xs)
plot(dens)
plot(dens$x, mykde(dens$x), type="l")
speed_range <- range(speed)
step <- 0.5
xs <- seq(speed_range[1], speed_range[2], step)
rdens <- density(speed, bw=1)
mydens <- kde(xs, speed)
par(mfrow=c(1,2))
plot(xs, mydens, type="l")
plot(rdens)
dens_range <- range(rdens$x)
xs <- seq(dens_range[1], dens_range[2], step)

plot(xs, kde(xs, speed), type="l")
plot(rdens)
