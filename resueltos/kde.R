#  KDE: Estimación de densidad por núcleos en R
ind <- function(x, min, max) {
  (min <= x) & (x <= max)
}

triangular <- function(x) {
  (1 - abs(x)) * ind(x, -1, 1)
}

xs <- seq(from = -2, to = 2, length.out = 200)

triangular(xs)
plot(xs, triangular(xs), type = "l")

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
  plot(xs, nucleos[[nombre]](xs), type = "l")
  # curve(K, from = -2, to = 2)
}

attach(cars)
dens <- density(speed)
dens(xs)

kde <- function(xs, datos, h = 1, nucleo = "gaussiano") {
  n <- length(datos)
  m <- length(xs)
  K <- nucleos[[nucleo]]
  fs <- vector(length=m)
  for (i in seq.int(m)) {
    fs[i] <- sum(K((xs[i] - datos) / h))
  }
  fs / (n * h)
}

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
