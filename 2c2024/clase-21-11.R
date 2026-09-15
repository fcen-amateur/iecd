class(1:5)
x <- 1:5
class(x)
help(attr)
attr(x, "class") <- "perro"
class(x)

y <- structure(6:10, class="gato")
class(y)

nuevo_perro <- function(nombre) {
  structure(TRUE, nombre=nombre, class="perro")
}
fido <- nuevo_perro("Fido")

X <- rnorm(1000)


plot(density(X))
methods(plot)

library(sloop)
s3_dispatch(plot(1:5))

plot.integer <- function(vec) { barplot(height=vec) }
notas <- as.integer(c(Juan=4, Noelia=2, Carla=9, Maru=11))
plot(notas)
s3_dispatch(plot(notas))

class(t.test)
class(binom.test(10, 20, 0.9))
class(wald.test(X))

class(unclass(unclass(test_t)))

stopifnot(unclass(test_t) == unclass(unclass(test_t)))

list(a=1) == list(b=3)
stopifnot(seq(2, 10, by=2) / 2 == 1:5)
stopifnot(T)
stopifnot(1)
stopifnot(as.logical(1))
stopifnot(as.character(T))
