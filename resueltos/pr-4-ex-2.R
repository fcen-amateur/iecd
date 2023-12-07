x <- c(500, 488, 426, 510, 450, 368, 508, 514, 426, 476, 512, 526, 444, 524, 236)

T1 <- function(x) { mean(x) }
T2 <- function(x) {( -1 + sqrt(1 + 4 * mean(x^2))) / 2}
lambda_m1 <- T1(x)
lambda_m2 <- T2(x)
set.seed(2023)
B <- 1000
muestra_T1_nopa <- vector("numeric", B)
muestra_T2_nopa <- vector("numeric", B)
muestra_T1_para <- vector("numeric", B)
muestra_T2_para <- vector("numeric", B)
for (i in 1:B) {
  x_nopa <- sample(x, size=length(x), replace=TRUE)
  x_para_T1 <- rpois(length(x), lambda_m1)
  x_para_T2 <- rpois(length(x), lambda_m2)
  muestra_T1_nopa[i] <- T1(x_nopa)
  muestra_T2_nopa[i] <- T2(x_nopa)
  muestra_T1_para[i] <- T1(x_para_T1)
  muestra_T2_para[i] <- T2(x_para_T2)
}

plot(density(muestra_T2_para), col="green", xlim=c(400, 520), ylim=c(0, 0.08))
lines(density(muestra_T2_nopa), col="red")
lines(density(muestra_T1_nopa), col="blue")
lines(density(muestra_T1_para), col="orange")

c(mean(muestra_T1_nopa), sd(muestra_T1_nopa))
c(mean(muestra_T2_nopa), sd(muestra_T2_nopa))
c(mean(muestra_T1_para), sd(muestra_T1_para))
c(mean(muestra_T2_para), sd(muestra_T2_para))

# IC cuantiles
alfa <- 0.05
quantile(muestra_T1_nopa, c(alfa / 2, 1 - alfa / 2))
quantile(muestra_T2_nopa, c(alfa / 2, 1 - alfa / 2))
# IC aprox normal
lambda_m1 + sd(muestra_T1_nopa) * qnorm(c(alfa / 2, 1 - alfa / 2))
lambda_m2 + sd(muestra_T2_nopa) * qnorm(c(alfa / 2, 1 - alfa / 2))

###### Cronometrando creacion de vectores ######
t0 <- proc.time()
res <- c()
for (i in 1:n) {
  # res <- c(res, runif(1))
  res[i] <- runif(1)
}
proc.time() - t0
mean(res)
sd(res)

n <- 1000000
t0 <- proc.time()
res <- vector("numeric", n)
for (i in 1:n) {
  res[i] <- runif(1)
}
proc.time() - t0
mean(res)
sd(res)


##### RNG
set.seed(2023)
sum(.Random.seed)
runif(1)
sum(.Random.seed)
