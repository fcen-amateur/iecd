

particiones <- function(t, n){
  if(n==0) {
    if(t ==0){return(1)} 
    else{return(0)}
  }
  if (t<0 || t>(n*(n + 1))/2){return(0)}
  
  return((particiones(t, n-1) + particiones(t -n, n-1)))
}




dTmas <-function(x,n ){
  res<-vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    t <-x[i]
    res[i]<- (particiones(t, n))/(2**n)
  }
  return(res)
  }


pTmas<-function(x , n){
  res<- vector(mode = "numeric", length = length(x))
  for (i in seq_along(x)) {
    t <-x[i]
    hasta<-0:t
    res[i]<-sum(sapply(hasta, function(j) (particiones(j, n))/(2**n)))
  }
  return(res)
}

mi.wilcox.test <-function(x,alternative,mu){
  
  x<-x-mu
  
  rangos <-rank(abs(x) ) 
  Tmas <-sum(rangos[x>0])
  
  n <-length(x)
  
  alternative <- match.arg(alternative,choices = c("two.sided" ,"greater","less"))
  pval <-0
  
  pmenor<- pTmas(Tmas, n)
  pmayor<- 1-pTmas(Tmas-1,n)
  
  if(alternative =="less"){
    pval<- pmenor}  
  else if (alternative=="greater"){
    pval<- pmayor} 
  else if (alternative== "two.sided"){
    pval<- 2*min(pmenor, pmayor)} 
  
  res<- list(
    statistic = setNames(Tmas,"V"),
    p.value = pval,
    alternative = alternative,
    data.name = "X"
    
  )
  structure(res,class="htest")
  
}





#########################################################################################################
#  este if (FALSE) es para que no se corra todo lo que esta abajo
#########################################################################################################
if (FALSE) {

## Ej 17
graficar_distribucion <- function(n){
  #rango de T+
  Tmas_vals<- 0:(n*(n+1)/2)
  
  #dist exacta
  prob_exacta <- sapply(Tmas_vals, function(t) dTmas(t, n))
  
  #defino mu y sigma (sigma con el ej 16)
  mu <-(n*(n+1))/4
  sigma <- sqrt((n)*(n+1)*((2* n)+1)/24)
  
  # Valores de la ditribucion asintotica
  dist_asintotica <- dnorm(Tmas_vals,mean =mu,sd=sigma)
  #dist_asintotica <- dnorm(Tmas_vals - 0.5*n, mean = mu, sd = sigma) #descomentar esta linea para correr los graficos con las correcciones
  
  barplot(prob_exacta, 
          names.arg = Tmas_vals, 
          main = paste("Distribución real vs. asintótica de T+ con n =", n),
          xlab = "T+",
          ylab = "Probabilidad",
          ylim=c(0, max(dist_asintotica) + max(dist_asintotica)/5))
  
  lines(Tmas_vals, dist_asintotica, col = "deeppink", lwd=2)
  legend("topleft", legend = c("Distribución Exacta", "Distribucion asintótica"), 
         col = c("gray", "deeppink"), lty = c(NA, 1), lwd = c(NA, 2), 
         pch = c(15, NA), pt.cex = 2, bty = "n")
}

par(mfrow = c(1, 3)) 
graficar_distribucion(4)
graficar_distribucion(10)
graficar_distribucion(20)








########## Ejercicio 18

set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
tests <- rep(NA,10000)
x <- rnorm(n,mean=theta1,sd=sqrt(sigma_sq))

plot(x)
#No param
for (i in 1:10000){
  muestra <-sample(x,12,replace=TRUE) #Hacemos bootstrap tomando partes de la muestra generada anteriormente
  test <-wilcox.test(muestra, alternative ="greater",mu = 0,conf.level= 0.950)
  alpha<- 0.05
  phi_w<- ifelse(test$p.value <= alpha, 1, 0)
  tests[i] <- phi_w
}

cat("Potencia de test no param=",mean(tests))

#Param
set.seed(1984)
for (i in 1:10000){
  muestra <-rnorm(n,mean=theta1,sd=sqrt(sigma_sq)) #Hacemos bootstrap con la distribucion que nos dan
  test <-wilcox.test(muestra, alternative ="greater",mu = 0,conf.level= 0.950)
  alpha<- 0.05
  phi_w<- ifelse(test$p.value <= alpha, 1, 0)
  tests[i] <- phi_w
}

cat("Potencia de test param=",mean(tests))









######## Ejercicio 19


###########
set.seed(1984)
n <- 12
theta1 <- 1
sigma_sq <- 1
X <- rnorm(n, mean = theta1, sd = sqrt(sigma_sq))
##########


resul <- t.test(X, alternative = "greater", mu = 0)
print(resul)

potencia_t <- power.t.test(n = n, delta = theta1, sd = sqrt(sigma_sq), 
                           sig.level = 0.05, type = "one.sample", 
                           alternative = "one.sided")
cat("Potencia analítica del test t:", round(potencia_t$power, 3), "\n")



alpha <- 0.05

# Potencia analítica de test t
potencia <- 1 - pt(qt(1-alpha, df=n-1), df = n-1, ncp = sqrt(n)*(theta1-0)/sqrt(sigma_sq))
cat("Potencia analítica del test t:", potencia, "\n")

#o tambien

z<-((qt(1-alpha, df=n-1)*sigma_sq/sqrt(n))-1)*sqrt(n)
1-pnorm(z)

#test phi s
test.signo <- function(x, mu, alternative, alpha) {
  x <-x -mu
  Ti<- ifelse(x>0, 1, 0)
  Smas<-sum(Ti)
  n <-length(x)
  
  if (alternative == "greater"){
    p_val <- 1-pbinom(Smas -1, size=n, prob=0.5)} 
  else if (alternative == "less"){
    p_val <- pbinom(Smas, size = n, prob = 0.5)}
  
  list(statistic = Smas, p.value = p_val, alternative = alternative)
}



#No param
for (i in 1:10000){
  muestra <-sample(x,12,replace=TRUE) #Hacemos bootstrap tomando partes de la muestra generada anteriormente
  test <-test.signo(muestra, alternative ="greater",mu = 0, 0.05)
  alpha<- 0.05
  phi_w<- ifelse(test$p.value <= alpha, 1, 0)
  tests[i] <- phi_w
}

cat("Potencia de test no param=",mean(tests))


#Param
set.seed(1984)
for (i in 1:10000){
  muestra <-rnorm(n,mean=theta1,sd=sqrt(sigma_sq)) #Hacemos bootstrap con la distribucion que nos dan
  test <-test.signo(muestra, alternative ="greater",mu = 0, 0.05)
  alpha<- 0.05
  phi_w<- ifelse(test$p.value <= alpha, 1, 0)
  tests[i] <- phi_w
}

cat("Potencia de test param=",mean(tests))

}

