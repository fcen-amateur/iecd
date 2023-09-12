library(fitdistrplus)

graph_par <- function(){
  par(family = "Verdana", cex.axis=0.7, cex.lab=0.7,
      mgp=c(1.1,0.25,0), tcl=0)
}
graph_par()

f <- function(datos,alfa){
  n <- length(datos)
  salida <- n*log(alfa/mean(datos))+sum(log(datos))-n*digamma(alfa)
  return(salida)
}
fprima <- function(datos,alfa){
  n <- length(datos)
  salida <- n/alfa-n*trigamma(alfa)
  return(salida)
}

momgamma <- function(datos){
	n <- length(datos) 
  	mediaDatos <- mean(datos)
  	sig2Datos <- (n-1)/n*var(datos)
  	lambdaMom <- mediaDatos/sig2Datos
  	alfaMom <- mediaDatos^2/sig2Datos
	c(alfaMom,lambdaMom)
}

emvgamma <- function(datos,I){
  ## estimadores de momentos como punto inicial del algoritmo N-R
  n <- length(datos) 
  mediaDatos <- mean(datos)
  sig2Datos <- (n-1)/n*var(datos)
  lambdaMom <- mediaDatos/sig2Datos
  alfaMom <- mediaDatos^2/sig2Datos
  a <- numeric(I+1)
  a[1] <- alfaMom
  for(i in 1:I){
    a[i+1] <- a[i]-f(datos,a[i])/fprima(datos,a[i]) ## hay dificultades en esta l?nea, da negativo a veces
  }
  lambda <- a[I+1]/mean(datos)
  salida <- c(a[I+1],lambda,f(datos,a[I+1]))
  return(salida)
}

enes <- c(6,10,20,40,80,200)
N <- 1000
alfaM <- matrix(NA,N,6)
lambdaM <- matrix(NA,N,6)
alfaMV <- matrix(NA,N,6)
lambdaMV <- matrix(NA,N,6)
j <- 1
for(n in enes){
	for (i in 1:N){
		set.seed(i)
		misDatos <- rgamma(n,3,4)
		momentos <- momgamma(misDatos)
		EMV <- emvgamma(misDatos,10)
		alfaM[i,j] <- momentos[1]
		lambdaM[i,j] <- momentos[2]
		alfaMV[i,j] <- EMV[1]
		lambdaMV[i,j] <- EMV[2]
	}
	j <- j+1
}

sum(alfaM=="NaN")
sum(lambdaM=="NaN")
sum(alfaMV=="NaN")
sum(lambdaMV=="NaN")
set.seed(206)
misDatos <- rgamma(6,3,4)
momentos <- momgamma(misDatos)
EMV <- fitdist(misDatos,distr="gamma", method = 'mle')
EMV$estimate

par(mfrow=c(1,3))
caso <- 5
#alpha
{
  hist(alfaM[,caso],probability=TRUE, 
       main = paste('n=',enes[caso]),
       xlab = 'Momentos',
       xlim = c(0,30),)
  hist(alfaMV[,caso],probability=TRUE,
       main = paste('n=',enes[caso]),
       xlab = 'Maxima verosimilitud',
       xlim = c(0,30))
  boxplot(alfaM[,caso],alfaMV[,caso])
  abline(a=3,b=0,col=2)
  
}

#lambda
{
  hist(lambdaM[,caso],probability=TRUE, 
       main = paste('n=',enes[caso]),
       xlab = 'Momentos',
       xlim = c(0,30),)
  hist(lambdaMV[,caso],probability=TRUE,
       main = paste('n=',enes[caso]),
       xlab = 'Maxima verosimilitud',
       xlim = c(0,30))
  boxplot(lambdaM[,caso],lambdaMV[,caso])
  abline(a=3,b=0,col=2)
  
}

#Errores cuadraticos
par(mfrow=c(1,1))
ECMalfaM <- apply((alfaM-3)^2,2,mean)
ECMalfaMV <- apply((alfaMV-3)^2,2,mean)
ECMalfaMV[1] <- mean((alfaMV[-which(alfaMV[,1]=="NaN"),1]-3)^2)
ECMalfaMV[2] <- mean((alfaMV[-which(alfaMV[,2]=="NaN"),2]-3)^2)
ECMlambdaM <- apply((lambdaM-4)^2,2,mean)
ECMlambdaMV <- apply((lambdaMV-4)^2,2,mean)
ECMlambdaMV[1] <- mean((lambdaMV[-which(lambdaMV[,1]=="NaN"),1]-4)^2)
ECMlambdaMV[2] <- mean((lambdaMV[-which(lambdaMV[,2]=="NaN"),2]-4)^2)

plot(enes,ECMalfaM,ylim=c(0,110),col=2,
     pch=20, ylab='ECM', main = 'Alfa',
     xlab = 'Tamaño de muestra')
points(enes,ECMalfaMV,col=3, pch=20)

plot(enes[3:6],ECMalfaM[3:6],ylim=c(0,5),col=2,
     pch=20, ylab='ECM', main = 'Alfa (zoom)',
     xlab = 'Tamaño de muestra')
points(enes,ECMlambdaMV,col=3, pch=20)

plot(enes,ECMlambdaM,ylim=c(0,110),col=2,
     pch=20, ylab='ECM', main = 'Lambda',
     xlab = 'Tamaño de muestra')
points(enes,ECMlambdaMV,col=3, pch=20)

plot(enes[3:6],ECMlambdaM[3:6],ylim=c(0,5),col=2,
     pch=20, ylab='ECM', main = 'Lambda (zoom)',
     xlab = 'Tamaño de muestra')
points(enes,ECMlambdaMV,col=3, pch=20)
