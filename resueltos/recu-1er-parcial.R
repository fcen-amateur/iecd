# Ej. 2
f_X <- function(t, tita=3, n=20) {2*n / tita * (t/tita)^(2*n-1)}
f_X(0)
F_X <- function(t, tita=3, n=20) {(t/tita)^(2*n)}
F_X(1)

0.05^(1/10) * (5.6^2)

# Ej. 4
# Realizar boxplots paralelos para la variable northing basado en los datos registrados segu ́n haya presencia o ausencia de ranas en el lugar. A partir de estos plots describir la distribucio ́n de esta variable en cada uno de los dos tipos de lugar (con ranas y sin ellas) y compararlas. (No ma ́s de 4 renglones).
# Muky dice que el gra ́fico muestra que los lugares con ranas tienden a estar a menos metros al norte del punto de referencia que los que no tienen ranas. ¿Esta ́ de acuerdo con Muky? Justificar en no ma ́s de 3 renglones.
librarian::shelf("tidyverse")
df <- read_csv("ranas.csv")
df
help(boxplot)
boxplot(northing ~ pres.abs, df)
plot(density(df$northing[df$pres.abs == 1]), col="red")
lines(density(df$northing[df$pres.abs == 0]), col="blue")
legend("topright", c("presencia", "ausencia"), col=c("red", "blue"), lty=1) 
# Poco ilustrativo el boxplot muchos "outliers"

plot(ecdf(df$meanmin[df$pres.abs == 1]), col="red")
lines(ecdf(df$meanmin[df$pres.abs == 0]), col="blue")
legend("topleft", c("presencia", "ausencia"), col=c("red", "blue"), lty=1) 

plot(ecdf(df$meanmax[df$pres.abs == 1]), col="red")
lines(ecdf(df$meanmax[df$pres.abs == 0]), col="blue")
legend("topleft", c("presencia", "ausencia"), col=c("red", "blue"), lty=1) 

plot(density(df$avrain[df$pres.abs == 0]), col="blue")
lines(density(df$avrain[df$pres.abs == 1]), col="red")
legend("topright", c("presencia", "ausencia"), col=c("red", "blue"), lty=1) 
table(df$pres.abs)
boxplot(avrain ~ pres.abs, df)
quantile(df$avrain[df$pres.abs == 1])
s