setwd("~/Git/iecd")
df <- read.csv("1-2.csv")
plot(df$PA, df$PE)
library(tidyverse)
library(broom)
# attach(df)
lin <- lm(PE ~ PA, df)
summary(lin)
broom::glance(lin)
tidy(lin)
plot(lin)

lin2 <- lm(PE ~ PA, df[-12,])
summary(lin2)
glance(lin2)
tidy(lin2)
plot(lin2)

T0 <- 212
P0 <- 29.92123
df["invPEc"] <- df$PE^-1 - 1 / T0
df["lnPAc"] <- log(df$PA) - log(P0)

fis <- lm(invPEc ~ lnPAc + 0, df)
summary(fis)
glance(fis)
tidy(fis)
plot(fis)
  
fis2 <- lm(invPEc ~ lnPAc + 0, df[-12,])
summary(fis2)
glance(fis2)
tidy(fis2)
plot(fis2)

inHg_a_Pa <- function(x) {x*3386.39}
fahrenheit_a_kelvin <- function(x) { (x - 32)*5/9 + 273.15 }
P0 <- 101325 # Pa, presión atmosférica de referencia
T0 <- 373.15 # K, temperatura de ebulicion del agua a P0
df["PE_K"] <- fahrenheit_a_kelvin(df$PE)
df["PA_Pa"] <- inHg_a_Pa(df$PA)
df["invPE_Kc"] <- df$PE_K^-1 - 1 / T0
df["lnPA_Pac"] <- log(df$PA_Pa) - log(P0)

fis3 <- lm(invPE_Kc ~ lnPA_Pac + 0, df[-12,])
summary(fis3)
glance(fis3)
tidy(fis3)
plot(fis3)


  library(ggplot2)
df["invPE"] <- df$PE^-1
df["lnPA"] <- log(df$PA)
m2 <- lm(invPE ~ lnPA, df)
summary(m2)
linbis <- lm(PE ~ PA, df[-12,])
summary(linbis)
m2bis <- lm(invPE ~ lnPA, df[-12,])
summary(m2bis)
r1bis <- df$PE - predict(linbis, df)
r2bis <- df$PE - 1 / predict(m2bis, df)
plot(df$PA, r2bis, col="green")
points(df$PA, r1bis)


library(tidyverse)

inHg_a_Pa <- function(x) {x*3386.39}
fahrenheit_a_kelvin <- function(x) { (x - 32)*5/9 + 273.15 }
P0 <- 101325 # Pa, presión atmosférica de referencia
T0 <- 373.15 # K, temperatura de ebulicion del agua a P0
# T0 <- 212
# P0 <- 29.92123
R <- 8.3145 # J/(K*mol), constante de los gases ideales
L <- 2256000 # kJ/kg, entalpia de vaporizacion del agua
mH2O <- 0.018 # kg/mol, masa del agua
Lmol <- L * mH2O
P0 <- 101325 # Pa, presión atmosférica de referencia
T0 <- 373.15 # K, temperatura de ebulicion del agua a P0
b <- R*log(P0)/Lmol+1/T0
m <- -R/(Lmol)

df2 <- read_csv("1-2.csv")

df2 <- df2 %>%
  mutate(
    id = seq_along(PA),
    PE = fahrenheit_a_kelvin(PE),
    PA = inHg_a_Pa(PA),
    invPE = 1/PE,
    lnPA = log(PA),
    invPEc = invPE - 1 / T0,
    lnPAc = lnPA - log(P0)) 

modelos <- list(
  lineal = PE ~ PA,
  fisico = invPE ~ lnPA,
  fis_cen = invPEc ~ lnPAc + 0)

datos <- list(
  "con outlier" = df2,
  "sin outlier" = df2[-12,])
resumen <- crossing(modelos, datos) %>%
  mutate(
    nombre_modelo = c(rep("lineal", 2), rep("fisico", 2), rep("fis_cen", 2)),
    outliers = rep(c("con outlier", "sin outlier"), 3),
    llamada_lm = map2(modelos, datos, lm),
    resumen = map(llamada_lm, broom::glance),
    # b = map(llamada_lm, "coefficients") %>% map_dbl(1),
    # m = map(llamada_lm, "coefficients") %>% map_dbl(2),
    R2_ajustado = map_dbl(resumen, "adj.r.squared") %>% signif(4),
    p_valor = map_dbl(resumen, "p.value") %>% signif(4),
    fstat = map_dbl(resumen, "statistic") %>% signif(4))

resumen %>%
  select(nombre_modelo, outliers, R2_ajustado, p_valor, fstat) %>%
  knitr::kable(digits=30)

predicciones <- resumen %>%
  mutate(
    ajuste = map(llamada_lm, augment),
    PA = map(datos, "PA"),
    PE = map(datos, "PE"),
    PEhat = map(ajuste, ".fitted")) %>%
  select(nombre_modelo, outliers, PA, PE, PEhat) %>%
  unnest(c(PA, PE, PEhat)) %>%
  mutate(
    PEhat = ifelse(nombre_modelo=="fisico", 1/PEhat, PEhat),
    PEhat = ifelse(nombre_modelo=="fis_cen", 1/(PEhat + 1 / T0), PEhat),
    resid = PE - PEhat)

predicciones %>%
  ggplot(aes(PA, resid)) +
  geom_point() +
  facet_grid(outliers ~ nombre_modelo, scales = "free")

blin <- filter(resumen, outliers=="sin outlier", nombre_modelo=="lineal")$b
mlin <- filter(resumen, outliers=="sin outlier", nombre_modelo=="lineal")$m
bfis <- filter(resumen, outliers=="sin outlier", nombre_modelo=="fisico")$b
mfis <- filter(resumen, outliers=="sin outlier", nombre_modelo=="fisico")$m



range(df2$PA)

tibble(
  PA = seq(P0/3, 1.5*P0, 100),
  # PA = seq(5, 50, 0.1),
  lineal = blin + mlin * PA,
  fisico = 1/(bfis + mfis * log(PA))) %>%
  gather(modelo, PE, -PA) %>%
  ggplot(aes(PA, PE, color = modelo)) +
  geom_line() +
  geom_point(data = df2, inherit.aes = F, mapping = aes(PA, PE), shape = "x", size = 2)

T0 <- 212
P0 <- 29.92123

df2["Y"] <- df2$invPE - 1 / T0
df2["X"] <- df2$lnPA - log(P0)

fis2 <- lm(invPEc ~ lnPAc + 0, df2[-12,])
glance(fis2)
tidy(fis2)
plot(fis2)

augment(fis2)
