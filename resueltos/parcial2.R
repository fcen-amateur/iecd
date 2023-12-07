setwd("~/Git/iecd")
library(tidyverse)
library(broom)

df1 <- datums::bordeaux %>%
  mutate(lnprice = log(price)) %>%
  drop_na

df2 <- read_csv("wine")

plot(df1$price, df2$Price)
plot(df1$lnprice, df2$Price)
plot(df1$wrain, df2$WinterRain)
df <- bind_cols(df1, df2)
cor(df) %>% round(3)
pairs(df)
df[5:8,]

summary(lm(lnprice ~ AGST, df))
summary(lm(Price ~ AGST, df))

mod1 <- lm(Price ~ AGST, df2)
df2 <- augment(mod1, df2)
n <- dim(df2)[1]
p1 <- 2. # intercept + AGST
dim(df2)
sqrt(sum((df2$.resid)^2)/(n - p1))
summary(mod1)

X1 <- model.matrix(~ AGST, df)
sqrt(solve(t(X1)%*%X1)[2,2]) * 0.4819

mod2 <- lm(Price ~ AGST + HarvestRain, df2)
X2 <- model.matrix(~ AGST + HarvestRain, df2)
sigmahat_psihat <- solve(t(X2)%*%X2)[2:3, 2:3] * 0.3707**2
psihat <- c(0.6298525, -0.0042511)
beta_2 = 0 versus beta_2 != 0summary(mod2)
df2 <- augment(mod2, df2)
sum(df2$.resid**2)


sigmahat_psihat <- solve(t(X2)%*%X2)[2:3, 2:3] * 0.3707**2
psihat <- c(0.6298525, -0.0042511)
Fobs <- t(psihat) %*% solve(sigmahat_psihat) %*% psihat / 2
Fobs
summary(mod2)

anova(mod1, mod2)
qt(0.025, df = 24, lower.tail=F)

