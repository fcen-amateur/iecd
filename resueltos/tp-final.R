setwd("~/Git/iecd")
librarian::shelf("tidyverse", "ggplot2", "broom", "readxl", "glmnet")

# Usando un mecanismo aletorio se dividio ́ la muestra en dos partes:
# entrenamiento y testeo. En el archivo TrainTest.txt los TRUE’s representan los
# datos en la muestra de entrenamiento y los FALSE’s los datos en la muestra de
# testeo. Utilizando los datos de entrenamiento, ajuste un modelo lineal para
# WEIG basado en todas las variables explicativas.

df <- read_xls(
  "datos/body.xls",
  col_names = c(
    "BIAC","BIIL","BITRO","CHEST1","CHEST2","ELBOW","WRIST", "KNEE","ANKLE",
    "SHOUL","CHESTG","WAISTG","NAVEL","HIP","GLUTE","BICEP", "FLOREA","KNEEG",
    "CALF","ANKLEG","WRISTG","AGE","WEIG","HEIG","GEN"
  )
)
df %>% head
is_train <- read.table("datos/TrainTest.txt")[[1]]
# is_train <- runif(dim(df)[1]) > 0.8
sum(is_train)
stopifnot(length(is_train) == dim(df)[1])

formula.todas <- WEIG ~ .
modelo.todas <- lm(formula.todas, df[is_train,])
summary(modelo.todas)
tidy.todas <- tidy(modelo.todas)
# Criterio sencillo: mantener todas las variables significativas individualmente a nivel `alfa`
alfa <- 0.02
set.signif <- tidy.todas %>%
  filter((p.value < alfa) & (term != "(Intercept)")) %>%
  pluck("term")

formula.signif <- as.formula(paste0("WEIG ~ ", paste(set.signif, collapse=" + ")))
modelo.signif <- lm (formula.signif, df[is_train,])
tidy.signif <- tidy(modelo.signif)

bind_rows(glance(modelo.todas), glance(modelo.signif))
# Con la mitad de las variables, se alcanza casi exactamente el mismo R^2:
# pareciera ser que hay alta colinealidad entre ellas, y el subespacio que
# spanean las 24 es basicamente identico al que spanean las 12 significativas

# Calcule el error de prediccio ́n emp ́ırico del modelo ajustado en el grupo de testeo.
rmse <- function(y, y_pred) { sqrt(mean((y - y_pred) ** 2)) }
y_pred.todas <- predict(modelo.todas, df[!is_train,])
y_pred.signif <- predict(modelo.signif, df[!is_train,])
stopifnot(df$WEIG[!is_train] == df[!is_train,"WEIG"])
y <- df$WEIG
y_train <- y[is_train]
y_test <- y[!is_train]
rmses <- c(
  todas=rmse(y_test, y_pred.todas),
  signif=rmse(y_test, y_pred.signif)
)
X <- select(df, -c("WEIG"))
X_train <- X[is_train,]
X_test <- X[!is_train,]

lambdas <- 10**seq(1.1, -3.5, -0.05)
modelo.lasso <- glmnet(
  X_train,
  y_train,
  alpha=1,
  lambda=lambdas
)
preds.train <- predict(modelo.lasso, newx=as.matrix(X_train))
preds.test <- predict(modelo.lasso, newx=as.matrix(X_test))
lambdas <- modelo.lasso$lambda
nlambdas <- length(lambdas)
rmse.train <- vector("numeric", nlambdas)
rmse.test <- vector("numeric", nlambdas)

for (i in seq.int(nlambdas)) {
  rmse.train[i] <- rmse(y_train, predict(modelo.lasso, newx=as.matrix(X_train), s=lambdas[i]))
  rmse.test[i] <- rmse(y_test, predict(modelo.lasso, newx=as.matrix(X_test), s=lambdas[i]))
  
}
plot(log(lambdas), log(rmse.train), type="l", col="red")
lines(log(lambdas), log(rmse.test), col="blue")
idx.min <- which.min(rmse.test)
lambda.min <- lambdas[idx.min]
rmses["lasso.manual.min"] <- rmse.test[idx.min]
abline(v=log(lambda.min), lty=2)
text(x=log(lambda.min), y=2, labels=paste("lambda =", round(lambda.min, 4)))
legend("topleft", legend=c("train", "test"), col=c("red", "blue"), lty=1)

# -1 para eliminar `(Intercept)` de la lista de coeficientes
coefs.lasso <- as.matrix(coef(modelo.lasso))[-1,]
rownames(coefs.lasso)

par(mfrow=c(1,2))
plot(modelo.lasso, xvar="lambda")
range(log(lambdas))
range(coefs.lasso)
plot(
  x=range(log(lambdas)),
  y=range(coefs.lasso),
  xlab="Log Lambda",
  ylab="Coefficients",
  type="n"
)
for (var in rownames(coefs.lasso)) {
  lines(log(lambdas), coefs.lasso[var,])
}
ref.logs <- seq(-8, 2, 2)
text(
  x=ref.logs,
  y=-0.5,
  label=colSums(coef(modelo.lasso, s=exp(ref.logs)) != 0) - 1
)

#(i) modelo.lasso <- glmnet(X_train, y_train, alpha=1, lambda=lambdas)
# modelo.lasso.cv <- cv.glmnet(as.matrix(X_train), y_train, alpha=1, lambda=lambdas)
modelo.lasso.cv <- cv.glmnet(as.matrix(X_train), y_train, alpha=1)
lambdas <- modelo.lasso.cv$lambda
modelo.lasso.cv$lambda.1se
# - ¿Cómo resulta el modelo ajustado usando este valor de λ de un desv ́ıo standard?
# ¿Co ́mo se relaciona esta estimacio ́n de los coeficientes del modelo ajustado en eĺ ıtem (g))?
# Para "resetear" los parámetros gráficos, y no tener subplots
dev.off()
plot(modelo.lasso.cv)
modelo.lasso.cv[c("lambda.min", "lambda.1se")]

# - Calcule el error de clasificaci ́on emp ́ırico en el grupo de testeo.
#   Compare con el obtenido en el  ́ıtem (g)).
rmses["lasso.cv.min"] <- rmse(
  y_test,
  predict(modelo.lasso.cv, s="lambda.min", newx=as.matrix(X_test))
)
rmses["lasso.cv.1se"] <- rmse(
  y_test,
  predict(modelo.lasso.cv, s="lambda.1se", newx=as.matrix(X_test))
)
sort(rmses)
coef(modelo.lasso.cv, s="lambda.1se")

coefs.comp <- tidy.todas[c("term", "estimate")] %>%
  rename(todas = estimate) %>%
  left_join(
    tidy.signif[c("term", "estimate")] %>% rename(signif = estimate),
    by="term"
  ) %>%
  mutate(
    lasso.manual.min=na_if(coef(modelo.lasso, s=lambda.min)[,1], 0),
    lasso.cv.1se=na_if(coef(modelo.lasso.cv, s="lambda.1se")[,1], 0),
    lasso.cv.min=na_if(coef(modelo.lasso.cv, s="lambda.min")[,1], 0)
  )

coefs.comp %>% print(n=25)
bind_rows(rmses^2, colSums(!is.na(coefs.comp[, -1])))

### Coda - GEN como factor interaccionado
df$GEN <- as.factor(df$GEN)
summary(modelo.todas)
m1 <- lm(WEIG ~ ., df[is_train,])
m2 <- lm(WEIG ~ GEN * ., df[is_train,])
y_pred.m1 <- predict(m1, df[!is_train,])
y_pred.m2 <- predict(m2, df[!is_train,])
c(rmse(y_test, y_pred.m1), rmse(y_test, y_pred.m2))
X_train.bis <- model.matrix(~ 0 + GEN * ., X_train)
X_test.bis <- model.matrix(~ 0 + GEN * ., X_test)

assess.glmnet(predict(cv.glmnet(X_train.bis, y_train), s="lambda.1se", newx=X_test.bis), newy=y_test)

