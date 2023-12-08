# setwd("C:/Users/nguyenra/Documents/SY19/TP4")

df <- read.csv2("Residential_Building.csv", sep=";")
# 105 x 372

# V9 : couts
# V10 : prix

# transformation "logarithmique"
df$log.cost <- log(df$V9)
df$log.price <- log(df$V10)

# hist(data$prix)
df$V9 = NULL
df$V10 = NULL # supprime les colonnes

# utiliser le modèle exponentiel au lieu de binomial
# pour régression logistique
fit <- lm(log.cost ~ . -log.price, data = df)
summary(fit)

library(Matrix)
rankMatrix(as.matrix(df))
length(df) # 79 < 107

# le problème vient de l'inversibilité de Xt X
# est inversible ssi son rang est maximal
# donc faut s'assurer de ça sur la matrice associée
# "les features forment une famille libre"
# (faut pas qu'une soit une combinaison linéaire des autres)

# il faudrait utiliser autre chose que la régression linéaire

# QUESTION 3 et 4

n = nrow(df)
K = 10
folds = sample(K, n, replace = TRUE)
rms = rep(0, K)
rms.aic = rep(0, K)
rms.bic = rep(0, K)

for(i in 1:K) {
  fit <- lm(
    log.price ~ . -log.cost, 
    data = df, 
    subset = folds != i, # on veut apprendre sur tout sauf le i-eme fold
  )
  
  n.train = sum(folds != i)
  
  fit.aic = stepAIC(fit, scope = log.price ~ . -log.cost, direction = "both")
  fit.bic = stepAIC(fit, scope = log.price ~ . -log.cost, direction = "both", k = log(n.train))
  
  pred = predict(fit, newdata = df[folds == i, ])
  pred.aic = predict(fit.aic, newdata = df[folds == i, ])
  pred.bic = predict(fit.bic, newdata = df[folds == i, ])
  
  rms[i] = sqrt(mean((pred - df[folds == i, ]$log.price)^2))
  rms.aic[i] = sqrt(mean((pred.aic - df[folds == i, ]$log.price)^2))
  rms.bic[i] = sqrt(mean((pred.bic - df[folds == i, ]$log.price)^2))
}

mean(rms)
mean(rms) / sqrt(K)

mean(rms.aic) # 0.98
mean(rms.bic) # 1.003

fit = lm(log.price ~ . - log.cost, data = df)
plot(df$log.price, fit$fitted.values)

fit.bic = stepAIC(fit, direction = "both", k = log(n))
plot(df$log.price, fit.bic$fitted.values)

formula(fit.bic)
