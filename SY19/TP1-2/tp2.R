library("FNN")

setwd("C:/Users/nguyenra/Documents/SY19/TP1-2")
data <- read.table("prostate.data")

attach(data)
names(data)
# detach(data) <- retire les colonnes de la portée globale

reg <- lm(lpsa ~ . - train, data=data)
summary(reg)

# gaussienne liée à l'erreur sur les coefficients
# t value test hypothèse nulle = l'espérance
# de la gaussienne est nulle

# plot(reg)

confint(reg)
# quand l'intervalle de confiance contient zero
# ça veut dire qu'il n'est pas significatif

# seul truc significatif c'était lcavol et lcavol
# il contient pas zero

plot(reg$fitted.values, lpsa)
abline(0, 1)

# Q4 : Estimer les coefficients
# sur les donnees d'apprentissage

train <- data[data$train == TRUE, ]
test <- data[data$train == FALSE, ]

training_cols <- c(
  'lcavol', 'lweight', 'age',
  'lbph'
)

y_train <- train[, c('lpsa')]
y_test <- test[, c('lpsa')] 
train = scale(train[, training_cols])
test = scale(test[, training_cols])
# n'est pas utile pour un modèle linéaire,
# seulement pour les KNN ou au moins
# pour les modèles pour lesquels
# la distance absolue importe

reg2 <- lm(
  lpsa ~ . - train, 
  data = data,
  subset = train
) # subset référence la colonne booléenne train

summary(reg)
summary(reg2)

# predict : tout le temps la même méthode
# quelque soit le modèle

pred = predict(
  reg2,
  newdata = data[!data$train,]
  ) # on passe une dataframe
# avec data non utilisée pour l'entrainement prec

MSE = mean((pred - data[!data$train, "lpsa"])^2)
MSE





# QUESTION 5 AU PROPRE

df <- read.table("prostate.data")


reg <- lm(
  df$lpsa ~ . - train, 
  data = df,
  subset = df$train
)

pred = predict(
  reg,
  newdata = df[!df$train,],
  interval = "p"
)

# lpsa de test pour référence
lpsaref = df[!df$train, ]$lpsa

plot(pred[,"fit"], lpsaref)
# pred[,fit] <=> reg2$fitted.values
abline(0, 1)

points(pred[, "lwr"], lpsaref, pch=19)
points(pred[, "upr"], lpsaref, pch=19)
# les bornes encadrent bien...
# pch : taille des points