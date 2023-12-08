library(MASS)

cols = c(
  "crim",
  "nox",
  "dis",
  "lstat"
)
df = Boston[, c("medv", cols)]

# ATTENTION : il peut y avoir des valeurs
# qui sont significatives uniquement 
# les unes par rapport aux autres
# cette vue ne permet pas de voir ce genre de relations
plot(df)

boxplot(Boston$medv ~ Boston$chas, data=Boston)

n <- nrow(df)
p <- ncol(df)

n.train <- floor(3/4 * n)
index.train <- sample(
  n, 
  size = n.train, 
  replace = FALSE
  )
# liste d'indices d'elements pour l'entrainement
# -index.train <= le complementaire de la liste
# dans le sélecteur

length(df[index.train,][,"medv"]) # test

df.train = df[index.train,]
df.test = df[-index.train,]

length(df.test[,"medv"]) # test

reg <- lm(medv ~ ., data = df.train)
pred <- predict(reg, newdata = df.test)

MSE <- mean((pred - df.test$medv)^2)
MSE

# ajouter des prédicteurs qui sont des carrés
# des prédicteurs résultants
# "augmente le pouvoir du modèle linéaire"
# en quelque sorte c'est plus une régression
# linéaire mais polynomiale, on donne accès
# au carré des prédicteurs

# AJOUT DES VARIABLES AU CARRE
# poly(df.train[,"lstat"], degree = 2)

reg <- lm(
  medv ~ . -lstat + poly(lstat, 2), 
  data = df.train
  )
# si on laisse lstat et son carré il y a
# possible dépendance linéaire
# ruine le calcul d'inverse matriciel
# et MSE ne change pas
pred <- predict(reg, newdata = df.test)

MSE <- mean((pred - df.test$medv)^2)
MSE




# EXERCICE 3

foo() <- function() {
  
  n = 100
  sig = .5
  x1 = runif(n)
  x2 = runif(n)
  y = 3 * x1 + 2 * x2 + sig * rnorm(n)
  
  df = data.frame(y=y, x1=x1, x2=x2)
  x10 = .5
  x20 = .5
  
  y0 = 3 * x10 + 2 * x20 + sig * rnorm(1)
  reg = lm(y ~ ., data = df)
  
  pred = predict(
    reg, 
    newdata = data.frame(x1 = c(x10), x2 = c(x20)),
    int = "p",
	level = .9
    )
  
  y0 < pred[1, 3] & pred[1, 2] < y0

}

mean(replicate(1000, foo()))
