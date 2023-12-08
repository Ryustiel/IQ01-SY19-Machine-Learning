setwd("C:/Users/nguyenra/Documents/SY19/TP3")

library('Ecdat')

data <- Participation
summary(data)

reg = glm(formula = lfp ~ ., family = "binomial", data = data)

summary(reg)
# on peut identifier là les variables qui servent pour la régression logistique



# EXERCICE 2

data <- read.table("spambase.dat")


# vérifier que le nombre d'indices obtenus est correct
# dataframe = collection de colonnes, pas d'erreurs si on met des indices de dimension 1 (sélectionne les colonnes)
train_indices = sample(
  nrow(data), 
  size = floor(nrow(data) * 2/3), 
  replace = FALSE
)

train = data[train_indices,]
test = data[-train_indices,]

reg = glm(formula = V58 ~ ., data = train, family = "binomial")

response = predict(reg, newdata=test, type="response")
# hist(response)

# pour avoir la matrice de decision il faut des classes,
# et on n'a pas de classe, donc on va en créer

# on choisit de diviser les données en deux classes suivant 0.5 milieu probas prédicteurs
response.pred = response > .5

perf = table(pred, test$V58)
# + on a une grosse diagonale + c'est vrai
# + on est hors diagonale + c'est faux (rapport entre valeurs et prédictions)
# on peut faire le ration entre sur diagonale et erreurs, hors diagonale

mean(response.pred != test$V58)


# Question 3

library("MASS")

data.lda = lda(formula = V58 ~ ., data = train)

pred.lda = predict(data.lda, newdata = test)$class
summary(pred.lda)

table(pred.lda, test$V58)

mean(pred.lda != test$V58)

# la régression logistique fais moins d'erreurs

pred.lda2 = predict(data.lda, newdata = test)$posterior[, 2]
# résultats de l'algo

hist(pred.lda2 - response)
plot(pred.lda2, response)

# QUESTION 5

library("pROC")

?roc
# montre la sensibilité à chaque classe par rapport à la vraie valeur
# adapté à la classification binaire
plot(roc(test$V58, pred.lda2))

# la régression logistique est systématiquement meilleure
plot(roc(test$V58, response), add = TRUE, col = "red")

# "courbe roc qui suit y = x c'est du pile ou face" --
# sans classe la courbe roc a pas de sens

# si on augmente la puissance (sensi) on va augmenter la détection erronée de mails qui sont pas des spams



# PROBLEME 3

library(mvtnorm)

mu1 = c(0, 0)
mu2 = c(0, 2)
mu3 = c(2, 0)
sigma = matrix(c(1, 0.5, 0.5, 2), 2, 2)
p1 = 0.3
p2 = 0.3
p3 = 1 - p1 * p2

gen.data = function(n, mu1, mu2, mu3, sigma1, sigma2, sigma3, p1, p2) {
  y = sample(3, n, prob=c(p1, p2, 1-p1*p2), replace=TRUE)
  x = matrix(0, n, 2)
  N1 = length(which(y==1)) # number of objects from class 1
  N2 = length(which(y==2))
  N3 = length(which(y==3))
  X[y==1] = rmvnorm(N1, mu1, sigma1)
  X[y==2] = rmvnorm(N2, mu2, sigma2)
  X[y==3] = rmvnorm(N3, mu3, sigma3)
  return(list(X=X, y=y))
}

test = gen.data(n=10000, mu1, mu2, mu3, sigma, sigma, sigma, p1=p1, p2=p2)

g = cbind(
  p1 * dmvnorm(test$X, mu1, sigma),
  p2 * dmvnorm(test$X, mu2, sigma),
  p3 * dmvnorm(test$X, my3, sigma)
)

ypred = max.col(g)

errb = mean(test$y != ypred)
errb

## voir pour la structure de "test"
# chercher d'où ça vient


