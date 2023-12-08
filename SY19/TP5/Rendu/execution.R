# 1. Apprentissage des modèles.
#############################  REGRESSION ############################################################
donnees <- read.table("TP5_a23_reg_app.txt")
library(glmnet)

n <- nrow(donnees)
donnees2 <- donnees[,-101]
n_colonnes <- ncol(donnees)

for (i in 1:n_colonnes-1) {
  nom_colonne <- colnames(donnees2)[i]
  nom_colonne2 <- colnames(donnees2)[i+1]
  donnees2[paste0(nom_colonne, "/",nom_colonne2)] <- (abs(donnees2[,i])/abs(donnees2[,i+1]))^0.25
}
donnees2 <- cbind(donnees2, y = donnees[,"y"]) 

X.app <-data.frame(donnees2[
  ,c(
    "X4","X6","X8","X10","X12","X13","X14", 
    "X16","X18","X20","X21","X22","X24","X26","X30","X31",
    "X32","X35","X37","X42","X43","X46","X47", 
    "X48","X50","X51","X52","X53","X55","X56","X57","X59", 
    "X65","X66","X67","X70","X71","X74","X80", 
    "X82","X83","X84","X90","X91","X92","X93","X94","X95","X96", 
    "X100","X12/X13","X18/X19", 
    "X19/X20","X32/X33","X37/X38",
    "X45/X46","X48/X49","X49/X50","X56/X57", 
    "X63/X64", 
    "X70/X71","X80/X81","X82/X83","X83/X84", 
    "X96/X97","X97/X98", 
    "X99/X100","y")])

x <- model.matrix(y~.,X.app)
y_train <- X.app[,"y"]

cv.out_lasso<-cv.glmnet(x,y_train,alpha=1)
reg<-glmnet(x,y_train,lambda=cv.out_lasso$lambda.min,alpha=1)

#### Classification
library(MASS)

data <- read.table("TP5_a23_clas_app.txt")
data$y <- as.factor(data$y)

clas <- qda(formula = y ~ ., data = data)

###################################################################################################

X.clas = read.table("TP5_a23_clas_app.txt")
clas <- lda(y ~ ., data = X.clas)

# 2. Création des fonctions de prédiction

classifieur <- function(test_set) {
  # Ne pas oublier de charger **à l'intérieur de la fonction** les
  # bibliothèques utilisées.
  library(MASS)
  
  # Attention à ce que retourne un modèle en prédiction. La lda
  # retourne une liste nommée. On sélectionne donc les classes ici.
  predict(clas, test_set)$class
}

regresseur <- function(test_set) {
  library(glmnet)
  
  predict(reg, test_set)
}

# 3. Sauvegarder sous forme de fichier .Rdata les fonctions
# `prediction_cls` et `prediction_reg` sans changer leur nom et avec
# les mêmes arguments. Sauvegarder également les objets utilisés dans
# ces fonctions (`clas` et `reg` dans l'exemple) !

save("clas", "reg", "classifieur", "regresseur", file = "env.Rdata")
