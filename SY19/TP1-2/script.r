library("FNN")

setwd("C:/Users/nguyenra/Documents/SY19/TP1-2")
data <- read.table("prostate.data")

# PARTIE I

training <- data[data$train == TRUE, ]
testing <- data[data$train == FALSE, ]

training_cols <- c(
  'lcavol', 'lweight', 'age',
  'lbph'
)

# column selector
y_train <- training[, c('lpsa')]
y_test <- testing[, c('lpsa')] 
training <- scale(training[, training_cols])
# reutiliser les donnees de normalisation (mean + sd) et l'appliquer sur testing
testing <- scale(testing[, training_cols])

# syntaxe alternative
# X.tst <- scale(data[!data$train, 1:4])

result <- knn.reg(
  training, 
  testing, 
  y_train, 
  k = 3, 
  algorithm=c("kd_tree", "cover_tree", "brute")
)

# erreur quadratique
mean((result$pred - y_test) ^ 2)

# parametres pour le modele
Ks <- 1:15

MSEs <- rep(0, length(Ks))
for (i in 1:length(Ks)) {
  K <- Ks[i]
  reg <- knn.reg(
    training, 
    testing, 
    y_train, 
    k = K
  )
  MSE <- mean((reg$pred - y_test) ^ 2)
  MSEs[i] <- MSE
}
plot(MSEs)


# Partie II

# simuler la variable
# en prenant la formule de base
# et en rajoutant du bruit (e)

rf <- function(n) {
  vx <- runif(n) # default = 0, 1
  vbruit <- rnorm(n, mean = 0, sd = sd)
  vy <- 1 + 5 * vx**2 + vbruit
  return(c(vx, vy))
}

# PARAMS
sd = .5

n = 100
N = 1000

Ks <- 1:15

yhat <- matrix(0, length(Ks), N)

for (i in 1:N) {
  x = rf(n)
  y = x[1]
  x = x[0]
  
  for (j in 1:length(Ks)) {
    K <- Ks[j]
    
    reg <- knn.reg(
      x, 
      test = NULL, 
      y, 
      k = K
    )
    MSE <- mean((reg$pred - y_test) ^ 2)
    yhat[j, i] <- reg$pred
  }
}

plot(MSEs)

# Var(f_K(x_0))
apply(yhat, MARGIN=1, FUN=fun)

Ey0 <- 1 + 5*x0^2
(apply(yhat, MARGIN=1, FUN=var) - Ey0)^2

fun <- function(row) {
  mea(row - y0)
}