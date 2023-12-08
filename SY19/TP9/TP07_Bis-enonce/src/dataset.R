sample.donut <- function(n1, r1, n2, r2) {
    p <- 2

    R1 <- rnorm(n1, mean = r1)
    angle1 <- runif(n1, 0, 2 * pi)
