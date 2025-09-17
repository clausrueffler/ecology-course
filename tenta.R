matrix <- matrix(data = c(0, 10, 40, 0.05, 0.1, 0.2, 0, 0.2, 0.1), nrow = 3, ncol = 3, byrow = T)

matrix

library(popbio)
eigen.analysis(matrix)

n0 <- c(100, 10, 5)
(n1 <- matrix %*% n0)
(n2 <- matrix %*% n1)