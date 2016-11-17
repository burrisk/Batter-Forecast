library(MASS)
library(LaplacesDemon)

# Dirichlet Simulation
N <- 2500
b1vec <- c(-0.1, 0.1, 0.3)
b2vec <- c(-0.01, -0.03, -0.05)
x1 <- rnorm(2500, 0, 0.02)
x2 <- rbinom(2500, 1, 0.4)
a_mean <- c(-0.75, -1.50, -1.75)
alpha_mat <- matrix(0, nrow = 2500, ncol = 3)
alpha_mat[1,] <- c(-0.75, -1.50, -1.75)
for (i in 2:2500){
  for (j in 1:3){
    alpha_mat[i,j] <- rnorm(1, a_mean[j] + 0.995*(alpha_mat[i-1,j] - a_mean[j]) + 
                              b1vec[j]*x1[i] + b2vec[j]*x2[i],
                            sqrt(1-0.995^2)*0.15)
  }
}
p_mat <- invlogit(alpha_mat)
p_mat = cbind(1-p_mat[,1] - p_mat[,2] - p_mat[,3],p_mat)
plot(p_mat[,1], type = 'l')

  
  