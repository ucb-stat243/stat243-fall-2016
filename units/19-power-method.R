
# Power Method
X <- as.matrix(USArrests)
S <- cor(X)

# arbitrary initial vector
u_old <- rep(1, ncol(S))
diff <- 1
tolerance <- 0.00001
iterations <- 0

while ((diff > tolerance) & (iterations < 100)) {
  iterations <- iterations + 1
  u_new <- S %*% u_old
  u_new <- u_new / sqrt(sum(u_new * u_new))
  diff <- sqrt(sum((u_old - u_new)^2))
  u_old <- u_new
  print(u_new)
}

# dominant eigenvector
u_new
# dominant eigenvalue
t(u_new) %*% S %*% u_new

# compare with eigen()
Xevd <- eigen(S)
eigenvalues <- Xevd$values
U <- Xevd$vectors

