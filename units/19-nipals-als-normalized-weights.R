# NIPALS (or ALS) algorithm to obtain normalized weights
# 
# NIPALS: Non-Linear Iterative Partial Least Squares
# ALS: Alternating Least Squares
#
# Decompose a matrix X as:  X = Z %*% t(W)
# where:
# Z is the matrix of orthogonal scores (i.e. factors or components)
# W is the matrix of orthonormal weights (i.e. loadings)


# Generate fake data set
set.seed(123)
n <- 10
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- runif(n)
x4 <- (x1 + x2 + x3) / 3

# mean-centered and scale data
X <- scale(cbind(x1, x2, x3, x4))

# arbitrary initial vector of scores
z_old <- rep(1, nrow(X))
tolerance <- 0.00001
criterion <- 1
iterations <- 0

while ((criterion > tolerance) & (iterations < 100)) {
  iterations <- iterations + 1
  w <- t(X) %*% z_old
  # normalized w
  w <- w / sqrt((sum(w * w)))
  # regress rows of X onto w
  z_new <- X %*% w
  # check convergence
  criterion <- sqrt(sum((z_new - z_old)^2))
  # update z_old
  z_old <- z_new
  print(z_new)
}

# after convergence
z <- z_new
w <- t(X) %*% z
w <- w / sqrt((sum(w * w)))


# Compare to SVD
Xsvd <- svd(X)
U <- Xsvd$u
D <- diag(Xsvd$d)
V <- Xsvd$v

# orthogonal scores
Z <- U %*% D
Z[,1]
# orthonormal (normalized) weights
W <- V
W[,1]




