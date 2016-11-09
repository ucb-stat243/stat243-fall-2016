# NIPALS (or ALS) algorithm to obtain normalized scores
# 
# NIPALS: Non-Linear Iterative Partial Least Squares
# ALS: Alternating Least Squares
#
# Decompose a matrix X as:  X = Z %*% t(W)
# where:
# Z is the matrix of orthonormal scores (i.e. factors or components)
# W is the matrix of orthogonal weights (i.e. loadings)


# Generate fake data set
set.seed(123)
n <- 10
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- runif(n)
x4 <- (x1 + x2 + x3) / 3

# mean-centered and scale data
X <- scale(cbind(x1, x2, x3, x4))

# arbitrary initial vector of weights
w_old <- rep(1, ncol(X))
tolerance <- 0.0001
criterion <- 1
iterations <- 0

while ((criterion > tolerance) | (iterations > 100)) {
  iterations <- iterations + 1
  z <- X %*% w_old
  # normalized z
  z <- z / sqrt((sum(z * z)))
  # regress variables onto z
  w_new <- t(X) %*% z
  # check convergence
  criterion <- sqrt(sum((w_new - w_old)^2))
  # update w_old
  w_old <- w_new
  print(w_new)
}

# after convergence
w <- w_new
z <- X %*% w
z <- z / sqrt((sum(z * z)))


# Compare to SVD
Xsvd <- svd(X)
U <- Xsvd$u
D <- diag(Xsvd$d)
V <- Xsvd$v

# orthonormal (normalized) scores
Z <- U
Z[,1]
# orthogonal weights
W <- V %*% D
W[,1]


