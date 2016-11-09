# Relationship of SVD and EVD for a given matrix
M <- as.matrix(USArrests)
X <- scale(M)
XtX <- t(X) %*% X
XXt <- X %*% t(X)

# Decompositions
SVD <- svd(X)
EVD1 <- eigen(XtX)
EVD2 <- eigen(XXt)

# singular values -vs- eigenvalues
SVD$d^2
EVD1$values
EVD2$values[1:4]

SVD$v
EVD1$vectors

head(SVD$u)
head(EVD2$vectors[,1:4])
