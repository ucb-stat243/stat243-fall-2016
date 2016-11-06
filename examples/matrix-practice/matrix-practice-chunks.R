# code chunks for "matrix-practice.Rmd"

## @knitr vectorx
x <- 1:9


## @knitr matrix1
M1 <- matrix(x, nrow = 3, ncol = 3)
M1


## @knitr matrix2
M2 <- matrix(x, nrow = 3, ncol = 3, byrow = TRUE)
M2

## @knitr identity5
diag(1, nrow = 5)


## @knitr vectors-a
a1 <- c(2, 3, 6, 7, 10)
a2 <- c(1.88, 2.05, 1.70, 1.60, 1.78)
a3 <- c(80, 90, 70, 50, 75)


## @knitr matrix3
M3 <- cbind(a1, a2, a3)
rownames(M3) <- 1:nrow(M3)
M3


## @knitr matrix4
M4 <- rbind(a1, a2, a3)
colnames(M4) <- 1:ncol(M4)
M4


## @knitr vnorm
# vector norm
vnorm <- function(x) {
  sqrt(t(x) %*% x)
}

vnorm(1:5)


## @knitr unitnorm
# vector of unit norm
v <- 1:5
u <- v / vnorm(v)
vnorm(u)


## @knitr is_square
# is a matrix square
is_square <- function(m) {
  nrow(m) == ncol(m)
}

is_square(matrix(1:9, 3, 3))


## @knitr mtrace
# function for trace of a matrix
mtrace <- function(m) {
  if (!is_square(m)) {
    stop("\n'mtrace()' requires a square matrix")
  }
  sum(diag(m))
}

mtrace(matrix(1:9, nrow = 3, ncol = 3))


## @knitr lineartrace
# trace is a linear mapping 
set.seed(123)
A <- matrix(sample(1:9, 9), nrow = 3, ncol = 3)
B <- matrix(sample(1:9, 9), nrow = 3, ncol = 3)

mtrace(A + B)
mtrace(A) + mtrace(B)

mtrace(3 * A)
3 * mtrace(A)


## @knitr prodtrace
# trace of a product
set.seed(123)
X <- matrix(sample(1:12, 12), nrow = 4, ncol = 3)
Y <- matrix(sample(1:12, 12), nrow = 4, ncol = 3)

mtrace(t(X) %*% Y)
mtrace(X %*% t(Y))
mtrace(t(Y) %*% X)
mtrace(Y %*% t(X))


## @knitr crossprod
# crossproduct
set.seed(345)
X <- matrix(runif(100000), 1000, 100)
system.time(t(X) %*% X)
system.time(crossprod(X))


## @knitr tcrossprod
# tcrossproduct
set.seed(345)
Y <- matrix(runif(100000), 100, 1000)
system.time(Y %*% t(Y))
system.time(tcrossprod(Y))



## @knitr transformations
M <- as.matrix(mtcars[ ,c('mpg', 'disp', 'hp', 'drat', 'wt')])

# vector of column-means
means <- apply(M, MARGIN = 2, FUN = mean)

# mean-centered Mc with scale()
Mc <- scale(M, scale = FALSE)
apply(Mc, MARGIN = 2, FUN = mean)

# mean-centered with sweep()
Mc2 <- sweep(M, MARGIN = 2, STATS = means, FUN = "-")
apply(Mc2, MARGIN = 2, FUN = mean)

# scaling by the maximum
maxs <- apply(M, MARGIN = 2, FUN = max)
Mx <- sweep(M, MARGIN = 2, STATS = maxs, FUN = "/")

# rescaling from 0 to 1
mins <- apply(M, MARGIN = 2, FUN = min)
ranges <- apply(M, MARGIN = 2, FUN = function(x) max(x) - min(x))
M01 <- scale(M, center = mins, scale = ranges)
# test it
apply(M01, MARGIN = 2, FUN = min)
apply(M01, MARGIN = 2, FUN = max)


## @knitr dummify
# function dummify
dummify <- function(x, all = FALSE) {
  if (!is.factor(x)) x <- as.factor(x)
  categories <- levels(x)
  num_categories <- length(categories)
  if (!all) {
    num_categories <- length(categories) - 1
  }
  dummies <- matrix(0, nrow = length(x), ncol = num_categories)
  for (k in 1:num_categories) {
    dummies[x == categories[k],k] <- 1
  }
  colnames(dummies) <- categories[1:num_categories]
  dummies
}

# test it
cyl <- factor(mtcars$cyl)
CYL1 <- dummify(cyl, all = TRUE)
CYL2 <- dummify(cyl, all = FALSE)


## @knitr crosstable
# function crosstable
crosstable <- function(x, y) {
  if (!is.factor(x)) x <- factor(x)
  if (!is.factor(y)) y <- factor(y)
  Xdum <- dummify(x, all = TRUE)
  Ydum <- dummify(y, all = TRUE)
  t(Xdum) %*% Ydum
}

cyl <- factor(mtcars$cyl)
gear <- factor(mtcars$gear)
xtb <- crosstable(cyl, gear)


