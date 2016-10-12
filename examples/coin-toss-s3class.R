
# auxiliar function to check 'prob'
check_prob <- function(prob) {
  # if (!is.numeric(prob)) {
  #   stop("\n'prob' must be a numeric vector")
  # }
  if (length(prob) != 2 | !is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector of length 2")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}


# class "coin"
coin <- function(object = c("heads", "tails"), prob = c(0.5, 0.5)) {
  if (length(object) != 2) {
    stop("\n'object' must be of length 2")
  }
  check_prob(prob)
  attr(object, "prob") <- prob
  class(object) <- "coin"
  object
}



# auxiliar function to check 'times'
check_times <- function(times) {
  if (times <= 0 | !is.numeric(times)) {
    stop("\nargument 'times' must be a positive integer")
  } else {
    TRUE
  }
}


# Declare Generic Method "flip"
flip <- function(x, ...) UseMethod("flip")


# flip method for "coin" object
flip.coin <- function(x, times = 1) {
  check_times(times)
  sample(x, size = times, replace = TRUE, prob = attr(x, "prob"))
}


# constructor function for object "toss"
make_toss <- function(coin, flips) {
  res <- list(
    coin = coin,
    tosses = flips,
    total = length(flips),
    heads = sum(flips == coin[1]),
    tails = sum(flips == coin[2]))
  class(res) <- "toss"
  res
}


# main toss function
toss <- function(coin, times = 1) {
  flips <- flip(coin, times = times)
  make_toss(coin, flips)
}


# print method for object of class "toss"
print.toss <- function(x, ...) {
  cat('object "toss"\n')
  cat(sprintf('coin: "%s", "%s"', x$coin[1], x$coin[2]), "\n")
  cat("total tosses:", x$total, "\n")
  cat(sprintf("num of %s:", x$coin[1]), x$heads, "\n")
  cat(sprintf("num of %s:", x$coin[2]), x$tails, "\n")
  invisible(x)
}


# summary method for object of class "toss"
summary.toss <- function(object) {
  structure(object, class = c("summary.toss", class(object)))
}

print.summary.toss <- function(x, ...) {
  cat('summary "toss"\n\n')
  cat(sprintf('coin: "%s", "%s"', x$coin[1], x$coin[2]), "\n")
  cat("total tosses:", x$total, "\n\n")
  cat(sprintf("num of %s:", x$coin[1]), x$heads, "\n")
  cat(sprintf("prop of %s:", x$coin[1]), x$heads/x$total, "\n\n")
  cat(sprintf("num of %s:", x$coin[2]), x$tails, "\n")
  cat(sprintf("prop of %s:", x$coin[2]), x$tails/x$total, "\n")
  invisible(x)
}


# auxiliary functions for plot.toss() method
head_freqs <- function(x) {
  cumsum(x$tosses == x$coin[1]) / 1:x$total
}

tail_freqs <- function(x) {
  cumsum(x$tosses == x$coin[2]) / 1:x$total
}

frequencies <- function(x, side = 1) {
  if (side == 1) {
    return(head_freqs(x))
  } else {
    return(tail_freqs(x))
  }
}

# plot method for objects of class "toss"
plot.toss <- function(x, side = 1, ...) {
  freqs <- frequencies(x, side = side)
  plot(1:x$total, freqs, type = "n", ylim = c(0, 1), las = 1,
       xlab = "number of tosses", bty = "n",
       ylab = sprintf("relative frequency of %s", x$coin[side]))
  abline(h = 0.5, col = "gray70", lwd = 1.5)
  lines(1:x$total, freqs, col = "tomato", lwd = 2)
  title(sprintf("Relative Frequencies in a series of %s coin tosses", x$total))
}


# replacement method for "toss"
"[<-.toss" <- function(x, i, value) {
  if (value != x$coin[1] & value != x$coin[2]) {
    stop(sprintf('\nreplacing value must be %s or %s', x$coin[1], x$coin[2]))
  }
  if (i > x$total) {
    stop("\nindex out of bounds")
  }
  x$tosses[i] <- value
  make_toss(x$coin, x$tosses)
}


# extraction method for objects of class "toss"
"[.toss" <- function(x, i) {
  x$tosses[i]
}


# check class "toss"
is.toss <- function(x) {
  inherits(x, "toss")
}


# addition method for class "toss"
"+.toss" <- function(obj, incr) {
  if (length(incr) != 1 | incr <= 0) {
    stop("\ninvalid increament")
  }
  more_flips <- flip(obj$coin, times = incr)
  make_toss(obj$coin, c(obj$tosses, more_flips))
}


# coercion method for a vector
as.toss <- function(x) {
  x_coin <- coin(unique(x))
  make_toss(x_coin, x)
}


# declaring "tails" method
tails <- function(x) UseMethod("tails")

# tails for object "toss"
tails.toss <- function(x) {
  x$tails
}


# declaring "heads" method
heads <- function(x) UseMethod("heads")

# tails for object "toss"
heads.toss <- function(x) {
  x$heads
}

