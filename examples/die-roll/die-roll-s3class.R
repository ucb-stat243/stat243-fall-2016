# ==============================================================
# Object Die
# ==============================================================

# auxiliar function to check the sides of a die
check_sides <- function(sides) {
  if (length(sides) != 6 | !is.numeric(sides)) {
    stop("\n'sides' must be a numeric vector of length 6")
  }
  TRUE
}


# auxiliar function to check probabilities of the sides of a die
check_prob <- function(prob) {
  if (length(prob) != 6 | !is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector of length 6")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}


#' @title Die object
#' @description constructor function of objects of class "die"
#' @param sides vector of die sides
#' @param prob vector with probabilities of sides
#' @return an object of class "die"
die <- function(sides = 1:6, prob = rep(1/6, 6)) {
  check_sides(sides)
  check_prob(prob)
  object <- list(
    sides = sides,
    prob = prob
  )
  class(object) <- "die"
  object
}


# print method for object of class "die"
print.die <- function(x, ...) {
  cat('object "die"\n\n')
  df_die <- data.frame(side = x$sides, prob = x$prob)
  print(df_die)
  invisible(x)
}


# ==============================================================
# Object Roll
# ==============================================================

# auxiliar function to check 'times'
check_times <- function(times) {
  if (times <= 0 | !is.numeric(times)) {
    stop("\nargument 'times' must be a positive integer")
  } else {
    TRUE
  }
}

# auxiliar function to roll a die
roll_die <- function(x, times = 1) {
  sample(x$sides, size = times, replace = TRUE, prob = x$prob)
}


# internal constructor function for object "roll"
make_roll <- function(die, rolls) {
  object <- list(
    rolls = rolls,
    sides = die$sides,
    prob = die$prob,
    total = length(rolls))
  class(object) <- "roll"
  object
}


#' @title Roll object
#' @description main roll function
#' @param die object of class "die"
#' @param times number of times to roll the die
#' @return an object of class "roll"
roll <- function(die, times = 1) {
  rolls <- roll_die(die, times = times)
  make_roll(die, rolls)
}


# print method for object of class "roll"
print.roll <- function(x, ...) {
  cat('object "roll"\n\n')
  cat('$rolls\n')
  print(x$rolls)
  invisible(x)
}


# summary method for object of class "roll"
summary.roll <- function(x, ...) {
  freqs <- table(x$rolls)
  relative_freqs <- prop.table(freqs)
  # table of frequencies
  roll_freqs <- as.data.frame(cbind(
    side = x$sides, 
    count = freqs, 
    prop = relative_freqs))
  obj <- list(freqs = roll_freqs)
  class(obj) <- "summary.roll"
  obj
}


# print summary method for object of class "roll"
print.summary.roll <- function(x, ...) {
  cat('summary "roll"\n\n')
  print(x$freqs)
  invisible(x)
}



# plot method for objects of class "roll"
plot.roll <- function(x, ...) {
  freqs <- prop.table(table(x$rolls))
  barplot(freqs, border = NA, las = 1,
          xlab = "sides of die",
          ylab = "relative frequencies")
  title(sprintf("Frequencies in a series of %s die rolls", length(x$rolls)))
}


# replacement method for "roll"
"[<-.roll" <- function(x, i, value) {
  if (all(x$sides != value)) {
    stop(paste('\nreplacing value must be one of', x$sides))
  }
  if (i > length(x$rolls)) {
    stop("\nindex out of bounds")
  }
  x$rolls[i] <- value
  x
}


# extraction method for objects of class "roll"
"[.roll" <- function(x, i) {
  x$rolls[i]
}


# addition method for class "roll"
"+.roll" <- function(obj, incr) {
  if (length(incr) != 1 | incr <= 0) {
    stop("\ninvalid increament")
  }
  more_rolls <- roll_die(obj, times = incr)
  obj$rolls <- c(obj$rolls, more_rolls)
  obj$total <- length(obj$rolls)
  obj
}


# check class "roll"
is.roll <- function(x) {
  inherits(x, "roll")
}


# ==============================================================
# Chi-square Statistic
# ==============================================================

# chi square statistic for an object "roll"
chi_square <- function(x) {
  obs <- prop.table(table(x$rolls))
  exp <- 1/6
  chi <- length(x$rolls) * sum( ((obs - exp)^2) / exp )
  list(
    stat = chi,
    df = 5
  )
}



