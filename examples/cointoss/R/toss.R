#' @title Toss a coin
#' @description Creates an object of class \code{"toss"}
#' @param coin object of class \code{"coin"}
#' @param times number of tosses
#' @return an object of class \code{"toss"} with the following elements:
#' @return \item{tosses}{vector of tosses}
#' @return \item{sides}{vector of coin \code{"sides"}}
#' @return \item{prob}{vector of coin \code{"prob"}}
#' @return \item{total}{total number of tosses}
#' @return \item{heads}{number of heads}
#' @return \item{tails}{number of tails} 
#' @export
#' @examples
#' coin1 <- coin()
#' 
#' # toss a coin 10 times
#' toss10 <- toss(coin1, times = 10)
#' 
#' # add 5 more tosses
#' toss15 <- toss10 + 5
toss <- function(coin, times = 1) {
  check_times(times)
  flips <- flip(coin, times = times)
  make_toss(coin, flips)
}


# private function to check vector of 'times'
check_times <- function(times) {
  if (times <= 0 | !is.numeric(times)) {
    stop("\nargument 'times' must be a positive integer")
  } else {
    TRUE
  }
}


# private function
flip <- function(x, times = 1) {
  sample(x$sides, size = times, replace = TRUE, prob = x$prob)
}


#' @title Make Toss Object
#' @description Constructor function for object "toss"
#' @param coin object of class coin
#' @param flips object of class flips
#' @keywords internal
make_toss <- function(coin, flips) {
  res <- list(
    tosses = flips,
    coin = coin,
    total = length(flips),
    heads = sum(flips == coin$sides[1]),
    tails = sum(flips == coin$sides[2]))
  class(res) <- "toss"
  res
}



#' @export
print.toss <- function(x, ...) {
  cat('object "toss"\n\n')
  cat(sprintf('coin: "%s", "%s"', x$coin$sides[1], x$coin$sides[2]), "\n")
  cat("total tosses:", x$total, "\n")
  cat(sprintf("num of %s:", x$coin$sides[1]), x$heads, "\n")
  cat(sprintf("num of %s:", x$coin$sides[2]), x$tails, "\n")
  invisible(x)
}


#' @export
summary.toss <- function(x, ...) {
  proportions <- c(
    sum(x$tosses == x$coin$sides[1]) / x$total,
    sum(x$tosses == x$coin$sides[2]) / x$total
  )
  freqs <- data.frame(
    side = x$coin$sides,
    count = c(x$heads, x$tails),
    prop = proportions)
  obj <- list(freqs = freqs)
  class(obj) <- "summary.toss"
  obj
}


#' @export
print.summary.toss <- function(x, ...) {
  cat('summary "toss"\n\n')
  print(x$freqs)
  invisible(x)
}


#' @export
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


#' @export
"[.toss" <- function(x, i) {
  x$tosses[i]
}


#' @export
"+.toss" <- function(obj, incr) {
  if (length(incr) != 1 | incr <= 0) {
    stop("\ninvalid increament")
  }
  more_flips <- flip(obj$coin, times = incr)
  make_toss(obj$coin, c(obj$tosses, more_flips))
}


#' @rdname toss
#' @param x an R object
#' @export
is.toss <- function(x) {
  inherits(x, "toss")
}


#' @rdname toss
#' @export
as.toss <- function(x) {
  x_coin <- coin(unique(x))
  make_toss(x_coin, x)
}


