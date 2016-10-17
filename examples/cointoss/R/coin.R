#' @title Coin
#' @description Creates an object of class \code{"coin"}
#' @param sides vector of coin sides
#' @param prob vector of side probabilities
#' @return an object of class coin
#' @export
#' @examples
#' # default 
#' coin1 <- coin()
#' 
#' # another coin
#' coin2 <- coin(c('h', 't'))
#' 
#' # us cent
#' cent1 <- coin(c('lincoln', 'shield'))
#' 
#' # loaded coin
#' loaded <- coin(prob = c(0.7, 0.3))
#' 
coin <- function(sides = c("heads", "tails"), prob = c(0.5, 0.5)) {
  check_sides(sides)
  check_prob(prob)

  object <- list(
    sides = sides,
    prob = prob)
  class(object) <- "coin"
  object
}


# private function to check vector of sides
check_sides <- function(sides) {
  if (length(sides) != 2) {
    stop("\n'prob' must be a vector of length 2")
  } 
  if (!is.numeric(sides) & !is.character(sides)) {
    stop("\n'sides' must be a character or numeric vector")
  }
  TRUE
}


# private funciton to check vector of probabilities
check_prob <- function(prob) {
  if (length(prob) != 2 | !is.numeric(prob)) {
    stop("\n'prob' must be a numeric vector of length 2")
  }
  if (any(is.na(prob))) {
    stop("\n'prob' cannot contain missing values")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}



#' @export
print.coin <- function(x, ...) {
  cat('object "coin"\n\n')
  cat(sprintf('"%s", p = %s', x$sides[1], x$prob[1]), "\n")
  cat(sprintf('"%s", p = %s', x$sides[2], x$prob[2]), "\n")
  invisible(x)
}


#' @rdname coin
#' @param x an R object
#' @export
is.coin <- function(x) {
  is(x, "coin")
}
