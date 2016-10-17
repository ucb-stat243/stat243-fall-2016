#' @title Plot of object toss
#' @description Plots the relative frequencies of a series of tosses
#' @param x an object of class \code{"coin"}
#' @param side number indicating which side of coin to consider
#' @param \dots arguments to be passed to/from other methods
#' @export
#' @examples
#'  \dontrun{
#'  # create a coin and toss it 100 times
#'  coin1 <- coin()
#'  toss1000 <- toss(coin1, times = 1000)
#'  
#'  plot(toss1000)
#'  }
plot.toss <- function(x, side = 1, ...) {
  freqs <- frequencies(x, side = side)
  plot(1:x$total, freqs, type = "n", ylim = c(0, 1), las = 1,
       xlab = "number of tosses", bty = "n",
       ylab = sprintf("relative frequency of %s", x$coin$sides[side]))
  abline(h = 0.5, col = "gray70", lwd = 1.5)
  lines(1:x$total, freqs, col = "tomato", lwd = 2)
  title(sprintf("Relative Frequencies in a series of %s coin tosses", x$total))
}



# auxiliary functions for plot.toss() method
head_freqs <- function(x) {
  cumsum(x$tosses == x$coin$sides[1]) / 1:x$total
}

tail_freqs <- function(x) {
  cumsum(x$tosses == x$coin$sides[2]) / 1:x$total
}

frequencies <- function(x, side = 1) {
  if (side == 1) {
    return(head_freqs(x))
  } else {
    return(tail_freqs(x))
  }
}

