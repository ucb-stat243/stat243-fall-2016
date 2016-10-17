#' @title Tails method
#' @description Extracts label of heads
#' @param x an R object
#' @export
tails <- function(x) UseMethod("tails")

#' @export
tails.toss <- function(x) {
  x$tails
}


#' @title Heads method
#' @description Extracts label of heads
#' @param x an R object
#' @export
heads <- function(x) UseMethod("heads")

#' @export
heads.toss <- function(x) {
  x$heads
}

