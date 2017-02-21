#' Convert a string to a vector of integers mod 26
#'
#' @param x A string (assumed to contain only lowercase letters)
#'
#' @return A vector of integers, where 0 corresponds to 'a', 1 to 'b', etc.
#' @export
#' @examples
#' stringToMod26("cubswin")
stringToMod26 <- function(x) {utf8ToInt(x)-utf8ToInt("a")}
