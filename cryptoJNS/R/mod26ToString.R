#' Convert a vector of integers mod 26 to a string
#'
#' @param x A vector of integers (assumed to contain only 0...25)
#'
#' @return A string, where 'a' corresponds to 0, 'b' to 1, etc.
#' @export
#' @examples
#' mod26ToString(c(18,14,23,11,14,18,4))
mod26ToString <- function(x) {intToUtf8(x+utf8ToInt("a"))}
