#' Determines if a number is "probably" Prime
#'
#' Checks if a bigz integer, n, is "probably" prime using Fermat's theorem.
#'
#' @param n A bigz integer.
#' @param a Some bigz integer less than n.
#'
#' @return A boolean value.
#' @export
#'
#' @examples
#' FermatTest(as.bigz(9^9), as.bigz(9^8))
#'
#'
FermatTest <- function(n, a){
  if (a > n){
    print("ERROR: a must be less than n")
  } else {
  x <- powm(a, (n-1), n)
  if (x == 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
  }
}

