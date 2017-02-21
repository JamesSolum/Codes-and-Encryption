#' Addition in mod p Elliptic Curves
#'
#' Adds two points on the mod p elliptic curve y^2 = x^3 + bx + c. Uses the Addition Law
#' given on page 352 of [Trappe]. All points on elliptic curves are represented as length
#' two bigz vectors: the point at infinity is represented as \code{c(NA,NA)}. While \code{p1}
#' and \code{p2} are assumed to be valid points on the elliptic curve (not checked), the program
#' should return a warning (using the \code{warning} command) if any of the required inverses
#' in the addition rule fail to exist, returning the value that failed to be invertible.
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (integer or bigz).
#' @param p1 A length 2 integer or bigz vector, representing a point on the curve.
#' @param p2 A length 2 integer or bigz vector, representing a point on the curve.
#'
#' @return A length 2 bigz vector, representing the sum p1 + p2.
#' @export
#' @import gmp
#'
#' @examples
#' ecAddModp(4, 4, 5, c(1,2), c(4,3)) # see page 353
#' ecAddModp(4, 4, 2773, c(1,3), c(1,3)) # see pp. 353-4
#' ecAddModp(4, 4, 2773, c(1771,705), c(1,3)) # see pp. 356-7
#' # Should return a warning message (or two messages):
#' # Warning messages:
#' # 1: In ecAddModp(4, 4, 2773, c(1771, 705), c(1, 3)) :
#' #   -1770 is not invertible mod 2773
#' # 2: In inv.bigz((x2 - x1), modulus) :
#' #   inv(x,m) returning NA as x has no inverse modulo m
ecAddModp <- function(b, c, modulus, p1=c(NA,NA), p2=c(NA,NA)) {
  #Assign Values
  x1 <- p1[1]
  x2 <- p2[1]
  y1 <- p1[2]
  y2 <- p2[2]

  #Check Inverses
  if(x1 != x2){
    if(is.na(inv.bigz((x2-x1), modulus))){
      warning(x2-x1, " has no inverse modulus ", modulus)
      return(c(NA,NA))
    }
  }

  if(x1 == x2){
    if(is.na(inv.bigz((2*y1), modulus))){
      warning(2*y1, " has no inverse modulus ", modulus)
      return(c(NA,NA))
    }
  }


  #Creates Inverses for Slope Generation
  if(x1 != x2){
    invX <- inv.bigz((x2-x1), modulus)
    m <- invX*(y2-y1)
  }
  if(x1 == x2){
    invY <- inv.bigz((2*y1), modulus)
    m <- invY*(3*x1*x1+b)
  }


  #Solves for the New Point
  x3 <- ((m^2) -x1 - x2) %% modulus
  y3 <- (m*(x1-x3)-y1) %% modulus

  return(c(x3, y3))
}

#' Efficient "eponentiation" in mod p elliptic curves
#'
#' Given a point \code{P} on an elliptic curve mod \code{modulus} and an integer/bigz \code{n},
#' computes the "power" nP. (Since elliptic curves form an additive group, we express "powers"
#' as integer multiples.) Does so using repeated "squaring" (doubling). (See the QPower function
#' in the notes.)
#'
#' @param b A bigz or integer representing the coefficient b in the equation of the curve.
#' @param c A bigz or integer representing the coefficient c in the equation of the curve.
#' @param modulus The modulus of the curve (bigz or integer).
#' @param P A length 2 integer or bigz vector, representing a point on the curve.
#' @param n A bigz or integer representing the "exponent".
#'
#' @return A length 2 bigz vector, representing the "power" nP.
#' @export
#' @import gmp
#'
#' @examples
#' ecPowModp(3, 45, 8831, c(4,11), 8) # see p. 364 (this is kG)
#' library(gmp)
#' ecPowModp(3, 45, 8831, c(4,11), as.bigz("2349089023472938409283490823")) # 6863 449
ecPowModp <- function(b, c, modulus, P, n) {

  if(identical(P, c(NA, NA))){
    return(c(NA, NA))
  }

  if(n == 0){
    return(c(NA, NA))
  }

  if ((n == 1) || (n == -1)){
    return(P)
  }

  if(n < 0){
    P <- ecNeg(P)
  }

  if(n %% 2 == 0){
    exponentiation <- ecPowModp(b, c, modulus, P, n %/% 2)
    add <- ecAddModp(b, c, modulus, exponentiation, exponentiation)
    return(add)
  } else {
    exponentiation <- ecPowModp(b, c, modulus, P, n %/% 2)
    add1st <- ecAddModp(b, c, modulus, P, exponentiation)
    final <- ecAddModp(b, c, modulus, add1st, exponentiation)
    return(final)
  }
}















