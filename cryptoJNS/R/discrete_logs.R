#' Discrete Logarithm (brute force)
#'
#' Computes a discrete logarithm by brute force.
#'
#' @param alpha The base, assumed to be a primitive root modulo p.
#' @param beta An element of Z_p
#' @param p A prime modulus.
#'
#' @return Computes L_alpha(beta), the smallest nonnegative integer x such that beta = alpha^x, modulo p.
#' @export
#' @import gmp
#'
#' @examples
#' discreteLogBrute(2,9,11) # should be 6
discreteLogBrute <- function(alpha, beta, p) {
  count <- 0
  while(TRUE){
    if (count <= p){
    if (beta == ((alpha^count) %% p)){
      return(count)
      break
    }else {
      count <- count + 1
      }
    } else {
      return(NULL)
    }
  }
}

#' Next Primitive Root
#'
#' Finds the next primitive root after \code{after} modulo \code{p}.
#'
#' @param p A prime number. Could be \code{bigz}.
#' @param after Number after which to start testing. Defaults to 1.
#'
#' @return The smallest primitive root modulo \code{p} greater than \code{after}. Applies
#' the procedure described in Exercise 21 on p. 107 of [Trappe].
#' @export
#' @import gmp
#'
#' @examples
#' nextPrimRoot(601) # should be 7
#' nextPrimRoot(nextprime(as.bigz("1203481092840918409408098")), 200) # should be 203
nextPrimRoot <- function(p, after = 1){
 x <- unique(factorize(p-1))
alpha <- as.bigz(after) + 1

 while(alpha < p){
   for (i in 1:length(x)){
     if(powm(alpha, (p-1)/(x[i]), p) == (p-1)){
       return(alpha)
     }
   }
   alpha <- alpha + 1
 }
}

#' Solve a system using the Chinese Remainder Theorem
#'
#' Given a system of i congruences of the form x = a_i mod m_i, returns
#' the smallest positive x that satisifies all the congruences. Applies the
#' Chinese Remainder Theorem, following the procedure outlined
#' in [Trappe], p. 108, Problem 24. Works for bigz integers.
#'
#' @param a A vector of i integers (or bigz integers)
#' @param m A vector of i moduli (as integers or as bigz)
#'
#' @return A bigz integer solutions to the system.
#' @export
#' @import gmp
#'
#' @examples
#' crtSolve(c(2,1,3), c(5,6,7))
crtSolve <- function(a, m){
  bigm <- 1
  z <- as.bigz(c())
  y <- as.bigz(c())
  x <- 0

  for (i in 1:length(m)){
    bigm <- bigm * m[i]
  }
  for (i in 1:length(m)){
    z[i] <- bigm / m[i]
  }
  for (i in 1 : length(z)){
    y[i] <- inv.bigz(z[i], m[i])
  }
  for (i in 1: length(m)){
    x <- x + a[i] * z[i] * y[i]
  }
  return (x %% bigm)
}
