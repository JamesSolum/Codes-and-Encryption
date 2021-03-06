% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/real_elliptic.R
\name{ecAddReal}
\alias{ecAddReal}
\title{Addition in Real Elliptic Curves}
\usage{
ecAddReal(b, c, p1 = c(NA, NA), p2 = c(NA, NA))
}
\arguments{
\item{b}{A real number representing the coefficient b in the equation of the curve.}

\item{c}{A real number representing the coefficient c in the equation of the curve.}

\item{p1}{A length 2 numeric vector, representing a point on the curve.}

\item{p2}{A length 2 numeric vector, representing a point on the curve.}
}
\value{
A length 2 numeric vector, representing the sum p1 + p2.
}
\description{
Adds two points on the real elliptic curve y^2 = x^3 + bx + c. Uses the Addition Law
given on page 352 of [Trappe]. All points on elliptic curves are represented as length
two numeric vectors: the point at infinity is represented as \code{c(NA,NA)}. This function
does no error trapping, so \code{p1} and \code{p2} are assumed to be valid points on
the elliptic curve.
}
\examples{
ecAddReal(0, 73, c(2,9), c(3,10)) # see page 350
ecAddReal(0, 73, c(2,9), c(NA,NA)) # c(NA,NA) is the additive identity
ecAddReal(0, 73, c(2,9), c(2,-9))
}

