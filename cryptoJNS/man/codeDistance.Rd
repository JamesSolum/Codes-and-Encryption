% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codetools.R
\name{codeDistance}
\alias{codeDistance}
\title{Minimum distance of a code}
\usage{
codeDistance(C)
}
\arguments{
\item{C}{A code, represented as a matrix of 0's and 1's. The rows
of this matrix are the words of the code.}
}
\value{
The minimum distance d(C) of the code.
}
\description{
Compute the smallest Hamming distance between two words of a code.
}
\examples{
codeDistance(matrix(c(0,1,0,0,1, 0,0,1,1,1, 0,1,0,0,0, 1,1,1,1,1), nrow=4, byrow=TRUE))
}

