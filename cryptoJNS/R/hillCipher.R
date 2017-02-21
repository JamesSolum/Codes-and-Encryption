#' Encrypts messages according to the Hill Cipher algorithm.
#'
#' @param txt The plaintext represented as a string you would like to enrypt.
#' @param keyMatrix A matrix that will be used to encrypt the plaintext.
#'
#' @return A string
#' @export
#'
#' @examples hillCipher("hello", matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3, byrow=TRUE))

#'
hillCipher <- function(txt, keyMatrix) {
  pt <- stringToMod26(txt)
  n <- attributes(keyMatrix)$dim[2]
  suppressWarnings(
    mPtxt <- matrix(pt,nrow=n) # repeats text so length is a multiple of n
  )
  mCtxt <- (keyMatrix %*% mPtxt) %% 26
  return(mod26ToString(as.vector(mCtxt)))
}

