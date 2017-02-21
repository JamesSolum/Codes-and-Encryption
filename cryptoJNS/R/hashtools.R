#' Miniature SHA hash
#'
#' Implements a 32-bit hash function by taking the first 32 bits of the SHA-1 hash.
#'
#' @param x A character string.
#'
#' @return A four-character hash string.
#' @export
#' @import openssl
#'
#' @examples
#' miniSHA("Corned Beef and Haggis") # should be "de0d"
miniSHA <- function(x) {
  x <- sha1(charToRaw(x))
  x <- paste(x[1], x[2], sep = "")
  return(x)
}

#' MD5 block cipher
#'
#' This block cipher encrypts in 128-bit blocks. Encrypts a raw vector using a variation of
#' OFB mode. (Certainly not very secure.) The initial vector \code{iv} is converted to
#' a raw vector representing its MD5 hash. This raw vector becomes X_1. If the blocks of
#' \code{m} are P_1, P_2, ..., P_k, then O_i is the MD5 hash of X_i using \code{k} as the
#' HMAC key (as a string), where X_(i+1) = O_i. The ciphertext
#' block C_i is the exclusive-or of O_i and P_i.
#'
#' @param m A raw vector, representing the message to be encrypted/decrypted.
#' Assumed to have length a multiple of 16 (i.e., the message length is a multiple
#' of 128-bits).
#' @param ky A string, used as the symmetric key.
#' @param iv A string, used as an initial vector.
#'
#' @return A raw vector of the same length as \code{m}
#'
#' @export
#' @import openssl
#'
#' @examples
#' testct <- "This block cipher is for educational purposes only. Learn well!!"
#' ct <- md5Cipher(charToRaw(testct), "Runner4567", "init vector") # should end in: 78 be 24 73
#' rawToChar(md5Cipher(ct, "Runner4567", "init vector")) # decryption
md5Cipher <- function(m, ky, iv){

  if((length(m) %% 16) != 0)
    stop("Message length is not a multiple of 128-bits.")


  numOfBlks <- length(m)/16
  x <- md5(charToRaw(iv))
  o <- raw(length = length(m))
  ct <- raw(length = length(m))

  o[1:16] <- md5(x, ky)


    for (i in 1:numOfBlks){
      x <- o[((16*(i-1))+1):(16*i)]
      o[(((16*(i-1))+1)+16):((16*i)+16)] <- md5(x, ky)
    }

  for (i in 1:numOfBlks){
    ct[((16*(i-1))+1):(16*i)] <- xor((m[((16*(i-1))+1):(16*i)]), (o[((16*(i-1))+1):(16*i)]))
  }

  return(ct)
}
