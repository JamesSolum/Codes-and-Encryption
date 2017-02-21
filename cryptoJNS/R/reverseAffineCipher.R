#' Decrypt a string with a known affine cipher parameters
#'
#' @param cryptedtext A string of lowercase letters
#' @param stretch An integer (mod 26) to stretch by
#' @param shift An integer (mod 26) to shift by
#'
#' @return A decrypted string, where each letter has been shifted by \code{shift} and stretched \code{stretch}
#' @export
#'
#' @examples
#' plainText <- "cat"
#' cipherText <- "smr"
#' print(plaintext)
decryptAffineCipher <- function(cryptedText,stretch,shift){
  ct <- stringToMod26(cryptedText)
  pt <- ((ct - shift)*stretch) %% 26 # decrypt by negating the shift and stretching by the inverse
  return(mod26ToString(pt))
}



