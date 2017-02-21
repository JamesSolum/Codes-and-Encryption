#' Encrypt a string using a shift cipher
#'
#' @param plainText A string of lowercase letters
#' @param shift An integer (mod 26) to shift by
#'
#' @return An encrypted (or decrypted) string, where each letter has been shifted by \code{shift} places.
#' @export
#'
#' @examples
#' plainText <- "thisisasupersecretmessagethatwewanttoencrypt"
#' cipherText <- shiftCipher(plainText, 5)
#' print(cipherText)
#' print(shiftCipher(cipherText, 21)) # should be original plaintext
shiftCipher <- function(plainText, shift){
  pt <- stringToMod26(plainText)
  ct <- (pt + shift) %% 26  # encrypt by shifting
  return(mod26ToString(ct))
}
