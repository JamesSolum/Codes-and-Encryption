#' Encrypt a string using a shift cipher
#'
#' @param plainText A string of lowercase letters
#' @param stretch An integer used to stretch the function.
#' @param shift An integer (mod 26) used to shift.
#'
#' @return An encrypted (or decrypted) string, where each letter has been shifted by \code{shift} and stretched by \code{stretch} places.
#' @export
#'
#' @examples
#' plainText <- "dog"
#' cipherText <- affineCipher(plainText,3,7)
#' print(cipherText)
#' You cannot decrypt a cipherText using the encryptAffineCipher function
affineCipher <- function(plainText,stretch,shift){
  pt <- stringToMod26(plainText)
  ct <- ((pt*stretch) + shift) %% 26  # encrypt by stretching and shifting
  return(mod26ToString(ct))
}
