#' AES decrypter
#'
#' @param msg The encrypted message.
#' @param key The key in string form (NOT RAW)
#'
#' @return A decrypted message.
#' @export
#'
#' @examples
#'
#'
#'
decryptAES <- function(msg, key){
  rawKey <- charToRaw(key)
  myAESobject <- AES(rawKey, mode="ECB")
  dt <- myAESobject$decrypt(msg, raw=TRUE)
  print(dt)
}
