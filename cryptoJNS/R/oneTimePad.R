#' One Time Pad function will encrypt and decrypt messages according to 1 time pad algorithm
#'
#' @param pt The plaintext, represented as a string, that you would like to encrypt.
#' @param padkey A key that is a string and is equal in length to the plaintext.
#'
#' @return A string.
#' @export
#'
#' @examples oneTimePad("james", "kdlaf")
#'
oneTimePad <- function(pt, padkey){
    ct <- xor(charToRaw(pt), charToRaw(padkey))
    return(rawToChar(ct))
}
