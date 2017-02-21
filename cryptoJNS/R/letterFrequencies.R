#' Count the Letters
#'
#' @param txt A string you would like to analyze
#'
#' @return A table that describes the amount of repeated letters you have in your txt
#' @export
#'
#' @examples letterCounts("hello")
letterCounts <- function(txt)
{
  return(sort(table(unlist(strsplit(txt,""))),decreasing=TRUE))
}

#' Creates a Digram table from a text.
#'
#' @param txt The text you wish the function to analyze.
#'
#' @return a table with the number of letter combinations
#' @export
#'
#' @examples digramTable("hellohowareyou")
digramTable <- function(txt) # returns a table with the numbers of digrams of each possible type
{
  l <- unlist(strsplit(txt,""))
  dgs <- data.frame(l,c(l[2:length(l)],NA))
  names(dgs) <- c("first","second")
  table(dgs)
}

#' Encrypt using a Vigenere Cipher.
#'
#' @param txt A string you would like to encrypt.
#' @param keyVector A vector to encrypt your string.
#'
#' @return An encrypted string
#' @export
#'
#' @examples vigenere("helloworld", "at")
vigenere <- function(txt, keyVector)
{
  pt <- stringToMod26(txt)
  suppressWarnings(
    ct <- (pt + keyVector) %% 26
  )
  return(mod26ToString(ct))
}
