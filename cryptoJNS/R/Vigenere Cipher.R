
#' Skip String
#'
#' @param txt a string you wish to analyze
#' @param n an integer that represents your initial position
#' @param r an integer you wish to skip through the string by
#'
#' @return a string of letters that were r length apart
#' @export
#'
#' @examples skipString("hellohowareyou", 1, 2)
skipString <- function(txt, n, r) {
  l <- unlist(strsplit(txt,""))
  ss <- l[seq(n,length(l),r)]
  return(paste0(ss,collapse=""))
}



#' Shift Vector
#'
#' @param v Takes a vector v
#' @param n an integer n to shift each element of the vector
#'
#' @return a vector
#' @export
#'
#' @examples shiftVec(c(1,1,2,3), 5)
shiftVec <- function(v, n) {
  v[(seq_along(v) - (n+1)) %% length(v) + 1]
}



#' Count matches within a text.
#'
#' @param ct a string of crypted text
#' @param shift an integer to shift by
#'
#' @return a table that counts the amount of matches
#' @export
#'
#' @examples countMatches("hello", 5)
#'
countMatches <- function(ct, shift){
  ct <- stringToMod26(ct)
  matches <- sapply(1:shift, function(x){sum(ct-shiftVec(ct, x)==0)})
  names(matches) <- 1:shift
  return(matches)
}



#' Find the letter frequency of a text.
#'
#' @param txt a string to analyze
#'
#' @return a vector of frequencies for each letter of the alphabet (in alphabetical order)
#' @export
#'
#' @examples letterFreq("hello")
letterFreq <- function(txt) {
  l <- unlist(c(strsplit(txt,""),letters))
  t <- as.vector(table(l))-1
  return(t/sum(t))
}


#' Find Vigenere Key
#'
#' @param ciphertext a vigenere encrypted string
#' @param keyLength the length of the key to the vigenere cipher
#'
#' @return an integer that is the length of the string.
#' @export
#'
#' @examples findVigKey("zyupzvjslelzkovevveobtwkjoqdlvokqyskvoqemkmdczjyacsdyrdcyretadwcrefxlyrefxlmyklbsdxzfqftqjwvdeskzoncsmancy", 6)
findVigKey <- function(ciphertext, keyLength){
  vKey <- numeric(keyLength) # preallocate a vector of the desired length
  englishFreqs <- c(0.082,0.015,0.028,0.043,0.127,0.022,0.020,0.061,0.070, 0.002,0.008,0.040,0.024,
                    0.067,0.075,0.019,0.001,0.060,0.063,0.091,0.028,0.010,0.023,0.001,0.020,0.001)

  for(i in 1:keyLength){
    v <- letterFreq(skipString(ciphertext,i,keyLength))
    matchFreqs <- sapply(0:25, function(i){v %*% shiftVec(englishFreqs, i)})
    vKey[i] <- which.max(matchFreqs)-1
  }
  return(mod26ToString(vKey))
}
