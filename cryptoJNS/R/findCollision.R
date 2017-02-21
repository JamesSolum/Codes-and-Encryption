#' Find a collision for a minisha
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
findCollision <- function(string){
  x <- miniSHA(string)
  for(i in 1:70000){
    random <- mod26ToString(as.integer(runif(nchar(string), 1, 26)))
    if(x == miniSHA(random)){
      print(random)
    }
  }
}

