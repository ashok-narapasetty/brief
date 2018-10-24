#' Brief function
#'
#' A random brief function
#'
#' @param x a numerical vector
#' @keywords Brief
#' @export
#' @example
#' brief()

brief = function(x){
 return(data.frame(mean = mean(x), sd = sd(x)))
}
