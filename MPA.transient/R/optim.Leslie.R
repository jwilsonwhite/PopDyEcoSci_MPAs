#' find the value of fecundity in a Leslie matrix that gives a particular lambda1 value
#' 
#' @param x multiplier (to be modified by optim)
#' @param L Leslie matrix
#' @param Target target eigenvalue
#' @export
#' @example 
#' optim.Leslie


optim.Leslie <- function(x,L,Target){
  
  L[1,] = L[1,]*x
  
 Out  = abs(Target - Re(eigen(L)$values[1]))
  return(Out)
}