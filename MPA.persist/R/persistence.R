#' Determine whether a coastline with MPAs is persistent or not

#' @param X landscape
#' @param D dispersal matrix
#' @param CRT minimum value of FLEP for replacement
#' @param FLEP.out value of FLEP in fished patches
#' @export
#' @example 
#' persistence

persistence <- function(X,D,CRT,FLEP.out){
  
  FLEP.vec = X
  FLEP.vec[X==0] = FLEP.out # put in correct value of FLEP in fished areas
  FLEP.mat = t(matrix(FLEP.vec,nrow=length(X),ncol=length(X))) # make this a matrix of production in each patch
  
  C = D * FLEP.mat / CRT # full projection matrix. dispersal x production / minimum replacement rate
  
  E = max(abs(eigen(C)$values))
  
  return(E>1)
} # end function