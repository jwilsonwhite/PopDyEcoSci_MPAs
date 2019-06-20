#' Create connectivity matrix for MPA persistence calculations
#' 
#' @param X the coastline vector (this will define the size of the matrix)
#' @param Mu the mean dispersal kernel
#' @param Sig the SD of dispersal kernel
#' @export
#' @example 
#' dispersal.matrix

dispersal.matrix <- function(X,Mu,Sig){
  
  x = length(X)
  xx = 1:x
  X = matrix(xx,nrow=x,ncol=x)
  Dist = X - t(X) # inter-patch distances
  
  D = matrix(0,nrow=x,ncol=x)
  
  # loop over several times to simulate an 'infinite' coastline
  for (i in -10:10){
    # integrate dispersal kernel for each interpatch distance
    D = D + pnorm(Dist+0.5+x*i,Mu,Sig) - pnorm(Dist-0.5+x*i,Mu,Sig)
  } # end for loop
  
  return(D)
} # end function