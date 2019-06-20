#' Assemble a coastline segment with repeating MPAs

#' @param Fr fraction of coastline in reserve
#' @param Rw reserve width (relative to dispersal distance)
#' @export
#' @example 
#' assemble.coastline

assemble.coastline <- function(Fr,Rw){
  
  if (Fr > 0 & Rw > 0){
  # Assume that dispersal distance = 1
 # Frac = 1/Fr # this will be the length of the segment in MPAs
  Facts = 1:100
  Rem = (Fr*Facts)%%1
  Fact = Facts[Rem==min(Rem)][1]
  
  # The coastline (1's for MPA, 0's for fished)
  X <- c(rep(1,round(Rw*Fact,1)), 
             rep(0,round(Rw*Fact/Fr-Rw*Fact)))
  
  # Sigma, the effective dispersal distance to make everything work
  Sigma <- Fact}
  else{
    
  X <- 0
  Sigma <- 1
  
  } # end if
  
  Output <- list(X=X,Sigma=Sigma)
  return(Output)
  
} # End function