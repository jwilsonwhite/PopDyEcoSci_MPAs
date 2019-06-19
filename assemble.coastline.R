# Assemble a coastline segment with repeating MPAs
# Inputs:
# Fr = fraction of coastline in reserve
# Rw = reserve width (relative to dispersal distance)

assemble.coastline <- function(Fr,Rw){
  
  if (Fr > 0 & Rw > 0){
  # Assume that dispersal distance = 1
  Frac = 1/Fr # this will be the length of the segment in MPAs
  Facts = 1:20
  Rem = Frac%%Facts
  Fact = Facts[Rem==min(Rem)][1]
  
  # The coastline (1's for MPA, 0's for fished)
  X <- rbind(rep(1,round(Rw*Fact,1)), 
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