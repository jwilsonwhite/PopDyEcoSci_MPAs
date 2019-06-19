# Transient analysis of post-MPA dynamics
# Uses a pre-determined age-structured matrix model
# Inputs: natural mortality rate, M; pre-MPA fishing rate, F

MPA.transient <-function(M,F){
  
  # Create Leslie matrix for fishing case
  LF <- create.Leslie(M,F)
  # Create Leslie matrix for no-fishing case
  L0 <- create.Leslie(M,0)
  
  # Iterate model
  N <- iterate.transient(LF,L0)
  
  # Plot results
  plot.results(N,L0)
  

} # end function