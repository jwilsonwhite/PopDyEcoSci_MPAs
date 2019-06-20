#' This function will calculate whether a given MPA configuration on an infinite coastline will persist (deterministically)

#' @param Fr fraction of coastline in MPA
#' @param Rw reserve width (relative to larval dispersal distance)
#' @param CRT critical replacement threshold
#' @param FLEP.out fraction of lifetime egg production in fished area (assume FLEP = 1 in MPA)
#' @export
#' @example 
#' MPA_persist

MPA_persist <- function(Fr,Rw,CRT,FLEP.out){
  
  # Assemble coastline
  Coast <- assemble.coastline(Fr,Rw)
  
  # Assemble connectivity matrix:
  D = dispersal.matrix(Coast$X,0,Coast$Sig)
  
  MPA_persist = persistence(Coast$X,D,CRT,FLEP.out)
  
} # end function