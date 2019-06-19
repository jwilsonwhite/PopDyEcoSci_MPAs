# This function will calculate whether a given MPA configuration on an infinite coastline will persist (deterministically)

# Input parameters:
# Fr = fraction of coastline in MPA
# Rw = reserve width (relative to larval dispersal distance)
# CRT = critical replacement threshold
# FLEP.out = fraction of lifetime egg production in fished area (assume FLEP = 1 in MPA)

MPA_persist <- function(Fr,Rw,CRT,FLEP.out){
  
  # Assemble coastline
  Coast <- assemble.coastline(Fr,Rw)
  
  # Assemble connectivity matrix:
  D = dispersal.matrix(Coast$X,0,Coast$Sig)
  
  MPA_persist = persistence(Coast$X,D,CRT,FLEP.out)
  
} # end function