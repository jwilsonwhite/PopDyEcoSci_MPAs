# This function will calculate whether a given MPA configuration on an infinite coastline will persist (deterministically)

# Input parameters:
# Fr = fraction of coastline in MPA
# Rw = reserve width (relative to larval dispersal distance)
# CRT = critical replacement threshold
# FLEP.out = fraction of lifetime egg production in fished area (assume FLEP = 1 in MPA)

MPA_persist <- function(Fr,Rw,CRT,FLEP.out){
  
  # Assemble coastline
  Coast <- assemble.coastline(Fr,Rw)
  X <- Coast[1]
  Sig <- Coast[2]
  
  # Assemble connectivity matrix:
  D = dispersal.matrix(X,0,Sig)
  
  MPA_persist = persistence(D,CRT,FLEP.out)
  
} # end function