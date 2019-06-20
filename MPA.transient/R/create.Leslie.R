#' Create a Leslie matrix
#' Currently coded with pre-determined parameters, lambda set to be 1.02
#' @param M natural mortality rate
#' @param F fishing mortality rate
#' @export
#' @examples 
#' create.Leslie


create.Leslie <- function(M,F){
  
  A = 30 # age classes
  Afish = 4
  Amat = 4
  isFish.surv = (1:(A-1))>=Afish
  Linf = 100
  k = 0.1
  
  # survival projection matrices
  Surv = diag(A-1)*exp(-M) # natural mortality only in this one
  SurvF = diag(exp(-(M + isFish.surv*F)))
  
  # fecundity vector
  L = Linf*(1 - exp(-k*(1:A))) # length
  B = L^3 # biomass
  E = 2.195e-6*B*((1:A)>=Amat) # eggs
  
  # This is some code used to adjust the coefficient above to get lambda = 1.02...
  # This is obviously a crude way to do it, but it's just a hard-coded example for students
  L1 = cbind(Surv,(rep(0,A-1)))
  L1 = rbind(E,L1)
  max(abs(eigen(L1)$values))
  
  # Now return the actual matrix:
  LF = cbind(SurvF,(rep(0,A-1)))
  LF = rbind(E,LF)
  
  return(LF)
  
  
} # end function