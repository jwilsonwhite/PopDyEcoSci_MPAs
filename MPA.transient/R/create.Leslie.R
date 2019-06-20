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
 # E = 2.195e-6*B*((1:A)>=Amat) # eggs
  E = B*((1:A)>=Amat) # eggs
  
  # adjust the coefficient to get lambda = 1.02
  L1 = cbind(Surv,(rep(0,A-1)))
  L1 = rbind(E,L1)
  o = suppressWarnings(optim(1e-6,optim.Leslie,L=L1,Target=1.02,method='Nelder-Mead'))
  # now get correct matrix:
  L0  = cbind(Surv,(rep(0,A-1)))
  L0 = rbind(E*o$par,L0)
  
  #max(abs(eigen(L0)$values))
  
  
  # Now return the fished matrix matrix:
  LF = cbind(SurvF,(rep(0,A-1)))
  LF = rbind(E*o$par,LF)
  
  return(list(L0=L0,LF=LF))
  
  
} # end function



