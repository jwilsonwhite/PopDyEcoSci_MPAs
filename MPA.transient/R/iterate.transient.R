#' Function to iterate an age-structured matrix model before and after MPA implementation
#' Leslie matrix LF (with fishing), L0 (without)

#' @param LF Leslie matrix with fishing
#' @param L0 Leslie matrix without fishing
#' @export
#' @examples 
#' iterate.transient


iterate.transient<-function(LF,L0){
  
  T = 30 # years post MPA
  Tinit = 100 # years pre-MPA
  A = dim(L0)[1] # number of age classes
  
  N0 = abs(Re(eigen(L0)$vector[,1])) # unfished SAD
  Nf = abs(Re(eigen(LF)$vector[,1])) # fished SAD
  
  N <- matrix(0,nrow=A,ncol=T)
  N[,1]=Nf
  for (t in 2:T){
    
    N[,t]=L0%*%N[,t-1]
  } # end loop
  
  # growth rates
  NN = colSums(N)
  Ls=  NN[2:T]/NN[1:(T-1)]

  return(list(N=N,Ls=Ls))
  
  
}