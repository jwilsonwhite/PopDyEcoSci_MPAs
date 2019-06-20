#' Plot the results of the matrix model
#' 
#' @param D results object from iterate.model
#' @param L Leslie matrix
#' @export
#' @examples 
#' plot.results

plot.results <- function(D,L){
  
  # 4 panels
  # NW: trend over time
  # NE: age distributions
  # SW: growth rate over time
  # SE: eigenvalues
  
  NN = colSums(D$N)
  NN = NN/NN[1]
  T  = 0:(length(NN)-1)
  A = 1:(dim(D$N)[1])
  D1 = data.frame(T=T,NN=NN)
  D2 = data.frame(A=A,Y0=D$N[,1],Y5=D$N[,6],Y10=D$N[,11],Y30=D$N[,30])
  D2.long = melt(D2,id.vars='A',variable.name='year',value.name = 'N')
  D3 = data.frame(T=T[1:(length(T)-1)],L=D$Ls)
  
  # get eigenvalues
  E = eigen(L)$values[1:2]
  E.frame1 = data.frame(Re = c(0,Re(E[1])),Im=c(0,Im(E[1])))
  E.frame2 = data.frame(Re = c(0,Re(E[2])),Im=c(0,Im(E[2])))
  # make a circle
  Theta = seq(from=0,to=2*pi,length.out=1e3)
  R = 1
  x = R*cos(Theta)
  y = R*sin(Theta)
  Circ = data.frame(x=x,y=y)
  Circ=Circ[order(Circ$x),]
  
  # Period of cycles:
  Angle = acos(Re(E[2])/abs(E[2]))
  Period = 2*pi/Angle
  
  # Deviation from SAD:
  V1 = abs(eigen(L)$vectors[,1])
  VF = abs(D$N[,1])
  Dot = sum(V1*VF) # dot product
  Norm1 = sqrt( sum( V1^2 ) )
  Norm2 = sqrt( sum( VF^2 ) )
  Dev = acos( Dot/ (Norm1 * Norm2) )/(2*pi)*360
  
  
  
  Trend <- ggplot(data=D1,aes(x=T,y=NN))+
                    geom_point(size=2)+
                    geom_line(size=1)+
                    ylab("Population density, relative to N(0)")+
                    xlab("Time after MPA implementation (y)")+
                    theme_bw()
                  
  
  Ages <- ggplot(data=D2.long,aes(x=A,y=N,color=year))+
           geom_line(size=1)+
           ylab("Abundance")+
           xlab("Age class (y)")+
           theme_bw()
  
  Growth <- ggplot(data=D3,aes(x=T,y=L))+
    geom_point(size=2)+
    geom_line(size=1)+
    ylab("Population growth rate")+
    xlab("Time after MPA implementation (y)")+
    theme_bw()
  
  Complex <- ggplot()+
    geom_line(data=E.frame1,aes(x=Re,y=Im))+
    geom_line(data=E.frame2,aes(x=Re,y=Im))+
    geom_point(data=Circ,aes(x=x,y=y),size=0.01,color='gray')+
    ylab("Imaginary part")+
    xlab("Real part")+
    coord_equal()+
    theme_bw()
  
  # add in returning the period (2*pi/Theta)
  ggarrange(Trend,Ages,Growth,Complex,nrow=2, ncol = 2)
  
  return(list(Period=Period,Dev=Dev))
} # end function