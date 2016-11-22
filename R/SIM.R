############################################################
# (Morrison, 2005) 2.3 Simultaneous Inferences for Means
############################################################

SIM <- function(xbar, mu, a, N, S, alpha = 0.05, intervals = "default", m ) {
   if(missing(m)) {
      m <- sum(a)
   }
   p <- length( xbar )
   if( intervals == "default" ) {
      T2 <- ( N - 1 )*p / ( N - p ) * qf( p = 1-alpha, p, N - p )
      ax <- t(a)%*%xbar
      SR <- sqrt( ( 1 / N )* (t( a ) %*% S %*% a) * T2 )
      limInf <- ax - SR
      limSup <- ax + SR
   } else if( intervals == "bonferroni" ) {
      tTheo <- qt(p = 1-(alpha/(2*m)), df = N-1)
      ax <- t(a)%*%xbar
      SR <- sqrt( ( 1 / N )* (t( a ) %*% S %*% a) * tTheo )
      limInf <- ax - SR
      limSup <- ax + SR
   } else {
      stop( 'The method to construct the intervals must be "default" or "bonferroni"' )
   }
   
   SIM <- data.frame("InfLim" = limInf, "ax" = ax, "SupLim" = limSup)
   
   return(SIM)

}
