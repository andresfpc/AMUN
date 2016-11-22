############################################################
# (Simar) 7.1 Test problem 3
############################################################

SingleCovComp <- function( Sigma_0, Sigma, n, alpha = 0.05 ) {
   p <- nrow( Sigma )
   df <- (1/2)*p*(p+1)
   
   Sigmahat <- ( (n-1)/n )*Sigma
   prodSigma <- solve( Sigma_0 )%*%Sigmahat
   estTest <- n * mTrace( prodSigma ) - n * log( det( prodSigma ) ) - n * p
   chi2 <- qchisq( p = 1-alpha, df = df, lower.tail = F )
   pValue <- pchisq( q = estTest, df = df, lower.tail = F )
   
	SingleCovComp <- list( "LStatistic" = estTest, df = df, chi2 = chi2, pValue = pValue )

   return(SingleCovComp)
}
