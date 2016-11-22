############################################################
# (Morrison, 2005) 5.3 Tests for Two Special Patterns
############################################################
TSP <- function(S, n) {
   v <- n-1
   p <- nrow(S)
   s2 <- (1/p)*sum(diag(S))
   S2r <- (1/(p*(p-1)))*sum(S-diag(S))
   r <- 1+(S2r/s2)
   L <- (det(S)/((s2^p)*((1-r)^(p-1))*(1+((p-1)*r))))
   chi <- ( -(v - (((p*(p+1)^2)*(2*p-3))/(6*(p-1)*(p^2*p-4)))) )*log(L)
   df <- (1/2)*p*(p-1)-1
   pValue <- 1-pchisq(q = chi, df = df)

   TSP <- list("ChiSquareStatistic" = chi, "pValue" = pValue)

   return(TSP)
}
