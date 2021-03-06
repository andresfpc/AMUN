############################################################
# (Morrison, 2005) 5.5 Independence of 2 Sets of Variates (pag 254)
# TODO: generalize it
############################################################

Ind2Variates <- function(S, n) {
	S11 <- S[1:2, 1:2]
	S12 <- S[1:2, 3:4]
	S22 <- S[3:4, 3:4]

   V <- (det(S))/(det(S11)*(det(S22)))
   Sigma_3 <- (nrow(S11)+nrow(S22))^3-((nrow(S11)^3+(nrow(S22)^3)))
   Sigma_2 <- (nrow(S11)+nrow(S22))^2-((nrow(S11)^2+(nrow(S22)^2)))
   f <- (1/2)*Sigma_2
   C <- 1-(1/((12*f)*(n-1)))*(2*Sigma_3 + 3*Sigma_2)
   chi <- (-(n-1)*log(V))/C
   pValue <- 1-pchisq(chi, f)

   Ind2Variates <- list("S11" = S11, "S22" = S22, "ChiSquareStatistic" = chi, "DegreesOfFreedom" = f,"pValue" = pValue)

   return(Ind2Variates)
}

