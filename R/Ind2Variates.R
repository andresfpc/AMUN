############################################################
# (Morrison, 2015) 5.5 Independence of 2 Sets of Variates (pag 254)
# TODO: generalize it
############################################################

Ind2Variates <- function(S, S11, S22, n) {
  V <- (det(S))/(det(S11)*(det(S22)))
  Sigma_3 <- (nrow(S11)+nrow(S22))^3-((nrow(S11)^3+(nrow(S22)^3)))
  Sigma_2 <- (nrow(S11)+nrow(S22))^2-((nrow(S11)^2+(nrow(S22)^2)))
  f <- (1/2)*Sigma_2
  invC <- 1-(1/((12*f)*(n-1)))*(2*Sigma_3+3*Sigma_2)
  chi <- (-((n-1)*invC)*log(V))
  pvalue <- pchisq(chi,f)
  
  Ind2Variates <- list("ChiSquareStatistic" = chi, "DegreesOfFreedom" = f,"p-value" = pvalue)

  return(Ind2Variates)
}