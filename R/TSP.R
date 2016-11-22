############################################################
# (Morrison, 2005) 5.3 Tests for Two Special Patterns
############################################################
TSP <- function(X) {
  S <- var(X)
  n <- nrow(X)
  r <- ncol(X)
  p <- nrow(S)
  s2 <- sum(diag(S))
  Smd <- S-diag(S)
  S2r <- sum(colSums(Smd))
  L <- (det(S)/((s2^p)*((1-r)^(p-1))*(1+((p-1)*r))))
  chi <- ( -(n-(((p*(p+1)^2)*(2*p-3))/(6*(p-1)*(p^2*p-4)))) )
  df <- (1/2)*p*(p+1)
  pvalue <- pchisq(q = chi,df = df)

  TSP <- list("ChiSquareStatistic" = chi, "pValue" = pvalue)

  return(TSP)
}