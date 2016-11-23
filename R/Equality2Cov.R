############################################################
# (Morrison, 2015) 5.4 Testing the Equality of Several Covariance Matrices
############################################################

Equality2Cov <- function(S, S1, S2, n1, n2) {
	M <- (n1+n2-1)*log(det(S))-(n1-1)*log(det(S1))-(n2-1)*log(det(S2))
	p <- nrow(S)
	k <- 2
	invC <- 1-(2*(p^2)+3*(p-1))/(6*(p+1)*(k-1))
	chi <- M*invC
	df <- (k-1)*(p)*(p+1)/2
	pValue <- 1-pchisq(chi, df)

	Equality2Cov <- list("ChiSquareStatistic" = chi, "DegreeOfFreedom" = df, "pValue" = pValue)

	return(Equality2Cov)
}

