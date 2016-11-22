############################################################
# (Morrison, 2005) 2.6 Groups of Repeated Measurements:
#  The Paired T^2 Test (case of 2 conditions)
############################################################

GRM <- function(x1, x2, N, S11, S22, S12) {
    mv1 <- colMeans(x1)
    mv2 <- colMeans(x2)
    S <- (S11+S22-S12-t(S12))
  p <- ncol(S)
  T2 <- N*(t(mv1-mv2))%*%solve(S)%*%(mv1-mv2)# hay un problemas en el ejemplo proque resulta no sigular
  Fc <- ((N-p)/((N-1)*p))*T2
  pvalue <- pf(q = Fc,df1 = p,df2 = N-p)
  
  return(list('FStatistic'=Fc,'pValue'=pvalue))
}


