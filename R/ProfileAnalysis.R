############################################################
# (Morrison, 2005) 2.7 Profile Analysis for Two Independent Group
############################################################

ProfileAnalysis <- function(dat1, dat2, C, alpha = 0.05 ) {
  if(ncol(dat1)!=ncol(dat2)){
    stop("Matrices must have the same column space\n")
  }
    means1 <- colMeans(dat1)
    means2 <- colMeans(dat2)
    mi <- min(c(means1,means2))
    ma <- max(c(means1,means2))
    plot(means1, type = "l", col="darkgreen", ylim = c(mi,ma),ylab = "Group Mean", main="Check Parallelism")
    lines(means2, col = "red ")
    ##  Test of Parallelism
    n1 <- nrow(dat1)
    n2 <- nrow(dat2)
    S1 <- var(dat1)
    S2 <- var(dat2)
    S <- (n1*S1+n2*S2)/(n1+n2-2)
    N <- ((n1*n2)/(n1+n2))
    md <- means1-means2
    T2 <- ((n1*n2)/(n1+n2))*(t(md)%*%t(C)%*%(solve(C%*%S%*%t(C)))%*%C%*%md)
    p <- nrow(S)
    Fc <- ((n1+n2-p)/((n1+n2-2)*(p-1)))*T2
    pvalueph <- pf(Fc,p-1,n1+n2-p,lower.tail = F)
    if(pvalueph < alpha) {
       return(list("P value of the Parallesim Hypothesis"=as.numeric(pvalueph), "Advice" = c( "The parallelism hypothesis had been rejected" ) ) )
    } else {
      ## Same level
      j <- rep(1,length(means1))
      t <- (t(j)%*%md)/(sqrt(t(j)%*%S%*%j)*(1/n1+1/n2))
      pvt <- pt(t,n1+n2-2,lower.tail = F)
      ## horizontales
      xbp <- (n1/(n1+n2))*means1+(n2/(n1+n2))*means2
      T2h <- (n1+n2)*(t(xbp)%*%t(C)%*%solve(C%*%S%*%t(C))%*%C%*%xbp)
      Fch <- ((n1+n2-p)/((n1+n2-2)*(p-1)))*T2h
      pfh <- pf(Fch,p-1,n1+n2-p,lower.tail = F)
      
      ProfileAnalysis <- list("ParallelismHypothesis_pValue" = as.numeric(pvalueph), "TestSameLevel_pValue"=as.numeric(pvt),"Directionality_pValue" = as.numeric(pfh))

      return(ProfileAnalysis)
   }

}

