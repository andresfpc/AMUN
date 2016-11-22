############################################################
# (Morrison, 2005) 2.4 The case of two samples
############################################################
TwoMeanComparison <- function( xbar, ybar, nx, ny, S, alpha = 0.05 ) {
   # Mean comparison for two independent sample with
   # confidence intervals for each variable
   # Args:
   #  xbar: Mean vector of X group
   #  ybar: Mean vector of Y group
   #  nx: size of X group
   #  ny: size of Y group
   #  S: covariance matrix
   #  alpha: significance level
   #
   nxy <- nx + ny
   nVar <- length( xbar )
   varDif <- xbar - ybar
   diagS <- diag( S )

   T2 <- ( ( nx*ny ) / nxy ) * ( t( varDif ) %*% solve( S ) %*% varDif )

   FCalc <- as.vector( T2 * ( ( nxy-nVar-1 ) / ( ( nxy-2 ) * nVar ) ) )

   FTheo <- qf( p = 1-alpha, df1 = nVar, df2 = nxy-nVar-1 )

   TTest <- FTheo * ( ( nxy-2 ) * nVar / ( nxy-nVar-1 ) )

   SE <- sqrt( TTest * diag( S ) * nxy / ( nx*ny ) )
   Tcd <- varDif / sqrt( diag( S ) *( (1/nx) + (1/ny) ) )
   pvalue <- pf( Tcd^2 * ( (nxy-nVar-1)/(nxy-2)/nVar ), nVar, nxy-nVar-1, lower.tail = FALSE)

   IC <- data.frame( varDif, varDif - SE, varDif + SE, Tcd, pvalue )
   rownames( IC ) <- names( xbar )
   colnames( IC ) <- c( "MeanDifference",
      paste( "Lower ", alpha/2, "%", sep = "" ),
      paste( "Upper ", 1-(alpha/2), "%", sep = "" ),
      "Tcd", "pValue" )

   TwoMeanComparison <- list( FCalc = FCalc, FTheo = FTheo, IC = IC )

   attr(TwoMeanComparison, "class") <- "TMC"

   return( TwoMeanComparison )
}

print.TMC <- function(x, ...) {
   cat( "Null hypothesis rejected: ", x$FCalc > x$FTheo, "\n\n" )
   cat( "Confidence Intervals:\n" )

   print( x$IC )
}