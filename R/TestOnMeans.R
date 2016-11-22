TestOnMeans <- function(x, mu0, alpha = 0.05) {
   xbar <- mean(x)
   xsd <- sd(x)
   
   N <- length(x)
   tc <- (xbar-mu0)*sqrt(N)/xsd
   tTheo <- pt(q = 1-alpha, df = length(x)-1)
   
   decision <- ifelse(tc <= tTheo, paste("Do not reject the null hypothesis"), paste("Reject the null hypothesis"))
   
   TestOnMeans <- list("tCalc" = tc, "tTheo" = tTheo, "Decision" = decision)
   
   return(TestOnMeans)
}
