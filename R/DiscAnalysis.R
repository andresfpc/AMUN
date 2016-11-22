### Discriminat analysis 
DiscAnalysis <- function(X){
  l<-ncol(X)
  X <- data.frame(X)
  sp <- factor(X[,l])
  levels(sp) <- paste0("g",1:length(levels(sp)))
  X <- as.matrix(X[,-l])
  g1 <- X[sp=="g1",]
  g2 <- X[sp=="g2",]
  m1 <- colMeans(g1); m1
  m2 <- colMeans(g2); m2
  ng1 <- nrow(g1)
  ng2 <- nrow(g2)
  vg1 <- var(g1)
  vg2 <- var(g2)
  S <- (1/(ng1+ng2+-2))*(((ng1-1)*vg1)+((ng2-1)*vg2))
  a12 <- solve(S)%*%(m1-m2)
  y12m <- ((m1+m2)%*%solve(S)%*%(m1-m2))/2
  W12 <- X %*% a12 - as.numeric(y12m)
  lx <- nrow(X)
  clas <- rep(NA,lx)
  clas[W12>0] <- 1
  clas[W12<0] <- 2
  classTable <- table(clas, sp)
  #porcentaje of wrong classification
  wc <- (classTable[1,2]+classTable[2,1])/lx
  
  DiscAnalysis <- list("Score" = a12,"ClassificationTable" = classTable, "WrongClassificationPerc" = wc)

  return(DiscAnalysis)
}