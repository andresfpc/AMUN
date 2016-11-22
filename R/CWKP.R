## Classification with Known parameters 
CWKP <- function(x1, x2,h, l12, l21) {
	x <- cbind(x1, x2)
	mu1 <- mean(x1)
	mu2 <- mean(x2)
	Sigma <- var(x1, x2)
	lambda <- exp((t(mu1-mu2)%*%solve(Sigma)%*%x)-((1/2)*(t(mu1-mu2)%*%solve(Sigma)%*%(mu1-mu2))))
	lx <- length(x)
	clasn <- rep(NA,lx)
	clasn[lambda > 0] <- "g1"
	clasn[lambda < 0] <- "g2"
	#table(clasn,infa.n[,4])
	#Missclasification 
	delta <- t(mu1-mu2)%*%solve(Sigma)%*%(mu1-mu2)
	sqdelta <- sqrt(delta)
	mcp <- pnorm(q = (-1/2)*sqdelta)
	#Bayes 
	ba <- t(mu1-mu2)%*%solve(Sigma)%*%x-((1/2)*(t(mu1-mu2)%*%solve(Sigma)%*%(mu1-mu2)))
	loss <- ((1-h)*l12)/(h*l21)
	clasb <- rep(NA,lx)
	clasb[ba > loss] <- "g1"
	clasb[ba < loss] <- "g2"
	clasb
  
	CWKP <- list('LikelihoodRatioClass' = clasn,'MisclassificationProb' = mcp,
              'BayesClassification' = clasb)

  return(CWKP)
}

