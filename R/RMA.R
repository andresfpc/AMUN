############################################################
# (Morrison, 2005) 2.5 The analysis of Repeated Mesurements
# RepeatedMeasurementAnalysis
############################################################

RMA <- function(dat) {
	n <- nrow(dat)
	d <- dat[,1]-dat[,2]
	dbar <- mean(d)
	dsd <- sd(d)
	tc <- (dbar/dsd)*sqrt(n)
	tTheo <- pt(tc, df = n-1, lower.tail = F)
	
	RMA <- list("p-value" = tTheo, tc) #P value in single analysis of Repeated measurements

	return(RMA)
}
