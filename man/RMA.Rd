\name{RMA}
\alias{RMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Repeated Measurements Analysis
}
\description{
Most of the time data collated in repeated measurements consists of groups of repetition of the same response at different times under varying experimental conditions. It tests the equal mean response by using the T square of Hotelling and uses simultaneous test and confidence intervals. The multinormal model of 2p variables is assumed.
}
\usage{
RMA(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
a matrix with n measurements which are written in the columns, and in the rows the repeated measurements are written.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{pValue}{P value in single analysis of Repeated measurements}
\item{tCalc}{Computed t statistic}
}
\references{
Morrison, D. F. (2005), Multivariate statistical methods, Series in Probability and Statistics, 4 edn, McGraw-Hill, New York.
}
\author{
\packageAuthor{AMUN}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
dat <- matrix(c( 2.50, 2.92,
                 3.32, 3.49,
                 3.00, 3.15,
                 3.21, 3.13,
                 2.65, 2.74,
                 2.33, 2.26), ncol = 2, byrow = TRUE)
                 
RMA(dat = dat)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
