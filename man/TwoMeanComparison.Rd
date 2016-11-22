\name{TwoMeanComparison}
\alias{TwoMeanComparison}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Comparison of two sample means  
}
\description{
Morrison(2005) says that many times two observations coming from a multidimensional variate and have been obtained under different conditions, a multinormal distribution is assumed with the same although unknown covariance matrix and it is desire to test the hypothesis of same mean vectors. The procedure uses the T square of Hotelling and presents a confidence interval.
}
\usage{
TwoMeanComparison(xbar, ybar, nx, ny, S, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xbar}{
a vector of means of the variable x
}
  \item{ybar}{
a vector of means of the variable y
}
  \item{nx}{
sample size of x
}
  \item{ny}{
sample size of y
}
  \item{S}{
a positive define matrix of the variance and covariance
}
  \item{alpha}{
the significance level (0.05 by default)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Morrison, D. F. (2005), Multivariate statistical methods, Series in Probability and Statistics, 4 edn, McGraw-Hill, New York.
}
\author{
Jesus Miguel Gonzalez Flores; Andres Felipe Palacios Clavijo; Campo Elias Pardo
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(infarto)

ns <- as.vector( by( infarto[,-8], infarto[,8], nrow ) )
means <- by( infarto[,-8], infarto[,8], colMeans )
W <- by(infarto[,-8],infarto[,8], var )
W1 <- ( ns[1]-1 ) * W$vive
W2 <- ( ns[2]-1 ) * W$muere
S <- ( W1 + W2 )/( sum( ns )-2 )

mc <- TwoMeanComparison( means$vive, means$muere, ns[1], ns[2] , S, 0.05 )

mc
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
