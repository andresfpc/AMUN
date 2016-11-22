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
\packageAuthor{AMUN}
}
\note{
%%  ~~further notes~~
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
xbar <- c(12.57, 9.57, 11.49, 7.97)
ybar <- c(8.75, 5.33, 8.50, 4.75)

nx <- 37
ny <- 12

S <- matrix(c(11.2624, 9.4060, 7.1550, 3.3791,
              9.4060, 13.5265, 7.3784, 2.5014,
              7.1550, 7.3784, 11.5796, 2.6167,
              3.3791, 2.5014, 2.6167, 5.8133),
            nrow = 4, byrow = TRUE)

TwoMeanComparison(xbar, ybar, nx, ny, S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
