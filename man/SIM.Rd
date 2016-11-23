\name{SIM}
\alias{SIM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simultaneous Inferences for Means
}
\description{
When the T square of Hotelling happened to be significative it is wanted to know which responses led to the rejection of the null hypothesis. To test that Morrison (2005) talk about two approaches using either the union and intersection of the T square of Hotelling or the Bonferroni critical value.
}
\usage{
SIM(xbar, mu, a, N, S, alpha = 0.05, intervals = "default", m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xbar}{
  a mean vector of the sample
}
  \item{mu}{
  a mean vector of the population
}
  \item{a}{
  a contrast vector
}
  \item{N}{
  the total number of observations
}
  \item{S}{
  a positive definite matrix with the variance and covariance
}
  \item{alpha}{
  a significance level (0.05 by default)
}
  \item{intervals}{
 the method for making simultaneous confidence intervals
}
  \item{m}{
  the number of linear functions of a'mu
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
xbar <- c(55.24, 34.97)
mu <- c(60, 30)
a <- c(1, 0)
N <- 100
S <- matrix(c(210.54, 126.99, 126.99, 119.68), nrow = 2, byrow = TRUE)

SIM(xbar = xbar, a = a, mu = mu, N = N, S = S)
SIM(xbar = xbar, a = a, mu = mu, N = N, S = S, interval = "bonferroni")
SIM(xbar = xbar, a = a, mu = mu, N = N, S = S, m = 4, interval = "bonferroni")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
