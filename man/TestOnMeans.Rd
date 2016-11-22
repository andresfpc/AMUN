\name{TestOnMeans}
\alias{TestOnMeans}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Univariate test on means
}
\description{
   An univariate test on means, where a random sample of N observations with known mean value \eqn{\mu} and unknown variance \eqn{\sigma^2} is drawn. The null hypothesis is that a the mean of the observations \eqn{\mu} is equal to a given \eqn{\mu_0}.
}
\usage{
TestOnMeans(x, mu0, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
   a vector containing the values of N observations
}
  \item{mu0}{
   a mean value for testing the null hypothesis
}
  \item{alpha}{
   a significance level (0.05 by default)
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
x <- c(53.5, 34.9)
mu0 <- c(43)

TestOnMeans(x = x, mu0 = mu0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
