\name{CWKP}
\alias{CWKP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classification with Known Parameters
}
\description{
Morrison (2005) presents two cases for classification when the parameters are known, the first case of classification is treated as a multinormal population with known parameters so the discriminate function is develop using likelihood-radio and also the concept of  the misclassification probabilities is used and in the second case the multinormal Bayes model of minimum expected loss is used.
}
\usage{
CWKP(mu1, mu2, Sigma, h, l12, l21)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu1}{
a data array of all observation in group 1
}
  \item{mu2}{
a data array of all observation in group 2
}
   \item{Sigma}{
a covariance matrix
}
  \item{h}{
the a priori probability of misclassification
}
  \item{l12}{
the loss function when classifiying an observation from the 2nd population in the 1st population
}
  \item{l21}{
the loss function when classifiying an observation from the 1st population in the 2nd population
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
mu1 <- c(12.57, 9.57, 11.49, 7.97)
mu2 <- c(8.75, 5.33, 8.50, 4.75)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
