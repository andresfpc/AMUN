\name{TSP}
\alias{TSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for Two Special Patterns
}
\description{
Equal-covariance patterns was sufficient for the validity of the repeated-measurements equal mean tests so he present an test to prove the hypothesis of those multinormal covariance models Morrison(2005).
}
\usage{
TSP(S, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{S}{
a data matrix of variances and covariances
}
   \item{n}{
number of individuals
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Chi square statistic}{the value of the computed Chi square statistic}
\item{P value}{the P value fot the computed Chi square statistic}
}
\references{
Morrison, D. F. (2005), Multivariate statistical methods, Series in Probabilty and Statistics, 4 edn, McGraw-Hill, New York
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
S <- matrix(c(2.2605, 2.1763, 1.6342,
              2.1763, 2.6605, 1.8237,
              1.6342, 1.8237, 2.4710), nrow = 3, byrow = TRUE)

TSP(S, n = 20)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
