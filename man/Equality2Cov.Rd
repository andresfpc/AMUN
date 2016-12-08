\name{Equality2Cov}
\alias{Equality2Cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Testing the Equality of several Covariance Matrices
}
\description{
To test the null hypothesis of equality of covariance of a multinormal population, a slight change over the likelihood-ratio statistic is made, and it is recommended if p and k do not exceed four or five and each \eqn{n_i} is twenty or more (Morrison, 2005).
}
\usage{
Equality2Cov(S, S1, S2, n1, n2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
a positive define matrix which contains the whole variances and covariances
}
  \item{S1}{
a positive define matrix related which contains the variances and covariances of the first group
}
  \item{S2}{
a positive define matrix related which contains the variances and covariances of the second group
}
  \item{n1}{
the number of observation in the first group
}
  \item{n2}{
the number of observations in the second group
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{ChiSquareStatistic}{the value of the Chi Square Statistic}
\item{DegreeOfFreedom}{total normal of degrees of freedom of the Chi Square Statistic}
\item{pValue}{the p value of the Chi square statistic}
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
# Example 5.3 (Morrison, 2005)
S_M <- matrix(c(4.32, 1.88,
                1.88, 9.18), nrow = 2, byrow = TRUE)

S_F <- matrix(c(2.52, 1.90,
                1.90, 10.06), nrow = 2, byrow = TRUE)

S <- matrix(c(3.42, 1.89,
              1.89, 9.62), nrow = 2, byrow = TRUE)

Equality2Cov(S = S, S1 = S_M, S2 = S_F, n1 = 32, n2 = 32)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
