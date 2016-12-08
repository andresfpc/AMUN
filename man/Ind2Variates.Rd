\name{Ind2Variates}
\alias{Ind2Variates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Independence of k Sets of Varieties
}
\description{
When the matrices of covaraince and correlation have been partitioned according with the population matrix it is recommended to use the Wilk's test but since is quite complicated there is an approximation to a Chi-square Morrison(2005).
}
\usage{
Ind2Variates(S, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
a positive define matrix which contains the whole variances and covariances
}
  \item{n}{
the total number of observations cover by the S matrix
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{S11}{
a positive define matrix related which contains the variances and covariances of the first group
}
  \item{S22}{
a positive define matrix related which contains the variances and covariances of the second group
}
\item{ChiSquareStatistic}{the value of the Chi Square Statistic}
\item{DegreeOfFreedom}{total normal of degrees of freedom of the Chi Square Statistic}
\item{pValue}{the p value of the Chi square statistic}
}
\references{
Morrison, D. F. (2005), Multivariate statistical methods, Series in Probabilty and Statistics, 4 edn, McGraw-Hill, New York.
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
S <- matrix(c(1, 0.45, -0.19, 0.43,
              0.45, 1, -0.02, 0.62,
              -0.19, -0.02, 1, -0.29,
              0.43, 0.62, -0.29, 1),
            nrow = 4, byrow = TRUE)
n <- 933

Ind2Variates(S = S, n = n)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
