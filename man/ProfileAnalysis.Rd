\name{ProfileAnalysis}
\alias{ProfileAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profile Analysis for Two Independent Group
}
\description{
The function does the profile analysis in the three steps test to check parallelism, in case the null hypothesis of parallelism is not rejected then a level and a horizontal test are applied (Morrison, 2005).
}
\usage{
ProfileAnalysis(dat1, dat2, C, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat1}{
A numeric matrix containing the values of the first group
}
  \item{dat2}{
A numeric matrix containing the values of the second group
}
  \item{C}{
A numeric \eqn{(p-1)\times x p} patterned matrix (by default a contrast matrix)
}
  \item{alpha}{
The significance level of the test
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{ParallelismHypothesis_pValue}{a p value to contrats the hypothesis of parallelism}
\item{TestSameLevel_pValue}{a p value to contrats the hypothesis of same level}
\item{Directionality_pValue}{a p value to contrats the hypothesis of directionality}
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
G1 <- matrix(c(125, 137, 121,
             144, 173, 147,
             105, 119, 125,
             151, 149, 128,
             137, 139, 109), nrow = 5, byrow = TRUE)

G2 <- matrix(c(93, 121, 107,
             116, 135, 106,
             109, 83, 100,
             89, 95, 83,
             116, 128, 100), nrow = 5, byrow = TRUE)

C <- matrix(c(1, -1, 0,
              0, 1, -1), nrow = 2, byrow = TRUE)

ProfileAnalysis(G1, G2, C)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
