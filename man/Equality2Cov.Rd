\name{Equality2Cov}
\alias{Equality2Cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Testing the Equality of several Covariance Matrices
}
\description{
Morrison (2005) gives an approximation to test the null hypothesis of equality of covariance of a multinormal population, by doing a slight change over the likelihood-ratio statistic, which is good if p and k do not exceed four or five and each n_i is twenty or more.
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (S, S1, S2, n1, n2)
{
    M <- (n1 + n2 - 1) * log(det(S)) - (n1 - 1) * log(det(S1)) -
        (n2 - 1) * log(det(S2))
    p <- nrow(S)
    k <- 2
    invC <- 1 - (2 * (p^2) + 3 * (p - 1))/(6 * (p + 1) * (k -
        1))
    chi <- M * invC
    df <- (k - 1) * (p) * (p + 1)/2
    pvalue <- 1 - pchisq(chi, df)
    Equality2Cov <- list(ChiSquareStatistic = chi, DegreeOfFreedom = df,
        `p-value` = pvalue)
    return(Equality2Cov)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
