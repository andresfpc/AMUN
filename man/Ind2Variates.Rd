\name{Ind2Variates}
\alias{Ind2Variates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Independence of k Sets of Varieties
}
\description{
When the matrices of covaraince and correlation have been partitioned according with the population matrix it is recommended to use the Wilk's test but since is quite complicated there is an approximation to a Chi-square, all the above can be seen in Morrison(2005)
}
\usage{
Ind2Variates(S, S11, S22, n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{
a positive define matrix which contains the whole variances and covariances
}
  \item{S11}{
a positive define matrix related which contains the variances and covariances of the first group
}
  \item{S22}{

}a positive define matrix related which contains the variances and covariances of the second group
  \item{n}{
the total number of observations cover by the S matrix
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (S, S11, S22, n)
{
    V <- (det(S))/(det(S11) * (det(S22)))
    Sigma_3 <- (nrow(S11) + nrow(S22))^3 - ((nrow(S11)^3 + (nrow(S22)^3)))
    Sigma_2 <- (nrow(S11) + nrow(S22))^2 - ((nrow(S11)^2 + (nrow(S22)^2)))
    f <- (1/2) * Sigma_2
    invC <- 1 - (1/((12 * f) * (n - 1))) * (2 * Sigma_3 + 3 *
        Sigma_2)
    chi <- (-((n - 1) * invC) * log(V))
    pvalue <- pchisq(chi, f)
    Ind2Variates <- list(ChiSquareStatistic = chi, DegreesOfFreedom = f,
        `p-value` = pvalue)
    return(Ind2Variates)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
