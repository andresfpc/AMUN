\name{SingleCovComp}
\alias{SingleCovComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test of hypothesis over a single covariance matrix 
}
\description{
The following function provides a useful computation to contrast a single covariance matrix against to an already set covariance matrix as in Simar(2007)
}
\usage{
SingleCovComp(Sigma_0, Sigma, n, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sigma_0}{
a positive define matrix which contains the variances and covariances under the null hypothesis
}
  \item{Sigma}{
A positive define matrix which contains the variances and covariances that is being use to contrast
}
  \item{n}{
number of observatios
}
  \item{alpha}{
level of significance
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
Hardle, W. & Simar, L. (2007), Applied Multivariate Statistical Analysis,
Springer, New York.
}
\author{
Jesus Miguel Gonzalez Flores;
Andres Felipe Palacios Clavijo;
Campo Elias Pardo
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
function (Sigma_0, Sigma, n, alpha = 0.05) 
{
    p <- nrow(Sigma)
    df <- (1/2) * p * (p + 1)
    Sigmahat <- ((n - 1)/n) * Sigma
    prodSigma <- solve(Sigma_0) \%*\% Sigmahat
    estTest <- n * mTrace(prodSigma) - n * log(det(prodSigma)) - 
        n * p
    chi2 <- qchisq(p = 1 - alpha, df = df, lower.tail = F)
    pValue <- pchisq(q = estTest, df = df, lower.tail = F)
    SingleCovComp <- list(`-2logLambda` = estTest, df = df, chi2 = chi2, 
        pValue = pValue)
    return(SingleCovComp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
