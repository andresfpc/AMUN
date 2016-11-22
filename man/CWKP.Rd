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
CWKP(x1, x2, h, l12, l21)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
a data array of all observation in group 1
}
  \item{x2}{
a data array of all observation in group 2
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x1, x2, h, l12, l21)
{
    x <- mu1 <- colMeans(x1)
    mu2 <- colMeans(x2)
    Sigma <- var(x1, x2)
    lamnda <- exp((t(mu1 - mu2) \%*\% solve(Sigma) \%*\% x) - ((1/2) *
        (t(mu1 - mu2) \%*\% solve(Sigma) \%*\% (mu1 - mu2))))
    lx <- lenght(x)
    clasn <- rep(NA, lx)
    clasn[lamnda > 0] <- "g1"
    clasn[lamnda < 0] <- "g2"
    clasn
    table(clasn, infa.n[, 4])
    delta <- t(mu1 - mu2) \%*\% solve(Sigma) \%*\% (mu1 - mu2)
    sqdelta <- sqrt(delta)
    mcp <- pnorm(q = (-1/2) * sqdelta)
    ba <- t(mu1 - mu2) \%*\% solve(Sigma) \%*\% x - ((1/2) * (t(mu1 -
        mu2) \%*\% solve(Sigma) \%*\% (mu1 - mu2)))
    loss <- ((1 - h) * l12)/(h * l21)
    clasb <- rep(NA, lx)
    clasb[ba > loss] <- "g1"
    clasb[ba < loss] <- "g2"
    clasb
    return(list(`Classification bylikelihood ratio` = clasn,
        `Misclassification probability` = mcp, `Classification by Bayes` = clasb))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
