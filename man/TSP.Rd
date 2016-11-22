\name{TSP}
\alias{TSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for Two Special Patterns 
}
\description{
Morrison(2005) says that equal-covariance patterns was sufficient for the validity of the repeated-measurements equal mean tests so he present an test to prove the hypothesis of those multinormal covariance models.  
}
\usage{
TSP(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
a data matrix of repeated-measurements
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X) 
{
    S <- var(X)
    n <- nrow(X)
    r <- ncol(X)
    p <- nrow(S)
    s2 <- sum(diag(S))
    Smd <- S - diag(S)
    S2r <- sum(colSums(Smd))
    L <- (det(S)/((s2^p) * ((1 - r)^(p - 1)) * (1 + ((p - 1) * 
        r))))
    chi <- -(n - (((p * (p + 1)^2) * (2 * p - 3))/(6 * (p - 1) * 
        (p^2 * p - 4))))
    df <- (1/2) * p * (p + 1)
    pvalue <- pchisq(q = chi, df = df)
    return(list(`Chi square statistic` = chi, `P value` = pvalue))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
