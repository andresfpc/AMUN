\name{SingleCovComp}
\alias{SingleCovComp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test of hypothesis over a single covariance matrix
}
\description{
The following function provides a useful computation to contrast a single covariance matrix against to an already set covariance matrix as in Simar(2007).
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
\item{LStatistic}{a value of the likelihood ratio estimator}
\item{df}{total number of degrees of freedom for the likelihood ratio}
\item{chi2}{the value of the Chi Square statistic for the likelihod radio}
\item{pValue}{the p value of the Chi square statistic}
}
\references{
Hardle, W. & Simar, L. (2007), Applied Multivariate Statistical Analysis, Springer, New York.
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
Sigma_0 <- matrix(c( 4, 3, 2,
                     3, 6, 5,
                     2, 5, 10), nrow = 3, byrow = TRUE)

Sigma <- matrix(c( 3.42, 2.60, 1.89,
                     2.60, 8.00, 6.51,
                     1.89, 6.51, 9.62), nrow = 3, byrow = TRUE)

n <- 20

SingleCovComp(Sigma_0 = Sigma_0, Sigma = Sigma, n = n)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
