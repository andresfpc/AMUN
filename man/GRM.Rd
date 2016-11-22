\name{GRM}
\alias{GRM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Groups of Repeated Measurements
}
\description{
Morrison (2005) in his book says that most of the time data collated in repeated measurements consists of groups of repetition of the same response at different times under varying experimental conditions. So the procedure to follow is to test the equal mean response by using the T square of Hotelling and latter use individual mean differences by simultaneous test and confidence intervals. The multinormal model of 2p variables is assumed.
}
\usage{
GRM(x1, x2, N, S11, S22, S12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
a data array with the measurement of the individuals with condition 1
}
  \item{x2}{
a data array with the measurement of the individuals with condition 2
}
  \item{N}{
total number of individuals
}
  \item{S11}{
a positive define matrix which contains the variances and covariances of the condition 1 group
}
  \item{S22}{
a positive define matrix which contains the variances and covariances of the condition 2 group
}
  \item{S12}{
a positive define matrix in the off-diagonal block of the covariance matrix partitioned
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
###########
# Example 1
###########
x1 <- matrix(c(100.48, 100.22, 105.33, 103.43,
               94.9, 97.28, 97.25, 97.83,
               94.54, 96.86, 95.39, 97.15,
               97.46, 100.24, 98.94, 101.77), nrow = 4, byrow = TRUE)
x2 <- matrix(c(100.48, 100.22, 105.33, 103.43,
               88.29, 93.13, 92.28, 93.43,
               88.19, 90.13, 92.4, 94.89,
               97.46, 100.24, 98.94, 101.77), nrow = 4, byrow = TRUE)

S11 <- matrix(c(49.79, 62.16, 26.06, 10.74,
               62.16, 126.98, 71.15, 34.65,
               26.06, 71.15, 62.47, 37.39,
               10.74, 34.65, 37.39, 36.3), nrow = 4, byrow = TRUE)

S22 <- matrix(c(31.84, 14.47, 9.63, 14.08,
               14.47, 20.7, 5.27, 1.55,
               9.63, 5.27, 13.6, 10.52,
               14.08, 1.55, 10.52, 21.9), nrow = 4, byrow = TRUE)

S12 <- matrix(c(17.92, 11.39, 12.78, 14.33,
               9.03, 21.20, 12.81, 4.32,
               -11.05, 15.46, 1.58, -11.08,
               -2.03, 17.12, -2.52, -10.25), nrow = 4, byrow = TRUE)

N <- 9

GRM(x1 = x1, x2 = x2, S11 = S11, S22 = S22, S12 = S12, N = N)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
