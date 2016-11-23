\name{RMA}
\alias{RMA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Repeated Measurements Analysis
}
\description{
According to Morrison(2005) who describes in his book, many times obsevations are coleccted at different time on the same experimental unit and then it is reasonable that the treatment has an additive effect.
}
\usage{
RMA(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
a matrix with n measurements which are written in the columns, and in the rows the repeated measurements are written.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{p-value}{P value in single analysis of Repeated measurements}
\item{tc}{Computed t statistic}
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
