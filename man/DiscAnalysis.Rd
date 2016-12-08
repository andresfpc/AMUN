\name{DiscAnalysis}
\alias{DiscAnalysis}
\title{
Discriminant Analysis
}
\description{
Sometimes it is required to assign an individual in one of many groups, to do that there consider the classification rule based on an index called lineal discriminate function Morrison(2005). This program computes the discriminate scores, the classification table and finally the misclassification percentage.
}
\usage{
DiscAnalysis(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
a data matrix which last column must be the classification column
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Score}{the value of the discriminate score}
\item{ClassificationTable}{a table with the classification according with the discriminate score}
\item{MisClassificationPerc}{Misclassification percentage}
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
data(iris)
X <- iris[1:100, ]

DiscAnalysis(X)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
