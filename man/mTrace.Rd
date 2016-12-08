\name{mTrace}
\alias{mTrace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Trace of a matrix
}
\description{
Calculates the trace of a matrix object
}
\usage{
mTrace(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
a square matrix
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{mTrace}{the sum of the diagonal of the matrix}
}
\references{

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
A <- matrix(c(1, -1,
              3, 4), nrow = 2, byrow = TRUE)

mTrace(A) # 5
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ trace }% use one of  RShowDoc("KEYWORDS")
