\name{DiscAnalysis}
\alias{DiscAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Discriminant Analysis
}
\description{
Sometimes it is required to assign an individual in one of many groups, to do that there consider the classification rule based on an index called lineal discriminate function, where different means but same covariance matrix are reasonable to assume. This program computes the discriminate scores, the classification table and finally the wrong classification percentage
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
function (X) 
{
    l <- ncol(X)
    X <- data.frame(X)
    sp <- factor(X[, l])
    levels(sp) <- paste0("g", 1:length(levels(sp)))
    X <- as.matrix(X[, -l])
    g1 <- X[sp == "g1", ]
    g2 <- X[sp == "g2", ]
    m1 <- colMeans(g1)
    m1
    m2 <- colMeans(g2)
    m2
    ng1 <- nrow(g1)
    ng2 <- nrow(g2)
    vg1 <- var(g1)
    vg2 <- var(g2)
    S <- (1/(ng1 + ng2 + -2)) * (((ng1 - 1) * vg1) + ((ng2 - 
        1) * vg2))
    a12 <- solve(S) \%*\% (m1 - m2)
    y12m <- ((m1 + m2) \%*\% solve(S) \%*\% (m1 - m2))/2
    W12 <- X \%*\% a12 - as.numeric(y12m)
    lx <- nrow(X)
    clas <- rep(NA, lx)
    clas[W12 > 0] <- 1
    clas[W12 < 0] <- 2
    classTable <- table(clas, sp)
    wc <- (classTable[1, 2] + classTable[2, 1])/lx
    DiscAnalysis <- list(Score = a12, ClassificationTable = classTable, 
        WrongClassificationPerc = wc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
