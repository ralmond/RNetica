\name{normCPT}
\alias{normCPT}
\title{
  Normalizes a conditional probability table.
}
\description{
  A conditional probability table (CPT) is represented as an array whose
  last dimension represets a probability distribution (given the state of
  other variables in the other dimenions.  This function normalizes the
  CPT, insuring that the probability of that row is 1.
}
\usage{
normCPT(cpt)
}
\arguments{
  \item{cpt}{
    A multiway array to be normalized.
  }
}
\details{
  This function can be used to ensure that an array can be used with
  \code{\link{NodeProbs}()}. 
}
\value{
  An array with the same dimensions as \code{cpt} but for whom the sum
  across the last dimension is equal to 1.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPI/functions/#1.html}{#1()}}
}
\author{
  Russell Almond
}
\note{
  May be other functions for CPTs later.
}
\seealso{
  \code{\link{NodeProbs}()}
}
\examples{
normCPT(1:4)
normCPT(matrix(1:6,2,3))
normCPT(array(1:24,c(4,3,2)))

}
\keyword{ array }
\keyword{ manip }% __ONLY ONE__ keyword per line
