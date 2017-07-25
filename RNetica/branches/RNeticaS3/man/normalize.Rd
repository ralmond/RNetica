\name{normalize}
\alias{normalize}
\alias{normalize.CPA}
\alias{normalize.CPF}
\alias{normalize.array}
\alias{normalize.data.frame}
\alias{normalize.matrix}
\alias{normalize.default}
\title{
  Normalizes a conditional probability table.
}
\description{
  A conditional probability table (CPT) represents a collection of
  probability distribution, one for each configuration of the parent
  variables. This function normalizes the
  CPT, insuring that the probabilities in each conditional distribution
  sum to 1.
}
\usage{
normalize(cpt)
\method{normalize}{CPF}(cpt)
\method{normalize}{data.frame}(cpt)
\method{normalize}{CPA}(cpt)
\method{normalize}{array}(cpt)
\method{normalize}{matrix}(cpt)
\method{normalize}{default}(cpt)
}
\arguments{
  \item{cpt}{
    A conditional probability table stored in either array
    (\code{\link{CPA}} format) or data frame (\code{\link{CPF}} format).
    A general data vector is treated like an unconditional probability
    vector. 
  }
}
\details{
  The \code{normalize} function is a generic function which attempts to
  normalize a conditional probability distribution.
  
  A conditional probability table in RNetica is represented in one of
  two ways.  In the conditional probability array (\code{CPA}) the table
  is represented as a \eqn{p+1} dimensional array.  The first \eqn{p}
  dimensions correspond to configurations of the parent variables and
  the last dimension the child value.  The \code{normalize.CPA} method
  adjusts the data value so that the sum across all of the child states
  is \eqn{1}.  Thus, \code{apply(result,1:p,sum)} should result in a
  matrix of \eqn{1}'s.  The method \code{normalize.array} first coerces
  its argument into a \code{CPA} and then applies the
  \code{normalize.CPA} method.

  The second way to represent a conditional probability table in RNetica
  is to use a data frame (\code{\link{CPF}}).  Here the factor variables
  correspond to a configuration of the parent states, and the numeric
  columns correspond to states of the child variable.  Each row
  corresponds to a particular configuration of parent variables and the
  numeric values should sum to one.  The \code{normalize.CPF} function
  makes sure this constraint holds.  The method
  \code{normalize.data.frame} first applies \code{as.CPF()} to make the
  data frame into a CPF.

  The method \code{normalize.matrix} ensures that the row sums are 1.
  It does not change the class.
  
  The default method only works for numeric objects.  It ensures that
  the total sum is \eqn{1}.

  \code{NA}'s are not allowed and will produce a result that is all
  \code{NA}s. 

}
\value{
  An object with similar properties to \code{cpt}, but adjusted so that
  probabilities sum to one.

  For \code{normalize.CPA} and \code{normalize.array} an normalized
  \code{CPA} array.

  For \code{normalize.CPF} and \code{normalize.data.fram} an normalized
  \code{CPF} data frame.

  For \code{normalize.matrix} an matrix whose row sums are \eqn{1}.

  For \code{normalize.default} a numeric vector whose values sum to
  \eqn{1}. 
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
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
n14 <- normalize(1:4)
stopifnot (abs(sum(n14)-1.0) <.0001)

normalize(matrix(1:6,2,3))
normalize(array(1:24,c(4,3,2)))
arr <- array(1:24,c(4,3,2),
             list(a=c("A1","A2","A3","A4"),
                  b=c("B1","B2","B3"),
                  c=c("C1","C2")))
arr <- as.CPA(arr)
narr <- normalize(arr)
stopifnot(
  is(narr,"CPA"), is(narr,"array"),
  all(abs(apply(narr,1:2,sum)-1) <.0001)
)

arf <- as.CPF(arr)
narf <- normalize(arf)
stopifnot(
  is(narf,"CPF"), is(narf,"data.frame"),
  all(abs(apply(narf[sapply(narf,is.numeric)],1,sum)-1) <.0001)
)

df2 <- data.frame(parentval=c("a","b"),
                  prob.true=c(1,1),prob.false=c(1,1))
ndf2 <- normalize(df2)
stopifnot(
  is(ndf2,"CPF"), is(ndf2,"data.frame"),
  all(abs(apply(ndf2[2:3],1,sum)-1) <.0001)
)

}
\keyword{ array }
\keyword{ manip }% __ONLY ONE__ keyword per line
