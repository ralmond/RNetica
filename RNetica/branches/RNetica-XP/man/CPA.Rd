\name{CPA}
\alias{CPA}
\alias{as.CPA}
\alias{is.CPA}
\title{
  Representation of a conditional probability table as an array.
}
\description{
  A conditional probability table for a node can be represented as a
  array with the first \eqn{p} dimensions representing the parent
  variables and the last dimension representing the
  states of the node.  Given a set of values for the parent variables,
  the values in the last dimension contain the conditional probabilities 
  corresponding conditional probabilities.  A \code{CPA} is a special
  \code{\link[base]{array}} object which represents a conditional
  probability table.
}
\usage{
is.CPA(x)
as.CPA(x)
}
\arguments{
  \item{x}{
    Object to be tested or coerced into a \code{CPA}.  
  }
}
\details{
  One way to store a conditional probability table is as an array in
  which the first \eqn{p} dimensions represent the parent variables, and
  the \eqn{p+1} dimension represents the child variable.  Here is an
  example with two parents variables, \eqn{A} and \eqn{B}, and a single
  child variable, \eqn{C}:

  \code{, , C=c1}
  \tabular{rrrr}{
     \tab b1   \tab b2   \tab b3 \cr 
  a1 \tab 0.07 \tab 0.23 \tab 0.30 \cr 
  a2 \tab 0.12 \tab 0.25 \tab 0.31 \cr 
  a3 \tab 0.17 \tab 0.27 \tab 0.32 \cr 
  a4 \tab 0.20 \tab 0.29 \tab 0.33 \cr 
  }
  \code{, , C=c2}
  \tabular{rrrr}{
     \tab b1   \tab b2   \tab b3 \cr 
  a1 \tab 0.93 \tab 0.77 \tab 0.70 \cr 
  a2 \tab 0.88 \tab 0.75 \tab 0.69 \cr 
  a3 \tab 0.83 \tab 0.73 \tab 0.68 \cr 
  a4 \tab 0.80 \tab 0.71 \tab 0.67 \cr 
  }

  [Because R stores (and prints) arrays in column-major order, the
  last value (in this case tables) is the one that sums to 1.]
  
  The \code{CPA} class is a subclass of the
  \code{\link[base]{array}} class (formally, it is class
  \code{c("CPA","array")}).  The \code{CPA} class interprets the
  \code{dimnames} of the array in terms of the conditional probability
  table.  The first \eqn{p} values of \code{names(dimnames(x))} are the
  input names of the edges (see \code{\link{NodeInputNames}()} or the
  variable names (or the parent variable, see
  \code{\link{NodeParents}()}, if the input names were not specified),
  and the last value is the name of the child variable.  Each of the
  elements of \code{dimnames(x)} should give the state names (see
  \code{\link{NodeStates}()}) for the respective value.  In particular,
  the conversion function \code{\link{as.CPF}()} relies on the existence
  of this meta-data, and \code{as.CPA()} will raise a warning if an
  array without the appropriate dimnames is supplied.

  Although the intended interpretation is that of a conditional
  probability table, the normalization constraint is not enforced.  Thus
  a \code{CPA} object could be used to store likelihoods, probability
  potentials, contingency table counts, or other similarly shaped
  objects.  The function \code{normalize} scales the values of a
  \code{CPA} so that the normalization constraint is enforced.

  The method \code{\link{NodeProbs}()} returns a \code{CPA} object.

  The function \code{as.CPA()} is designed to convert between
  \code{\link{CPF}}s (that is, conditional probability tables stored as
  data frames) and \code{CPA}s.  It assumes that the factors variables
  in the data frame represent the parent variables, and the numeric
  values represent the states of the child variable.  It also assumes
  that the names of the numeric columns are of the form
  \code{\var{varname}.\var{state}}, and attempts to derive variable and
  state names from that.

  If the argument to \code{as.CPA(x)} is an array, then it assumes that
  the \code{dimnames(x)} and \code{names(dimnames(x))} are set to the
  states of the variables and the names of the variables respectively.
  A warning is issued if the names are missing.
}
\value{
  The function \code{is.CPA()} returns a logical value indicating
  whether or not the \code{is(x,"CPA")} is true.
  
  The function \code{as.CPA} returns an object of class
  \code{c("CPA","array")}, which is essentially an array with the
  dimnames set to reflect the variable names and states.
}
\author{
  Russell Almond
}
\note{
  The obvious way to print a \code{CPA} would be to always show the
  child variable as the rows in the individual tables, with the parents
  corresponding to rows and tables.  R, however, internally stores
  arrays in column-major order, and hence the rows in the printed tables
  always correspond to the second dimension.  A new print method for
  \code{CPA} would be nice.
}
\seealso{
  \code{\link{NodeProbs}()}, \code{\link{Extract.NeticaNode}},
  \code{\link{CPF}}, \code{\link{normalize}()}
}
\examples{
arf <- data.frame(A=rep(c("a1","a2"),each=3),
                  B=rep(c("b1","b2","b3"),2),
                  C.c1=1:6, C.c2=7:12, C.c3=13:18, C.c4=19:24)
arfa <- as.CPA(arf)
stopifnot(
  is.CPA(arfa),
  all(dim(arfa)==c(2,3,4))
)

arr1 <- array(1:24,c(4,3,2),
            dimnames=list(A=c("a1","a2","a3","a4"),B=c("b1","b2","b3"),
                          C=c("c1","c2")))
arr1a <- as.CPF(arr1)
stopifnot(
  is.CPA(as.CPA(arr1a))
)

\dontrun{
  as.CPF(node[])
}
}
\keyword{ array }
\keyword{ classes }% __ONLY ONE__ keyword per line
