\name{CPF}
\alias{CPF}
\alias{as.CPF}
\alias{is.CPF}
\title{
  Representation of a conditional probability table as a data frame.
}
\description{
  A conditional probability table for a node can be represented as a
  data frame with a number of factor variables representing the parent
  variables and the remaining numeric values representing the
  conditional probabilities of the states of the nodes given the parent
  configuration.  Each row represents one configuration and the
  corresponding conditional probabilities.  A \code{CPF} is a special
  \code{\link[base]{data.frame}} object which represents a conditional
  probability table.
}
\usage{
is.CPF(x)
as.CPF(x)
}
\arguments{
  \item{x}{
    Object to be tested or coerced into a \code{CPF}.  
  }
}
\details{
  One way to store a conditional probability table is a table in which
  the first several columns indicate the states of the parent variables,
  and the last several columns indicate probabilities for several child
  variables. Consider the following example:
  \tabular{rllrrrr}{
     \tab  A \tab  B \tab C.c1 \tab C.c2 \tab C.c3 \tab C.c4 \cr 
[1,] \tab a1 \tab b1 \tab 0.03 \tab 0.17 \tab 0.33 \tab 0.47 \cr 
[2,] \tab a2 \tab b1 \tab 0.05 \tab 0.18 \tab 0.32 \tab 0.45 \cr 
[3,] \tab a1 \tab b2 \tab 0.06 \tab 0.19 \tab 0.31 \tab 0.44 \cr 
[4,] \tab a2 \tab b2 \tab 0.08 \tab 0.19 \tab 0.31 \tab 0.42 \cr 
[5,] \tab a1 \tab b3 \tab 0.09 \tab 0.20 \tab 0.30 \tab 0.41 \cr 
[6,] \tab a2 \tab b3 \tab 0.10 \tab 0.20 \tab 0.30 \tab 0.40 \cr 
  }
  In this case the first two columns correspond to parent variables
  \eqn{A} and \eqn{B}.  The variable \eqn{A} has two possible states and
  the variable \eqn{B} has three.  The child variable is \eqn{C} and it
  has for possible states.  The numbers in each row give the conditional
  probabilities for those states give the state of the child variables.

  The class \code{CPF} is a subclass of \code{\link[base]{data.frame}}
  (formally, it is class \code{c("CPF","data.frame")}).  Although the
  intended interpretation is that of a conditional probability table, the
  normalization constraint is not enforced.  Thus a \code{CPF} object
  could be used to store likelihoods, probability potentials,
  contingency table counts, or other similarly shaped objects.  The
  function \code{normalize} scales the numeric values of \code{CPF} so
  that each row is normalized.

  The method \code{"[.NeticaNode"} %]
  returns a \code{CPF} (if the node is not deterministic).

  The function \code{as.CPF()} is designed to convert between
  \code{\link{CPA}}s (that is, conditional probability tables stored as
  arrays) and \code{CPF}s. In particular, \code{as.CPF} is designed to
  work with the output of \code{NodeProbs()} or a similarly formatted
  array.  It assumes that \code{names(dimnames(x))} are the names of the
  variables, and \code{dimnames(x)} is a list of character vectors
  giving the names of the states of the variables. (See
  \code{\link{CPA}} for details.)  This general method should work with
  any numeric array for which both \code{dimnames(x)} and
  \code{names(dimnames(x))} are specified.

  The argument \code{x} of \code{as.CPF()} could also be a data frame,
  in which case it is permuted so that the factor variable are first and
  the class tag \code{"CDF"} is added to its class.

}
\value{
  The function \code{is.CPF()} returns a logical value indicating
  whether or not the \code{is(x,"CDF")} is true.
  
  The function \code{as.CPF} returns an object of class
  \code{c("CPF","data.frame")}, which is essentially a data frame with
  the first couple of columns representing the parent variables, and the
  remaining columns representing the states of the child 
  variable. 
}
\author{
  Russell Almond
}
\note{
  The parent variable list is created with a call
  \code{\link[base]{expand.grid}(dimnames(x)[1:(p-1)])}.  This produces
  conditional probability tables where the first parent variable varies
  fastest.  The Netica GUI displays tables in which the last parent
  variable varies fastest.
}
\seealso{
  \code{\link{NodeProbs}()}, \code{\link{Extract.NeticaNode}},
  \code{\link{CPA}}, \code{\link{normalize}()}
}
\examples{
arf <- data.frame(A=rep(c("a1","a2"),each=3),
                  B=rep(c("b1","b2","b3"),2),
                  C.c1=1:6, C.c2=7:12, C.c3=13:18, C.c4=19:24)
arf <- as.CPF(arf)
stopifnot(is.CPF(arf))

arr <- array(1:24,c(2,3,4),
            dimnames=list(A=c("a1","a2"),B=c("b1","b2","b3"),
                          C=c("c1","c2","c3","c4")))
arrf <- as.CPF(arr)
stopifnot(
  is.CPF(arrf),
  all(levels(arrf$A)==c("a1","a2")),
  all(levels(arrf$B)==c("b1","b2","b3")),
  nrow(arrf)==6, ncol(arrf)==6
)

##Warning, this is not the same as arf, rows are permuted.
as.CPF(as.CPA(arf))

\dontrun{
  as.CPF(NodeProbs(node))
}
}
\keyword{ array }
\keyword{ classes }% __ONLY ONE__ keyword per line
