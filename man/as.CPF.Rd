\name{as.CPF}
\alias{as.CPF}
\title{
  Translates a conditional probability array to a conditional
  probability data frame.
}
\description{
  This translates a conditional proability table stored as an array,
  with the last index corresponding to the dependent variable, to a data
  frame, where the parent variables are expressed as factors.  In
  particular, this is suitable for use with the output of the function
  \code{\link{NodeProbs}()}.  
}
\usage{
as.CPF(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A numeric array where the diminesion correspond to variables, and
    the last variable is the dependent (child) variable.  For the
    function to work properly \code{dimnames(x)} should be complete with
    both names and state names for the variables.

    The argument \code{x} could also be a data frame, in which case it
    is permuted so that the factor variable are first and the class tag
    \code{"CDF"} is added.
  }
}
\details{
  This function is designed to work with the output of
  \code{NodeProbs()} or a similarly formatted array.  It assumes that
  \code{names(dimnames(x))} are the names of the variables, and
  \code{dimnames(x)} is a list of character vectors giving the names of
  the states of the variables.

  Let \eqn{p} be the number of dimensions in \code{x}.  The first
  \eqn{p-1} columns of the result represent the parent variables.
  The rows in the matrix represent all possible configurations of the
  parents. The remaining columns of the result follow the states of the
  last dimension of \code{x} and the values are the conditional
  probabilities.  
}
\value{
  An object of class \code{"CDF"}, which is essentailly a data frame
  with the first couple of columns representing the parent variables,
  and the remaining columns representing the states of the child
  variable. 
}
\author{
  Russell Almond
}
\note{
  The parent variable list is created with a call
  \code{\link[base]{expand.grid}(dimnames(x)[1:(p-1)])}.
}
\seealso{
  \code{\link{NodeProbs}()}
}
\examples{
arr <- array(1:24,c(2,3,4),
            dimnames=list(A=c("a1","a2"),B=c("b1","b2","b3"),
                          C=c("c1","c2","c3","c4")))
as.CPF(arr)

\dontrun{
  as.CPF(NodeProbs(node))
}
}
\keyword{ array }
\keyword{ classes }% __ONLY ONE__ keyword per line
