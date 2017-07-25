\name{NodeUserField}
\alias{NodeUserField}
\alias{NodeUserField<-}
\alias{NodeUserObj}
\alias{NodeUserObj<-}
\alias{NodeAllUserFields}
\title{
  Gets user definable fields associated with a Netica node.
}
\description{
  Netica provides a mechanism for associating user defined values with a
  node as a series of key/value pairs.  The key must be a
  \code{\link{IDname}} and the value can be an arbitrary string.  The
  function \code{NodeUserField} accesses the string value associated
  with the key, and the function \code{NodeUserObj} accesses an R object
  associated with the key.
}
\usage{
NodeUserField(node, fieldname)
NodeUserField(node, fieldname) <- value
NodeUserObj(node, fieldname)
NodeUserObj(node, fieldname) <- value
NodeAllUserFields(node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{node}{
    A \code{\link{NeticaBN}} object indicating the node.
}
  \item{fieldname}{
    A character scalar conforming to the \code{\link{IDname}} rules.
}
  \item{value}{
    For \code{NodeUserField}, an arbitrary character vector
    containing the new value.  Only the first element is used. For
    \code{NodeUserObj}, an arbitrary object which is serialized with
    \code{\link{dputToString}} and then saved.
}
}
\details{
  Netica contains a mechanism for associating user data with nodes.
  In the Netica documentation, they note that only strings are really
  supported as only strings are portable across implementations.The
  function \code{NodeUserField} provides direct access for storing
  strings.  

  The function \code{NodeUserObj} wraps the call to
  \code{NodeUserField} with a call to \code{\link{dputToString}} or
  \code{\link{dgetFromString}} to allow the serialization of arbitrary
  objects.  


}
\value{
  The function \code{NodeUserField} returns a character scalar with
  the value stored in the field \code{fieldname}, or \code{NA} if no
  such field exists.

  The function \code{NodeUserObj} returns an arbitrary object created
  by calling \code{\link{dgetFromString}} on the value stored in the
  field \code{fieldname}, or \code{NULL} if no such field exists.  If
  the string cannot be interpreted as an R object, it generates an
  error. 

  The function \code{NodeAllUserFields} returns a character vector
  containing all user data stored with the node (this will be the
  serialized versions of the objects, not the objects themselves).  The
  names of the result are the names of the fields.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}
  \nref{GetNodeUserField_bn}, \nref{SetNodeUserField_bn},
  \nref{GetNodeNthUserField_bn}

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeDescription}()}
  \code{\link{NetworkUserField}}, \code{\link{dputToString}()}

}
\examples{
usedNet <- CreateNetwork("UsedNet")

userNode <- NewContinuousNode(usedNet, "UserNode")
NodeUserField(userNode,"Author") <- "Russell Almond"
NodeUserField(userNode,"Status") <- "In Progress"

stopifnot(NodeUserField(userNode,"Author")=="Russell Almond")
stopifnot(NodeUserField(userNode,"Status")=="In Progress")

fields <- NodeAllUserFields(userNode)
stopifnot(length(fields)==2)
stopifnot(all(!is.na(match(c("Russell Almond","In Progress"),fields))))
stopifnot(all(!is.na(match(c("Author","Status"),names(fields)))))

stopifnot(is.na(NodeUserField(userNode,"gender")))
stopifnot(is.null(NodeUserObj(userNode,"gender")))

x <- sample(1L:10L)
NodeUserObj(userNode,"x") <- x
x1 <- NodeUserObj(userNode,"x")
stopifnot(all(x==x1))


DeleteNetwork(usedNet)
}
\keyword{ interface }
\keyword{ attribute }
