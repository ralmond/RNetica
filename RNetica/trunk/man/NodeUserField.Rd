\name{NodeUserField}
\alias{NodeUserField}
\alias{NodeUserField<-}
\alias{NodeAllUserFields}
\title{
  Gets user definable fields associated with a Netica node.
}
\description{
  Netica provides a mechanism for associating user defined values with a
  node as a series of key/value pairs.  The key must be a
  \code{\link{IDname}} and the value can be an aribtrary string.
}
\usage{
NodeUserField(node, fieldname)
NodeUserField(node, fieldname) <- value
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
    An arbitrary character string containing the new value.  Only the
    first element is used.
}
}
\details{
  Netica contains a mechanism for associating user data with nodes.
  In the Netica documentation, they note that only strings are really
  supported as only strings are portable across implementations.  This
  meachnism can be used to store arbitrary values, but the user is
  responsible for encoding/decoding them as strings.
}
\value{
  A character string with the value stored in the field
  \code{fieldname}, or \code{NA} if no such field exists.

  The function \code{NodeAllUserFields} returns a character vector
  containing all user data stored with the node.  The names of the
  result are the names of the fields.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPI/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPI/Manual/index.html}
  \nref{GetNodeUserField_bn}, \nref{SetNodeUserField_bn},
  \nref{GetNodeNthUserField_bn}

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeDescription}()}
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

DeleteNetwork(usedNet)
}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
