\name{NetworkUserField}
\alias{NetworkUserField}
\alias{NetworkUserField<-}
\alias{NetworkAllUserFields}
\title{
  Gets user definable fields associated with a Netica network.
}
\description{
  Netica provides a mechanism for associating user defined values with a
  network as a series of key/value pairs.  The key must be a
  \code{\link{IDname}} and the value can be an aribtrary string.
}
\usage{
NetworkUserField(net, fieldname)
NetworkUserField(net, fieldname) <- value
NetworkAllUserFields(net)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object indicating the network.
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
  Netica contains a mechanism for associating user data with networks.
  In the Netica documentation, they note that only strings are really
  supported as only strings are portable across implementations.  This
  meachnism can be used to store arbitrary values, but the user is
  responsible for encoding/decoding them as strings.
}
\value{
  A character string with the value stored in the field
  \code{fieldname}, or \code{NA} if no such field exists.

  The function \code{NetworkAllUserFields} returns a character vector
  containing all user data stored with the network.  The names of the
  result are the names of the fields.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}
  \nref{GetNetUserField_bn}, \nref{SetNetUserField_bn},
  \nref{GetNetNthUserField_bn}

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NetworkComment}()}
}
\examples{
userNet <- CreateNetwork("UserNet")
NetworkUserField(userNet,"Author") <- "Russell Almond"
NetworkUserField(userNet,"Status") <- "In Progress"

stopifnot(NetworkUserField(userNet,"Author")=="Russell Almond")
stopifnot(NetworkUserField(userNet,"Status")=="In Progress")

fields <- NetworkAllUserFields(userNet)
stopifnot(length(fields)==2)
stopifnot(all(!is.na(match(c("Russell Almond","In Progress"),fields))))
stopifnot(all(!is.na(match(c("Author","Status"),names(fields)))))

stopifnot(is.na(NetworkUserField(userNet,"gender")))

DeleteNetwork(userNet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
