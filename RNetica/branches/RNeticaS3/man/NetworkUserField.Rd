\name{NetworkUserField}
\alias{NetworkUserField}
\alias{NetworkUserField<-}
\alias{NetworkUserObj}
\alias{NetworkUserObj<-}
\alias{NetworkAllUserFields}
\title{
  Gets user definable fields associated with a Netica network.
}
\description{
  Netica provides a mechanism for associating user defined values with a
  network as a series of key/value pairs.  The key must be a
  \code{\link{IDname}} and the value can be an arbitrary string
  (\code{NetworkUserField}) or arbitrary object (\code{NetworkUserObj}).
}
\usage{
NetworkUserField(net, fieldname)
NetworkUserField(net, fieldname) <- value
NetworkUserObj(net, fieldname)
NetworkUserObj(net, fieldname) <- value
NetworkAllUserFields(net)
}
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object indicating the network.
}
  \item{fieldname}{
    A character scalar conforming to the \code{\link{IDname}} rules.
}
  \item{value}{
    For \code{NetworkUserField}, an arbitrary character vector
    containing the new value.  Only the first element is used. For
    \code{NetworkUserObj}, an arbitrary object which is serialized with
    \code{\link{dputToString}} and then saved.
}
}
\details{
  Netica contains a mechanism for associating user data with networks.
  In the Netica documentation, they note that only strings are really
  supported as only strings are portable across implementations.  The
  function \code{NetworkUserField} provides direct access for storing
  strings.  

  The function \code{NetworkUserObj} wraps the call to
  \code{NetworkUserField} with a call to \code{\link{dputToString}} or
  \code{\link{dgetFromString}} to allow the serialization of arbitrary
  objects.  
}
\value{
  The function \code{NetworkUserField} returns a character scalar with
  the value stored in the field \code{fieldname}, or \code{NA} if no
  such field exists.

  The function \code{NetworkUserObj} returns an arbitrary object created
  by calling \code{\link{dgetFromString}} on the value stored in the
  field \code{fieldname}, or \code{NULL} if no such field exists.  If
  the string cannot be interpreted as an R object, it generates an
  error. 
  
  The function \code{NetworkAllUserFields} returns a character vector
  containing all user data stored with the network (this will be the
  serialized versions of objects, not the objects themselves).  The
  names of the result are the names of the fields.

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
  \code{\link{NodeUserField}}, \code{\link{dputToString}()}
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
stopifnot(is.null(NetworkUserObj(userNet,"gender")))

x <- sample(1L:10L)
NetworkUserObj(userNet,"x") <- x
x1 <- NetworkUserObj(userNet,"x")
stopifnot(all(x==x1))

DeleteNetwork(userNet)
}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
