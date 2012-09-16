\name{CopyNets}
\alias{CopyNets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Makes copies of Netica networks.
}
\description{
  Makes a copy of the networks in the list \code{nets} giving them the
  names in \code{newnamelist}.  The \code{options} argument controls how
  much information is copied.
}
\usage{
CopyNets(nets, newnamelist, options = character(0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nets}{
    A list of \code{\link{NeticaBN}} objects.
}
  \item{newnamelist}{
    A character vector of the same length as \code{nets} which gives the
    names for the newly created copies.
}
  \item{options}{
    A character vector containing information about what to copy.  The
    elements should be one of the values \code{"no_nodes"},
    \code{"no_links"}, \code{"no_tables"}, \code{"no_visual"}.
}
}
\details{
  Copies each of the networks in the \code{nets} lists, giving it a new
  name from the \code{newnamelist}. It returns a list of the new
  networks.  If the specified net does not exist, then a warning is
  issued and a \code{NULL} is returned instead of the corresponding
  \code{NeticaBN} object.
  
  The \code{options} argument is passed to the \code{options} argument
  of the Netica API function \code{CopyNet_bn()}.  Meanings for the
  various arguments can be found in the documentation for that
  function.  Note that Netica expects a list of comma separated values.
  RNetica will collapse the \code{options} argument into a comma
  separated list, so the argument can be given either as a character
  vector of length 1 containing a comma separated list, or the elements
  of that list in separate elements of a character vector.
}
\value{
  A list of \code{\link{NeticaBN}} objects corresponding to the new
  networks, or if the length of \code{nets} is one, a single
  \code{NeticaBN} object is returned instead.  A \code{NULL} is returned
  instead of the \code{NeticaBN} object if the corresponding element of
  \code{nets} does not exit.
}
\references{
  \link{http://norsys.com/onLineAPI/Manual/index.html}
}
\author{
  Russell Almond
}

\seealso{
  \code{\link{DeleteNet}()}
}
\examples{
StartNetica()

net1 <- CreateNetwork("Untitled1")
nets <- CreateNetwork(paste("Untitled",2:3,sep=""))

copy1 <-CopyNets("Untitled1","Copy1")
stopifnot(is(copy1,"NeticaBN"))
stopifnot(as.character(copy1) == "Copy1")

netc <- CopyNets(netnn,paste("Copy",2:3,sep=""))
stopifnot(all(sapply(netc,is,"NeticaBN")))
stopifnot(as.character(netc) == c("Copy2","Copy3"))

StopNetica()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ utility }
\keyword{ models }% __ONLY ONE__ keyword per line
