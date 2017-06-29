\name{cc}
\alias{cc}
\alias{c.NeticaNode}
\alias{c.NeticaBN}
\title{Concatenates lists without stripping attributes}
\description{

  The base R function \code{\link[base]{c}()} strips the attributes off
  of objects (particularly \code{\link{NeticaNode}} and
  \code{\link{NeticaBN}} objects).  The function \code{cc} is a
  replacement which does not do that stripping.

}
\usage{
cc(...)
\method{c}{NeticaNode}(...)
\method{c}{NeticaBN}(...)
}
\arguments{
  \item{\dots}{A list of objects.  Generally it should be either
    \code{NeticaNode} or \code{NeticaBN} objects or lists of such
    objects.}
}
\details{

  The base R \code{c()} function strips attributes from objects.  For
  \code{\link{NeticaNode}} and \code{\link{NeticaBN}} objects, this
  removes the attributes that link the name to the Netica object and
  leaves just a string.  This \dQuote{feature} of S has been around
  since the days of the Blue Book and there is probably code that relies
  on this unexpected behavior.

  The \code{cc()} function works around this by copying the arguments
  one at a time into a new list (slower but safer).  Arguments which
  satisfy \code{is(arg,"list")} are treated as lists and add
  \code{length(arg)} elements to the lsit.  All other arguments are
  treated as essentially lists of length 1, and the value is inserted in
  the appropriate place in the list.

  The methods for \code{NeticaNode} and \code{NeticaBN} fix the
  \code{c()} function (which is generic) if the first argument is a
  singleton. Thus \code{c(newNode,nodeList)} and
  \code{cc(newNode,nodeList)} are identical.  Note that these only fix
  half of the problem; \code{c(nodeList,newNode)} still calls the
  default method (or the method for lists) which strips the attributes
  of \code{newNode}.  Instead use \code{cc(nodeList,newNode)} or
  \code{c(nodeList,list(newNode))}

}
\value{
  A list containing all of the values in the arguments.  If there is a
  single, non-list argument, it will return a list with one element.
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}},\code{\link{NeticaBN}}
}
\examples{

anet <- CreateNetwork("anet")

nodeList <- NewDiscreteNode(anet,paste("oldNode",1:3,sep=""))
newNode <- NewDiscreteNode(anet,"newNode")

l1 <- c(newNode,nodeList)               #A list of nodes
stopifnot(is.list(l1),length(l1)==4L,sapply(l1,is.NeticaNode))

l2 <- c(nodeList,newNode)               #Doesn't work!!!
stopifnot(!all(sapply(l2,is.NeticaNode)))

l2a <- cc(nodeList,newNode)               #Does work!!!
stopifnot(all(sapply(l2a,is.NeticaNode)))

l2b <- c(nodeList,list(newNode))         #As does this
stopifnot(all(sapply(l2b,is.NeticaNode)))

l3 <- c(newNode)                        #List with one element
stopifnot(is.list(l3),length(l3)==1L,sapply(l3,is.NeticaNode))

l4 <- c(newNode,nodeList[[1]],nodeList[[3]])                        
stopifnot(is.list(l4),length(l4)==3L,sapply(l4,is.NeticaNode))

l5 <- c(newNode,nodeList[2:3],nodeList[[1]])                        
stopifnot(is.list(l5),length(l5)==4L,sapply(l5,is.NeticaNode))


}
\keyword{ manip }
\keyword{ methods }
