\name{is.active}
\alias{is.active}
\title{
  Check to see if a Netica network or node object is still valid.
}
\description{
  Both \code{\link{NeticaBN}} and \code{\link{NeticaNode}} objects
  contain embedded pointers into Netica's memory.  The function
  \code{is.active()} checks to see that the corresponding Netica object
  still exists. 
}
\usage{
is.active(x)
}
\arguments{
  \item{x}{A \code{NeticaBN} or \code{NeticaNode} object to test, or a
    list of such objects.
  }
}
\details{
  Internally, both \code{NeticaBN} and \code{NeticaNode} objects contain
  pointers to the corresponding Netica objects. The
  \code{\link{DeleteNetwork}()} and \code{\link{DeleteNodes}()}
  functions deletes the Netica objects (and clears the pointers in the R
  objects).  It is difficult to control when R objects are deleted,
  especially if they are protected in data structures that are saved in
  the workspace.  The function \code{is.active()} is meant to check if
  the corresponding object is still valid.  In most cases, RNetica will
  give an error (or at least a warning) if an inactive object is
  supplied as an argument.

  Note that the function \code{StopNetica()} should make all
  \code{NeticaBN} and \code{NeticaNode} objects inactive.  Thus, these
  objects cannot be saved from one R session to another, and should be
  recreated when needed.
  
}
\value{
  The function \code{is.active()} returns \code{TRUE} if the argument
  still points to a network or node loaded in Netica's memory, and
  \code{FALSE} if that network or node has been deleted.  It returns
  \code{NA} if the argument is not a \code{NeticaBN} or
  \code{NeticaNode}.

  If \code{x} is a list, then a logical vector of the same length of
  \code{x} is returned with \code{is.active()} recursively applied to
  each one.
}
\references{
  \url{http://norsys.com/onLineAPIManual/index.html},
  \url{http://lib.stat.cmu.edu/R/CRAN/doc/manuals/R-exts.html}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{StopNetica}()}, \code{\link{NeticaBN}},
  \code{\link{DeleteNetwork}()}, \code{\link{NeticaNode}},
  \code{\link{DeleteNodes}()}
}
\examples{
anet <- CreateNetwork("ActiveNet")
stopifnot(is.active(anet))

anodes <- NewContinuousNode(anet,paste("ActiveNode",1:2,sep=""))
stopifnot(all(is.active(anodes)))

inode <- DeleteNodes(anodes[[1]])
stopifnot(!is.active(anodes[[1]]))
stopifnot(!is.active(inode))
stopifnot(is.active(anodes[[2]]))

DeleteNetwork(anet)
stopifnot(!is.active(anet))
## Node gets deleted along with network
stopifnot(!any(is.active(anodes)))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ utility }% __ONLY ONE__ keyword per line
