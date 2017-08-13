\name{is.active}
\alias{is.active}
\title{
  Check to see if a Netica network or node object is still valid.
}
\description{

 \code{\linkS4class{NeticaSession}}, \code{\linkS4class{NeticaBN}},
  \code{\linkS4class{NeticaNode}}, \code{\linkS4class{CaseStream}}, and
  \code{\linkS4class{NeticaRNG}} objects all contain embedded pointers
  into Netica's memory.  The function 
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

  Internally,  \code{\linkS4class{NeticaSession}},
  \code{\linkS4class{NeticaBN}} and 
  \code{\linkS4class{NeticaNode}} objects all contain pointers to the
  corresponding Netica objects. The \code{\link{DeleteNetwork}()} and
  \code{\link{DeleteNodes}()} functions deletes the Netica objects (and
  clears the pointers in the R objects).  It is difficult to control
  when R objects are deleted, especially if they are protected in data
  structures that are saved in the workspace.  The function
  \code{is.active()} is meant to check if the corresponding object is
  still valid.  In most cases, RNetica will give an error (or at least a
  warning) if an inactive object is supplied as an argument.

  For \code{\linkS4class{CaseStream}} objects (and its sub-classes
  \code{\linkS4class{FileCaseStream}} and
  \code{\linkS4class{MemoryCaseStream}}) active and open have the same
  meaning.

  For \code{\linkS4class{NeticaRNG}} objects, they become inactive when
  they are freed.

  Note that the function \code{StopNetica()} should make all
  \code{NeticaBN} and \code{NeticaNode} objects inactive.  Thus, these
  objects cannot be saved from one R session to another, and should be
  recreated when needed.  In particular, any Netica object restored from
  a saved workspace should be inactive.
  
}
\value{

  The function \code{is.active()} returns \code{TRUE} if the argument
  still points to a network or node loaded in Netica's memory, and
  \code{FALSE} if that network or node has been deleted.  It returns
  \code{NA} if the argument is not a \code{NeticaSession},
  \code{NeticaBN}, \code{NeticaNode}, \code{CaseStream}, or
  \code{NeticaRNG} object.

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
\note{

  The actual test done is to test the pointer to see if it is null or
  not.  It should be the case that when an R object is disconnected from
  its Netica counterpart, the pointer is set to null.

}
\seealso{
  \code{\link{StopNetica}()}, \code{\linkS4class{NeticaBN}},
  \code{\link{DeleteNetwork}()}, \code{\linkS4class{NeticaNode}},
  \code{\link{DeleteNodes}()}, \code{\linkS4class{NeticaSession}},
  \code{\linkS4class{CaseStream}}, \code{\linkS4class{NeticaRNG}} 

}
\examples{
sess <- NeticaSession()
stopifnot(!is.active(sess))
startSession(sess)
stopifnot(is.active(sess))

anet <- CreateNetwork("ActiveNet", session=sess)
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

rng <- NewNeticaRNG(1, session=sess)
stopifnot(is.active(rng))
FreeNeticaRNG(rng)
stopifnot(!is.active(rng))

casefile <- tempfile("testcase",fileext=".cas")
filestream <- CaseFileStream(casefile, session=sess)
stopifnot(is.active(filestream))
CloseCaseStream(filestream)
stopifnot(!is.active(filestream))

stopSession(sess)
stopifnot(!is.active(sess))
}
\keyword{ interface }
\keyword{ utility }
