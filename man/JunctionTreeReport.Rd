\name{JunctionTreeReport}
\alias{JunctionTreeReport}
\alias{NetworkCompiledSize}
\title{

  Produces a report about the junction tree from a compiled Netica network.

}
\description{

  The process of compilation transforms the network into a junction tree
  -- a tree of cliques in the original graph -- that is more convenient
  computationally.  The function \code{JunctionTreeReport(\var{net})} produces
  a report on the junction tree.  The function
  \code{NetworkCompiledSize(\var{net})} reports on the size of the compiled
  network.  The network must be compiled
  (\code{\link{CompileNetwork}(\var{net})} must be called) before these
  functions are called.
}
\usage{
JunctionTreeReport(net)
NetworkCompiledSize(net)
}
\arguments{
  \item{net}{
    An active and compiled \code{\linkS4class{NeticaBN}} object.
  }
}
\value{

  For \code{JunctionTreeReport()} a character vector giving the report,
  one element per line.

  For \code{NetworkCompiledSize()} a scalar value giving the size of the
  network. 

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{ReportJunctionTree_bn}, \nref{SizeCompiledNet_bn}
}
\author{
  Russell Almond
}
\note{

  Currently, no attempt is made to parse the report, which has a fairly
  well structured format.  Future versions may produce a report object
  instead. 

}
\seealso{
  \code{\linkS4class{NeticaBN}}, \code{\link{CompileNetwork}()},
  \code{\link{EliminationOrder}()},
}
\examples{
sess <- NeticaSession()
startSession(sess)

EMSMMotif <- ReadNetworks(system.file("sampleNets","EMSMMotif.dne",
                                       package="RNetica"),
                           session=sess)

CompileNetwork(EMSMMotif)

JunctionTreeReport(EMSMMotif)

NetworkCompiledSize(EMSMMotif)

DeleteNetwork(EMSMMotif)
stopSession(sess)

}
\keyword{ interface }
\keyword{ misc }
