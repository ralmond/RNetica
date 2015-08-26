\name{CompileNetwork}
\alias{CompileNetwork}
\alias{UncompileNetwork}
\alias{is.NetworkCompiled}
\title{
  Builds the junction tree for a Netica Network
}
\description{
  Before Netica performs inference in a network, it needs to compile the
  network.  This process consists of building a junction tree and
  conditional probability tables for the nodes of that tree.  The
  function \code{CompileNetwork()} compiles the network and
  \code{UncompileNetwork()} undoes the compilation and frees the
  associated memory.
}
\usage{
CompileNetwork(net)
UncompileNetwork(net)
is.NetworkCompiled(net)
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} which will be compiled.
  }
}
\details{
  Usually Bayesian network projects operate in two phases.  In the
  construction phase, new nodes are added to the network, new connections
  made and conditional probability tables are set.

  In the inference phase, findings are added to nodes and other nodes
  are queried about their current conditional probability tables.

  The functions \code{CompileNetowrk()} and \code{UncompileNetwork()}
  move the networks between the two phases.  The documentation for
  \code{\link{EliminationOrder}()} and \code{\link{JunctionTreeReport}()}
  provide more details about the compilation process.  The function
  \code{\link{NetworkCompiledSize}()} provides information about the
  amount of storage used by the compiled network, but only after the
  network is compiled. 

  The function \code{is.NetworkCompiled()} tests to see if a network is
  compiled or not.
}
\value{
  The \code{NeticaBN} object \code{net} is returned invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{CompileNet_bn}, \nref{UncompileNet_bn}, \nref{SizeCompiledNet_bn}, 

}
\author{
  Russell Almond
}
\note{
  Calling \code{\link{NetworkCompiledSize}()} on an uncompiled network
  produces, an error, but also the sensible value of \code{-1}.  The
  function \code{is.NetworkCompiled()} calls the same internal function
  as \code{NetworkCompiledSize}, but clears the error.  This means it
  also clears any other errors that might be lurking in the system (see
  \code{\link{ReportErrors}()}).  
  
  I think calling \code{CompileNetwork()} twice on
  the same network is harmless.  Adding a node to a network will
  automatically uncompile it.
}
\section{Warning }{

  I'm currently observing a bug that occurs under Windows if not all of
  the nodes have their CPTs set.  Under Linux the function exhibits the
  expected behavior:  It generates a warning about the unset CPTs and
  enters a uniform distribution for each one.  Under Windows it reports
  the warning, but then generates an error "GetError_ns:  deleted or
  damage report_ns passed".  It is unclear if this a problem in Netica
  or RNetica.

  To work around, simply set all tables before compiling.
  
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{HasNodeTable}()},
  \code{\link{NodeFinding}()}, \code{\link{NodeBeliefs}()},
  \code{\link{EliminationOrder}()}, \code{\link{JunctionTreeReport}()},
  \code{\link{JointProbability}()},
  \code{\link{MostProbableConfig}()},
  \code{\link{FindingsProbability}()}}

\examples{

irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))
stopifnot (!is.NetworkCompiled(irt5))

CompileNetwork(irt5) ## Ready to enter findings
stopifnot (is.NetworkCompiled(irt5))

UncompileNetwork(irt5) ## Ready to add more nodes
stopifnot (!is.NetworkCompiled(irt5))

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
