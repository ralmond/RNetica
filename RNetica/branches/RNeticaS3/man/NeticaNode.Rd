\name{NeticaNode}
\alias{NeticaNode}
\alias{is.NeticaNode}
\alias{toString.NeticaNode}
\alias{print.NeticaNode}
\alias{Ops.NeticaNode}
\title{
  An object referencing a node in a Netica Bayesian network.
}
\description{
  This object is returned by various RNetica functions which create or
  find nodes in a \code{\link{NeticaBN}} network.  A \code{NeticaNode}
  object represents a node object inside of Netica's memory.  The
  function \code{is.active()} tests whether the node is still a valid
  reference. 
}
\usage{
is.NeticaNode(x)
\method{toString}{NeticaNode}(x, ...)
\method{print}{NeticaNode}(x, ...)
\method{Ops}{NeticaNode}(e1, e2)
e1 == e2
e1 != e2
}
\arguments{
  \item{x}{The object to print or test
  }
  \item{\dots}{Other arguments to \code{\link[base]{print}()} or
    \code{\link[base]{toString}()}
  }
  \item{e1}{A \code{NeticaNode} object to test.
  }
  \item{e2}{A \code{NeticaNode} object to test, or a list of such objects.
  }
}
\details{
  This is an object of class \code{NeticaNode}.  It consists of a name,
  and an invisible handle to a Netica node.  The function
  \code{\link{is.active}()} tests the state of that handle and returns
  \code{FALSE} if the node is no longer in active memory (usually
  because of a call to \code{DeleteNode()} or \code{DeleteNetwork()}.

  \code{NeticaNode}s come in two types:  discrete and continuous (see
  \code{\link{is.discrete}()}).  The two types give slightly different
  meanings to the \code{\link{NodeStates}()} and
  \code{\link{NodeLevels}()} attributes of the node.  The printed
  representation shows whether the node is discrete, continuous or
  inactive (deleted).  

  For active nodes, the equality test tests to see if both object
  point to the same object in Netica memory.  Note that the name of the
  node is embedded in the R object implementation and may get out of
  sync with Netica memory, so the printed representations may be unequal
  even if it points to the same node.  For inactive nodes, the
  objects are compared using the cached names.

}
\value{
  For \code{toString()} a string.  The function \code{print()} is
  usually called for its side effects.

  The function \code{is.NeticaNode()} returns a logical scalar depending
  on whether or not its argument is a \code{NeticaBN}.

  The function \code{Ops.NeticaNode()} returns a logical value depending on
  whether the objects are equal.  If the second argument is a list of
  \code{NeticaNode} objects, then a logical vector is returned, testing
  \code{e1} against every element of \code{e2}.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLurl/Manual/index.html}:
  \nref{AddNodeToNodeset_bn}, \nref{RemoveNodeFromNodeset_bn},
  \nref{IsNodeInNodeset_bn}
  \nref{GetNodeUserData_bn}, \nref{SetNodeUserData_bn} (these are used
  to maintain the back pointers to the R object).
}
\author{
  Russell Almond
}
\note{
  Internally, the \code{NeticaNode} objects are
  character strings with extra attributes.  So \code{as.character(node)}
  will return the name of the node.  Because of this, the default
  \code{\link[base]{c}()} function will strip off the essential
  attributes returning them to strings.  Use the \code{\link{cc}()}
  function instead to avoid this problem.

  Note that if a \code{NeticaNode} object is stored in an R object, and
  the Node is subsequently renamed (with a call to the set method of
  \code{NodeName}), the old object may persist with the wrong name.
  This may result in a situation where the printed names of the objects
  are different but \code{node1==node2} returns true.  This can be fixed
  with the code \code{NodeName(net) <- NodeName(net)}.

  \code{NeticaNode} objects are all rendered inactive when
  \code{StopNetica()} is called, therefore they do not persist across R
  sessions.  Generally speaking, the network should be saved, using
  \code{\link{WriteNetworks}()} and then reloaded in the new session
  using \code{\link{ReadNetworks}()}.  The node objects should then be
  recreated via a call to \code{\link{NetworkFindNode}()}.

  Note that RNetica is lazy about creating \code{NeticaNode} objects for
  nodes when a network is read from a file.  Probably users should avoid
  creating or saving \code{NetworkNode} objects unless they are going to
  use them frequently.
  
}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NetworkFindNode}()},
  \code{\link{is.active}()}, \code{\link{is.discrete}()}, 
  \code{\link{NewContinuousNode}()}, \code{\link{NewDiscreteNode}()},
  \code{\link{DeleteNodes}()}, \code{\link{NodeName}()},
  \code{\link{NodeStates}()}, \code{\link{NodeLevels}()},
  \code{\link{cc}()}

}
\examples{

nety <- CreateNetwork("yNode")

node1 <- NewContinuousNode(nety,"aNode")
stopifnot(is.NeticaNode(node1))
stopifnot(is.active(node1))
stopifnot(as.character(node1)=="aNode")

node2 <- NetworkFindNode(nety,"aNode")
stopifnot(as.character(node2)=="aNode")
stopifnot(node1==node2)

NodeName(node1) <- "Unused"
stopifnot(node1==node2)
## Warning:  The following expression is true!
as.character(node1) != as.character(node2)

noded <- DeleteNodes(node1)
stopifnot(!is.active(node1))
stopifnot(!is.active(node2))
stopifnot(as.character(noded)=="Unused")
stopifnot(noded == node1)
## Warning:  The following expression is true!
node1 != node2

DeleteNetwork(nety)
}
\keyword{ classes }
\keyword{ graphs }
\keyword{ interface }
