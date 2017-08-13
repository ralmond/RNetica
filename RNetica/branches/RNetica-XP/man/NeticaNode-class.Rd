\name{NeticaNode-class}
\Rdversion{1.1}
\docType{class}
\alias{NeticaNode-class}
\alias{Compare,NeticaNode,ANY-method}
\alias{print,NeticaNode-method}
\alias{toString,NeticaNode-method}

\title{Class \code{"NeticaNode"}}
\description{
  This object is returned by various RNetica functions which create or
  find nodes in a \code{\linkS4class{NeticaBN}} network.  A \code{NeticaNode}
  object represents a node object inside of Netica's memory.  The
  function \code{is.active()} tests whether the node is still a valid
  reference. 
}
\section{Extends}{

All reference classes extend and inherit methods from
\code{"\linkS4class{envRefClass}"}. Note that because this is a
reference class unlike traditional S3 and S4 classes it can be
destructively modified.  Also fields (slots) are accessed using the
\sQuote{$} operator.

}
\section{Methods}{
  \describe{
    \item{[<-}{\code{signature(x = "NeticaNode")}: Sets conditional
      probabliity table for node, see \link{Extract.NeticaNode}. }
    \item{[}{\code{signature(x = "NeticaNode")}: Gets conditional
      probabliity table for node, see \link{Extract.NeticaNode}. }
    \item{[[}{\code{signature(x = "NeticaNode")}: Gets conditional
      probabliity table for node, see \link{Extract.NeticaNode}. }
    \item{Compare}{\code{signature(e1 = "NeticaNode", e2 = "ANY")}:
      Tests two nodes for equality }
    \item{print}{\code{signature(x = "NeticaNode")}: Makes printed
      representation.  }
    \item{toString}{\code{signature(x = "NeticaNode")}: Makes character
      representation.  }
	 }
}
\details{

  This is an object of class \code{NeticaNode}.  It consists of a name,
  and an pointer to a Netica node in the workspace.  The function
  \code{\link{is.active}()} tests the state of that handle and returns
  \code{FALSE} if the node is no longer in active memory (usually
  because of a call to \code{DeleteNode()} or \code{DeleteNetwork()}.

  \code{NeticaNode}s come in two types:  discrete and continuous (see
  \code{\link{is.discrete}()}).  The two types give slightly different
  meanings to the \code{\link{NodeStates}()} and
  \code{\link{NodeLevels}()} attributes of the node.  The printed
  representation shows whether the node is discrete, continuous or
  inactive (deleted).  

  \code{NeticaNode} objects are created at two different times.  First,
  when the user creates a node in a network using the
  \code{\link{NewContinuousNode}()} or  \code{\link{NewDiscreteNode}()}
  functions. The second is when a user first reads the network in from a
  file using \code{\link{ReadNetworks}} and then subsequently searches
  for the node using \code{\link{NetworkFindNode}}.  Note that this
  latter means that there may be nodes in the Netica network for which
  no R object has yet been created.  When \code{NeticaNode} objects are
  created, they are cached in the \code{\linkS4class{NeticaBN}} object.
  Cached objects can be referenced by the \code{nodes} field of the
  \code{NeticaBN} object (which is an R
  \code{\link[base]{environment}}).  Thus, the expressions
  \code{\var{net}$nodes$\var{nodename}} and
  \code{\var{net}$nodes[[\var{nodename}]]} both reference a node with
  the Netica name \code{\var{nodename}} in the network
  \code{\var{net}}.   Note that both of these expressions will yeild
  \code{NULL} if no R object has yet been created for the node.  The
  function \code{\link{NetworkAllNodes}(\var{net})} will as a side
  effect create node objects for all of the nodes in \code{\var{net}}.
  

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

  \code{NeticaNode} objects are all rendered inactive when
  \code{\link{StopNetica}()} is called, therefore they do not persist
  across R sessions.  Generally speaking, the network should be saved,
  using \code{\link{WriteNetworks}()} and then reloaded in the new
  session using \code{\link{ReadNetworks}()}.  The node objects should
  then be recreated via a call to \code{\link{NetworkFindNode}()} or
  \code{\link{NetworkAllNodes}()}.
  

}

\seealso{
  Its container class can be found in \code{\linkS4class{NeticaBN}}.

  The help file \code{\link{Extract.NeticaNode}} explains the principle
  methods of referencing the conditional probability table.

  \code{\link{NetworkFindNode}()},
  \code{\link{is.active}()}, \code{\link{is.discrete}()}, 
  \code{\link{NewContinuousNode}()}, \code{\link{NewDiscreteNode}()},
  \code{\link{DeleteNodes}()}, \code{\link{NodeName}()},
  \code{\link{NodeStates}()}, \code{\link{NodeLevels}()},

}
\examples{

sess <- NeticaSession()
startSession(sess)

nety <- CreateNetwork("yNode",sess)

node1 <- NewContinuousNode(nety,"aNode")
stopifnot(is.NeticaNode(node1))
stopifnot(is.active(node1))
stopifnot(node1$Name=="aNode")

node2 <- NetworkFindNode(nety,"aNode")
stopifnot(node2$Name=="aNode")
stopifnot(node1==node2)

NodeName(node1) <- "Unused"
stopifnot(node1==node2)
node1$Name == node2$Name

noded <- DeleteNodes(node1)
stopifnot(!is.active(node1))
stopifnot(!is.active(node2))
stopifnot(noded$Name=="Unused")
stopifnot(noded == node1)
node1 == node2

DeleteNetwork(nety)
stopSession(sess)

}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{Name}:}{Object of class \code{character} giving the
      Netica name of the node.  Must follow the \code{\link{IDname}} rules. }
    \item{\code{Netica_Node}:}{Object of class \code{externalptr} giving
      the address of the node in Netica's memory space. }
    \item{\code{Net}:}{Object of class \code{\linkS4class{NeticaBN}}, a
      back reference to the network in which this node resides. }
    \item{\code{discrete}:}{Object of class \code{logical} true if the
      node is discrete and false otherwise. }
  }
}
\section{Class-Based Methods}{
  \describe{
    \item{\code{show()}:}{ Prints a description of the node. }
    \item{\code{isActive()}:}{ Returns true if the object currently
      points to a Netica node, and false if it does not. }
    \item{\code{clearErrors(severity)}:}{ Calls \code{clearErrors} on
      the \code{Net$Session} object. }
    \item{\code{reportErrors(maxreport, clear)}:}{ Calls
      \code{reportErrors} on the \code{Net$Session} object. }
    \item{\code{initialize(Name, Net, discrete, ...)}:}{ Initialziation
      function.  Should not be called directly by user code. Use
      \code{\link{NewDiscreteNode}} or \code{\link{NewContinuousNode}}
      instead. }
    \item{\code{deactivate()}:}{ Recursively deactives all nodes
      contained by this network.  Should not be called by user code. }
  }
}
