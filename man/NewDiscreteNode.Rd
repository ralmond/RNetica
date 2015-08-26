\name{NewDiscreteNode}
\alias{NewDiscreteNode}
\alias{NewContinuousNode}
\alias{DeleteNodes}
\title{
  Creates (or destroys) a node in a Netica Bayesian network.
}
\description{
  Creates a new node in the \code{\link{NeticaBN} net}.  Netica Nodes
  can be either discrete, in which case a list of states must be given,
  or continuous, where states are not given.  The function
  \code{DeleteNodes()} deletes a single node or a list of nodes.
}
\usage{
NewDiscreteNode(net, names, states = c("Yes","No"))
NewContinuousNode(net, names)
DeleteNodes(nodes)
}
\arguments{
  \item{net}{
    A \code{\link{NeticaBN}} object point to the network where the nodes
    will be created.
  }
  \item{names}{
    A character vector containing the name or names of the new nodes to
    be created.  The names must follow the \code{\link{IDname}} rules.
  }
  \item{states}{
    Either or character vector, or a list of character vectors.  If it
    is a list, its length should be the same as the length of
    \code{names}.  The character vectors give the names of the states
    for the corresponding node.  The entries should all correspond to
    the \code{IDname} rules.
  }
  \item{nodes}{
    A \code{\link{NeticaNode}} or list of \code{NeticaNode} objects to
    be deleted.  If a list of nodes, all must be from the same network.
  }
}
\details{
  Both \code{NewDiscreteNode()} and \code{NewContinuousNode()} both
  create new nodes in the network \code{net}.  If \code{names} has
  length greater than 1, multiple nodes are created. 

  Netica currently supports two types of nodes.  Discrete nodes
  represent nominal variables.  Continuous nodes represent real
  variables.  Continuous nodes cannot be changed to discrete nodes (or
  vise versa) using calls to the API [this is a different from the
  GUI].  However, a continuous node can be made to behave like a discrete
  node (or vise versa) by setting the \code{\link{NodeLevels}()}
  attribute. 

  \code{NewDiscreteNode()} additionally requires the \code{states}
  argument to set the initial set of states.  (These can be changed
  later through calls to \code{\link{NodeStates}()}).  If \code{states}
  is a character vector, it is used for the state names.  If
  \code{names} has length greater than one, all nodes are created with
  the same set of states.  The default values create a collection of
  binary variables.  If \code{states} is a list, then each entry should
  be a character vector providing the list of states for the
  corresponding new node.

  The function \code{NewContinuousNode()} creates a new continuous
  node.  It appears as if Netica expects continuous nodes to be used in
  one of three ways:  (1) they can be discretized using
  \code{\link{NodeLevels}()}, (2) they can be used as utilities, (3)
  they can be used as constants.  The function \code{\link{NodeKind}()}
  can change a nature node (the default) to a constant or utility node.
  It appears as if Netica will not compile the network unless this is
  done for all nodes.
  
  The function \code{DeleteNode()} deletes a single node or a group of
  nodes.  If multiple nodes are to be deleted in a single call, they
  must all belong to the same network.  Node that any \code{NeticaNode}
  objects that referenced the just deleted nodes will become inactive
  (see \code{\link{is.active}()}).

}
\value{
  For \code{NewDiscreteNode()} or \code{NewContinuousNode()}, this
  returns either a single object of class \code{\link{NeticaNode}} or a
  list of such objects (depending on the length of \code{names}).

  For \code{DeleteNodes()} a list of inactive \code{NeticaNode} objects
  corresponding to the recently deleted nodes. If a node was not found,
  a value of \code{NULL} will be returned instead.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewNode_bn}, \nref{DeleteNode_bn}, \nref{GetNodeType_bn},
  \nref{SetNodeLevels_bn} 
}
\author{
  Russell Almond
}
\note{
  Netica nodes internally contain a pointer back to the net they are
  associated with (see \code{\link{NodeNet}()}), so most functions
  involving nodes don't require the net to be named.  The node creation
  functions are an exception.

  Most functions involving lists of nodes assume that all nodes come
  from the same network.  Netica will generate an error if this is not
  the case.

  Internally, node objects are character vectors giving the name of the node
  with special properties representing the node.  Thus,
  \code{as.character(node)} will usually return the name of the node.
  However, if the node is renamed, that might not work properly.  Using
  \code{NodeName(node)} is generally safer.
}
\seealso{
  \code{\link{CreateNetwork}()}, \code{\link{NeticaNode}},
  \code{\link{NodeName}()}, \code{\link{is.discrete}()},
  \code{\link{is.active}()}, \code{\link{NodeStates}()},
  \code{\link{NodeLevels}()}, \code{\link{NodeKind}()}
}
\examples{
safetyNet <- CreateNetwork("safetyNet")

noded1 <- NewDiscreteNode(safetyNet, "frayed") ## Yes/No
stopifnot(
 NodeName(noded1) == "frayed",
 NodeStates(noded1) == c("Yes", "No"),
 is.discrete(noded1)
 )

## Both variables should have the same set of states
noded23 <- NewDiscreteNode(safetyNet,c("TensionNS","TensionEW"),
            c("High","Med","Low"))
stopifnot(
  all(sapply(noded23,is.active)),
  all(sapply(noded23,is.discrete)),
  NodeNumStates(noded23[[1]]) == 3,
  NodeStates(noded23[[1]])==NodeStates(noded23[[2]])
)

noded45 <- NewDiscreteNode(safetyNet,c("MeshSize","RopeThickness"),
            list(c("Coarse","Fine"),c("Thick","Medium","Thin")))
stopifnot(
  all(sapply(noded45,is.active)),
  all(sapply(noded45,is.discrete)),
  NodeNumStates(noded45[[1]]) == 2,
  NodeNumStates(noded45[[2]]) == 3,
  NodeStates(noded45[[1]])!=NodeStates(noded45[[2]])
)

nodec <- NewContinuousNode(safetyNet, "Area")
stopifnot(
  is.active(nodec),
  is.continuous(nodec),
  NodeName(nodec) == "Area"
)

stopifnot(length(NetworkAllNodes(safetyNet))==6)

DeleteNodes(nodec)
stopifnot(length(NetworkAllNodes(safetyNet))==5)

DeleteNodes(noded45)
stopifnot(length(NetworkAllNodes(safetyNet))==3)

DeleteNetwork(safetyNet)  

}
\keyword{ interface }
\keyword{ graphs }% __ONLY ONE__ keyword per line
