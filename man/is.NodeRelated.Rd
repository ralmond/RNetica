\name{is.NodeRelated}
\alias{is.NodeRelated}
\alias{GetRelatedNodes}
\title{
  Computes topological proprieties of a \code{Netica} network.
}
\description{
  The function \code{is.NodeRelated()} tests to see if \code{relation}
  holds between \code{node1} and \code{node2}.  The function
  \code{GetRelatedNodes} creates a list of all nodes that satisfy the
  \code{relation} with any node in \code{nodelist}.
}
\usage{
is.NodeRelated(node1, node2, relation = "connected")
GetRelatedNodes(nodelist, relation = "connected")
}
\arguments{
  \item{node1}{
    An active \code{\link{NeticaNode}} whose relationship will be tested.
  }
  \item{node2}{
    Another active \code{\link{NeticaNode}} whose relationship will be tested.
  }
  \item{relation}{
    A character scalar which should be one of the values:  "parents",
    "children", "ancestors", "descendents" [sic], "connected",
    "markov_blanket", or "d_connected".  Singular forms and modifiers
    are also allowed, see details.
  }
  \item{nodelist}{
    A list of active \code{\link{NeticaNode}} whose relationship will be tested.
  }
}
\details{
  These functions are useful for testing the topology of a network.
  Each of the functions offers a measure related to the network.  The
  \code{is.NodeRelated()} form tests the relationship between
  \code{node1} and \code{node2}.  The function \code{GetRelatedNodes()}
  returns a list of any nodes for which the relationship holds with any
  of the elements of \code{nodelist}.  The plural and singular forms of
  the relationships can be used with both functions.

  \code{"parent"}, \code{"parents"}.  True if \code{node1} is a parent
  of \code{node2}, or returns a list of parents of the nodes in
  \code{nodelist}. 

  \code{"ancestor"}, \code{"ancestors"}.  True if there is a directed
  (parent to child) path from \code{node1} to \code{node2}, or returns a
  list of ancestors of the nodes in \code{nodelist}. 
  
  \code{"child"}, \code{"children"}.  True if \code{node1} is a child
  of \code{node2}, or returns a list of children of the nodes in
  \code{nodelist}. 

  \code{"descendent"}, \code{"descedents"} [This is the spelling used by
  Netica].  True if there is a directed
  (parent to child) path from \code{node2} to \code{node1}, or returns a
  list of descedants of the nodes in \code{nodelist}. 

  \code{"connected"}.  True if there is a chain (unordered path) from
  \code{node1} to \code{node2}, or returns a list of all nodes connected
  to any of the nodes in \code{nodelist}.

  \code{"markov_blanket"}.  The Markov blanket of \code{nodeset} is the
  a set of nodes that renders the nodes in \code{nodeset} conditionally
  independent of the remaining nodes given the ones in the blanket.  The
  simple form returns true if \code{node2} is in the Markov blanket of
  \code{node1}.

  \code{"d_connected"}.  The rules for d-connection are somewhat complex
  (see Pearl, 1988), but basically \code{node1} and \code{node2} are
  d-connected if they are not independent given the current findings.
  The function returns true if \code{node1} and \code{node2} are
  d-connected or a list of all nodes that are d-connected to the nodes
  in \code{nodelist}.

  In addition, the relation can be modified in the
  \code{GetRelatedNodes()} form by adding one or more modifiers to the
  main relation separated by commas.  The two that are useful in RNetica
  are:

  \code{"include_evidence_nodes"}.  For the \code{"markov_boundary"} and
  \code{"d_connected"} relations indicates whether nodes with findings
  should be included in the result (they would normally not be
  included in the result).

  \code{"exclude_self"}.  For the \code{"ancestors"},
  \code{"descendents"}, \code{"connected"}, and \code{"d_connected"}
  relations, the elements of \code{nodelist} are not initially added to
  the result.
}
\value{
  For \code{is.NodeRelated()} \code{TRUE} or \code{FALSE}, or \code{NA}
  if one of the input nodes was not active.

  For \code{GetNodeRelated()} a list of \code{NeticaNode} objects which
  have the target relationship with one of the nodes in
  \code{nodelist}.  There may be duplicates in this list.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{IsNodeRelated_bn}, \nref{GetRelatedNodes_bn},
  \nref{GetRelatedNodesMult_bn}

  Pearl, J. (1988).  \emph{Probabilistic Reasoning in Intelligent
    Systems.}  Morgan--Kaufmann.
}
\author{
  Russell Almond
}
\note{
  \code{GetRelatedNodes()} uses \code{GetRelatedNodesMult_bn()}, not
  \code{GetRelatedNode_bn()}, but that should not present any serious
  issues.  Also, it always passes an empty list for the
  \code{related_nodes} arguments.  Consequently, the \code{"append"},
  \code{"union"}, \code{"intersection"}, and \code{"subtract"} options
  don't make much sense.  This is only a minor limitation as R provides
  similar functions.
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeParents}()},
  \code{\link{NodeChildren}()}, \code{\link{AddLink}()}
}
\examples{
testnet <- CreateNetwork("ABCDEFG")
###  A   D
###   \ / \
###    C   F - G
###   / \ /
###  B   E
A <- NewDiscreteNode(testnet,"A")
B <- NewDiscreteNode(testnet,"B")
C <- NewDiscreteNode(testnet,"C")
D <- NewDiscreteNode(testnet,"D")
E <- NewDiscreteNode(testnet,"E")
F <- NewDiscreteNode(testnet,"F")
G <- NewDiscreteNode(testnet,"G")

AddLink(A,C)
AddLink(B,C)

AddLink(C,D)
AddLink(C,E)

AddLink(D,F)
AddLink(E,F)

AddLink(F,G)

stopifnot(
  is.NodeRelated(A,C,"parent"),
  is.NodeRelated(D,C,"child"),
  is.NodeRelated(C,G,"ancestor"),
  is.NodeRelated(E,C,"descendent"),
  is.NodeRelated(A,B), ## Same as connected
  is.NodeRelated(D,E,"markov_blanket"), 
  !is.NodeRelated(A,B,"d_connected"), ## No common ancestor
  is.NodeRelated(D,E,"d_connected") ## Common ancestor
)

stopifnot(
  setequal(GetRelatedNodes(F,"parents"),list(D,E)),
  setequal(GetRelatedNodes(C,"children"),list(D,E)),
  setequal(GetRelatedNodes(D,"descendents"),list(D,F,G)),
  setequal(GetRelatedNodes(E,"ancestors"),list(E,C,A,B)),
  setequal(GetRelatedNodes(E,"ancestors,exclude_self"),
           GetRelatedNodes(D,"ancestors,exclude_self")),
  setequal(GetRelatedNodes(A),list(A,B,C,D,E,F,G)), ##All nodes connected
  setequal(GetRelatedNodes(D,"markov_blanket"),list(C,E,F)),
  setequal(GetRelatedNodes(A,"d_connected"),list(A,C,D,E,F,G))
)

DeleteNetwork(testnet)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ graphs }
\keyword{ logic }% __ONLY ONE__ keyword per line
