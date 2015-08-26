\name{MakeCliqueNode}
\alias{MakeCliqueNode}
\alias{is.CliqueNode}
\alias{GetClique}
\title{
  Forces a collection of nodes in a Netica network to be in the same
  clique. 
}
\description{
  When a junction tree is compiled, if the nodes are in the same clique,
  it is easier to calculate their joint probability.  The function
  \code{MakeCliqueNode(nodelist)} forces the nodes in \code{nodelist} by
  making a special one state clique node with all of the nodes in
  \code{nodelist} as a parent.
}
\usage{
MakeCliqueNode(nodelist)
is.CliqueNode(x)
GetClique(cliquenode)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodelist}{
    A list of active \code{\link{NeticaNode}} objects from the same
    network. 
  }
  \item{x}{
    An object to be tested to see if it is a clique node.
  }
  \item{cliquenode}{
    A \code{CliqueNode} to be queried.
  }
}
\details{

  It is substantially easier to calculate the joint probability of a
  number of nodes if they are all in the same clique (see
  \code{\link{JointProbability}(nodelist)}.  If it is known that such a
  query will be common, the analyst can take steps to force the nodes
  into the same clique if required.  The Student Model/Evidence Model
  algorithm of Almond and Mislevy (1999) also requires that the student
  model variables that are referenced in an evidence model all be in the
  same clique (although this algorithm is not currently supported by
  Netica).
    
  A node and its parents is always a clique or a subset of a clique in
  the junction tree (see \code{\link{CompileNetwork}()} or
  \code{\link{JunctionTreeReport}()}). This function forces nodes into
  the same clique by creating a new \code{CliqueNode} and making all of
  the nodes in \code{nodelist} parents of the new node.

  The \code{CliqueNode} is a subclass of \code{\link{NeticaNode}}
  (formally, the class is \code{c("CliqueNode", "NeticaNode")}).  It has
  a number of special features.  It's name is always \dQuote{Clique}
  followed by a number.  It only has one state, and it has a special
  \code{"clique"} attribute which records the \code{nodelist} used to
  create it.  The function \code{is.CliqueNode()} tests a node to see if
  it is a clique node, and the function \code{GetClique(node)} retrieves
  the \code{nodelist}.  (This should not be set manually).

  The \code{CliqueNode} objects should, for the most part, behave like
  regular nodes.  However, it is almost certainly a mistake to try and
  set findings on a \code{CliqueNode}.

}
\value{
  The function \code{MakeCliqueNode(nodelist)} returns a new
  \code{CliqueNode} object whose parents are the variables in
  \code{nodelist}.  This behaves in most respects like an ordinary node,
  but it would almost certainly be a mistake to try and enter findings
  for this node.  In particular, deleting the clique node will no longer
  constrain its parents to be in the same clique (although other
  connections in the network may cause the nodes to be placed in the
  same clique).

  The function \code{is.CliqueNode(x)} returns a logical value which is
  true if \code{x} is a clique node.

  The function \code{GetClique(node)} returns the \code{nodelist} used
  to create the clique node.
}
\references{
Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223-238.

Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
L. S. (1999). Transfer of information between system and evidence
models. In Artificial Intelligence and Statistics 99, Proceedings
(pp. 181--186). Morgan-Kaufmann

\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  See the NeticaEx function \code{FormCliqueWith} is the documentation
  for \nref{JointProbability_bn}
}
\author{
  Russell Almond
}
\note{
  Clique nodes only last for the R session that was used to create
  them.  After that, they will appear like ordinary nodes.  They will
  still be present in the network, but the special \code{"clique"}
  attribute will be lost.

  Currently Netica only allows virtual evidence at the node level
  (\code{\link{NodeLikelihood}()}).  I'm lobbying to get Netica to
  support it at the clique level as well.  At which point, this function
  becomes extremely useful.
}
\seealso{
  \code{\link{NeticaNode}},
  \code{\link{JointProbability}()}, \code{\link{AddLink}()},
  \code{\link{JunctionTreeReport}()}
}
\examples{

EMSMSystem <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","System.dne",
                           sep=.Platform$file.sep))
CompileNetwork(EMSMSystem)
## Note that Skill1 and Skill2 are in different cliques
JunctionTreeReport(EMSMSystem)

Skills12 <- NetworkFindNode(EMSMSystem,c("Skill1","Skill2"))
cn <- MakeCliqueNode(Skills12)
cnclique <- GetClique(cn)

stopifnot(
  is.CliqueNode(cn),
  setequal(sapply(cnclique,NodeName),sapply(Skills12,NodeName))
)

CompileNetwork(EMSMSystem)
## Note that Skill1 and Skill2 are in different cliques
JunctionTreeReport(EMSMSystem)

DeleteNodes(cn) ## This clears the clique.

DeleteNetwork(EMSMSystem)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ misc }% __ONLY ONE__ keyword per line
