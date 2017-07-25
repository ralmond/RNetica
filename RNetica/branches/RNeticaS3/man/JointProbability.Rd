\name{JointProbability}
\alias{JointProbability}
\title{
  Calculates the joint probability over several network nodes.
}
\description{
  The Bayesian network, once compiled, gives the joint probability of
  all nodes in the network given the findings.  This function calculates
  the joint probability over all of the nodes its argument and returns
  it as an array.
}
\usage{
JointProbability(nodelist)
}
\arguments{
  \item{nodelist}{
    A list of active \code{\link{NeticaNode}} objects from the same
    network. 
  }
}
\details{
  This calculates the joint probability distribution over two, three or
  more variables in the same network.  Calculating the joint probability
  is easy if all of the nodes are in the same clique, so one might want
  to use the function \code{\link{MakeCliqueNode}(nodelist)}  before
  compiling the network to force the nodes in the same clique.  The
  function can calculate the joint probability table for nodes not in
  the same clique, it just takes longer.

}
\value{
  A multidimensional array given the probabilities of the various
  configurations.  The dimensions correspond to the variables in
  \code{nodelist}, and the dimnames of the result are the result of
  \code{sapply(nodelist,\link{NodeStates})}. 
}
\references{
Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223--238.

Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
L. S. (1999). Transfer of information between system and evidence
models. In Artificial Intelligence and Statistics 99, Proceedings
(pp. 181--186). Morgan-Kaufmann

\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{JointProbability_bn}
}
\author{
  Russell Almond
}
\note{
  One possible use for the joint probability function is to get a joint
  likelihood over the footprint nodes in an evidence model (see Almond
  et al, 1999; Almond & Mislevy, 1999).  However, Netica currently does
  not support inserting a likelihood on a clique, just on a single
  node.
}
\seealso{
  \code{\link{NeticaNode}},\code{\link{NodeBeliefs}()}
  \code{\link{MakeCliqueNode}()}, \code{\link{AddLink}()},
  \code{\link{JunctionTreeReport}()}, \code{\link{MostProbableConfig}()} 
}
\examples{
EMSMMotif <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMSMMotif.dne",
                           sep=.Platform$file.sep))
## Force Skills 1 and 2 into the same clique.
Skills12 <- NetworkFindNode(EMSMMotif,c("Skill1","Skill2"))
cn <- MakeCliqueNode(Skills12)

CompileNetwork(EMSMMotif)

## Prior Joint probability.
prior <- JointProbability(Skills12)
stopifnot (abs(sum(prior)-1) <.0001)

## Find observable nodes
obs <- NetworkNodesInSet(EMSMMotif,"Observable")

NodeFinding(obs$Obs1a1) <- "Right"
NodeFinding(obs$Obs1a2) <- "Wrong"

post <- JointProbability(GetClique(cn))
stopifnot (abs(sum(post)-1) <.0001)

DeleteNetwork(EMSMMotif)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
