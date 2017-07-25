\name{AdjoinNetwork}
\alias{AdjoinNetwork}
\title{
  Links an evidence model network to a system model network.
}
\description{
  This function assumes that the two arguments are networks that were
  designed to be connected to one another.  It copies the nodes from
  \code{em} into \code{sm} and then tries to resolve any stub links in
  the copied nodes by connecting them to nodes in \code{sm}.
}
\usage{
AdjoinNetwork(sm, em, setname = character())
}
\arguments{
  \item{sm}{
    An active \code{\link{NeticaBN}} which contains the system state variables.
  }
  \item{em}{
    An active \code{\link{NeticaBN}} which contains variables that
    provide evidence about the system state.
  }
  \item{setname}{
    An optional character vector containing names of node sets (see
    \code{\link{NodeSets}()}).  If supplied, all of the newly created
    nodes are added to the node sets.  Note that all node set names must
    conform to the \code{\link{IDname}} rules.
  }
}
\details{

  This follows the System Model--Evidence Model protocol laid out in
  Almond et al (1999) and Almond and Mislevy (1999).  The idea is that
  the network \code{sm} is a complete network that encodes beliefs about
  the current status of a system.  In particular, it often encodes the
  state of knowledge about a student and is then called a \emph{student
    model}.

  The second network \code{em} is an incomplete network: a fragment of a
  network, some of whose nodes could be stub nodes referring to nodes in
  the \code{sm} (see \code{\link{NodeInputNames}()} and
  \code{\link{NodeKind}()}).  The idea is that the \emph{evidence model}
  provides a set of observable values associated with some diagnostic
  procedure, in particular, a task on an assessment.

  The function \code{AdjoinNetwork(sm,em)} copies all of the nodes from
  \code{em} to \code{sm}, modifying \code{sm} in the process (copy it
  first using \code{\link{CopyNetworks}(sm)} if this is not the
  intention).  It then the parents of each node, \code{emnode}, in
  \code{em} looking for stub nodes (cases where
  \code{\link{NodeParents}(emnode)[j]} has been set to \code{NULL} for some
  parent.  \code{AdjoinNetworks(sm,em)} then tries to find a 
  matching parent by searching for a system model node, \code{smnode}
  named \code{\link{NodeInputNames}(emnode)[j]}.  If it finds one, it
  sets \code{NodeParents(emnode)[j] <- smnode}; if not, it issues a
  warning.
  
  The function \code{AdjoinNetwork(sm,em)} also copies node set
  information from the nodes in \code{em} to their copies in \code{sm}.
  The value of  \code{setname} is concatenated with the current node
  sets of the nodes in \code{em}.  This provides a handy way of
  identifying the evidence model from which the nodes came.

  After findings are entered on the nodes in the evidence model, the can
  be eliminated using \code{\link{AbsorbNodes}()}.
  
}
\value{
  A list containing the newly copied nodes (the instances of the
  \code{em} nodes now in \code{sm}).
}
\references{
  Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223--238.

Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
L. S. (1999). Transfer of information between system and evidence
models. In Artificial Intelligence and Statistics 99, Proceedings
(pp. 181--186). Morgan-Kaufman

}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{AbsorbNodes}()},
  \code{\link{JointProbability}()}, \code{\link{NodeSets}()},
  \code{\link{CopyNodes}()},\code{\link{NetworkFootprint}()}

}
\examples{

## System/Student model
EMSMSystem <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","System.dne",
                           sep=.Platform$file.sep))

## Evidence model for Task 1a
EMTask1a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask1a.dne",
                           sep=.Platform$file.sep))

## Evidence model for Task 2a
EMTask2a <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask2a.dne",
                           sep=.Platform$file.sep))

## Evidence model for Task 2b
EMTask2b <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMTask2b.dne",
                           sep=.Platform$file.sep))


## Task 1a has a footprint of Skill1 and Skill2 (those are the
## referenced student model nodes.  So we want joint the footprint into
## a single clique.
MakeCliqueNode(NetworkFindNode(EMSMSystem, NetworkFootprint(EMTask1a)))
## The footprint for Task2 a is already a clique, so no need to do
## anything. 

## Make a copy for student 1
student1 <- CopyNetworks(EMSMSystem,"student1")
## Monitor nodes for proficiency
student1.prof <- NetworkNodesInSet(student1,"Proficiency")

student1.t1a <- AdjoinNetwork(student1,EMTask1a)
## We are done with the original EMTask1a now
DeleteNetwork(EMTask1a)

## Now add findings
CompileNetwork(student1)
NodeFinding(student1.t1a$Obs1a1) <- "Right"
NodeFinding(student1.t1a$Obs1a2) <- "Right"

student1.probt1a <- JointProbability(student1.prof)

## Done with the observables, absorb them
AbsorbNodes(student1.t1a)
CompileNetwork(student1)
student1.probt1ax <- JointProbability(student1.prof)

## This should be the same
stopifnot(
  sum(abs(student1.probt1a-student1.probt1ax)) <.0001
)

## Now Task 2
student1.t2a <- AdjoinNetwork(student1,EMTask2a,as.IDname("t2a"))
stopifnot(
  setequal(names(student1.t2a),names(NetworkNodesInSet(student1,"t2a")))
)
DeleteNetwork(EMTask2a)

## Add findings
CompileNetwork(student1)
NodeFinding(student1.t2a$Obs2a) <- "Half"

student1.probt1a2a <- JointProbability(student1.prof)

AbsorbNodes(student1.t2a)
CompileNetwork(student1)
student1.probt1a2ax <- JointProbability(student1.prof)

## This should be the same
stopifnot(
  sum(abs(student1.probt1a2a-student1.probt1a2ax)) <.0001
)

## Adjoining networks twice should result in copies with incremented
##   numbers.
AdjoinNetwork(student1,EMTask2b)
AdjoinNetwork(student1,EMTask2b)

DeleteNetwork(student1)
DeleteNetwork(EMTask2b)
DeleteNetwork(EMSMSystem)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
