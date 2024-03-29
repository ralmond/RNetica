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

  This follows the System Model--Evidence Model (or Hub-and-spoke)
  protocol laid out in Almond et al (1999) and Almond and Mislevy
  (1999).  The idea is that the network \code{sm} is a complete network
  that encodes beliefs about the current status of a system.  In
  particular, it often encodes the state of knowledge about a student
  and is then called a \emph{student model}.

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
  \code{\linkS4class{NeticaNode}}, \code{\link{AbsorbNodes}()},
  \code{\link{JointProbability}()}, \code{\link{NodeSets}()},
  \code{\link{CopyNodes}()},\code{\link{NetworkFootprint}()}

}
\examples{
sess <- NeticaSession()
startSession(sess)

## System/Student model
EMSMSystem <- ReadNetworks(system.file("sampleNets","System.dne",
                                       package="RNetica"),
                                       session=sess)


## Evidence model for Task 1a
EMTask1a <- ReadNetworks(system.file("sampleNets","EMTask1a.dne",
                                      package="RNetica"),
                                      session=sess)

## Evidence model for Task 2a
EMTask2a <- ReadNetworks(system.file("sampleNets","EMTask2a.dne",
                                      package="RNetica"),
                                      session=sess)

## Evidence model for Task 2b
EMTask2b <- ReadNetworks(system.file("sampleNets","EMTask2b.dne",
                                      package="RNetica"),
                                      session=sess)

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
stopifnot(setequal(student1$listNodes(),
 c("CliqueNode1", "Obs1a1", "Obs1a2", "Skill1", "Skill2", "Skill3")))

## We are done with the original EMTask1a now
DeleteNetwork(EMTask1a)

## Now add findings
CompileNetwork(student1)
NodeFinding(student1.t1a$Obs1a1) <- "Right"
NodeFinding(student1.t1a$Obs1a2) <- "Right"

student1.probt1a <- JointProbability(student1.prof)
## Done with the observables, absorb them
if (NeticaVersion(sess)$number >600) {
AbsorbNodes(student1.t1a)
stopifnot(setequal(student1$listNodes(),
 c("CliqueNode1", "Skill1", "Skill2", "Skill3")))
}

CompileNetwork(student1)
student1.probt1ax <- JointProbability(student1.prof)

## This should be the same
stopifnot(
  sum(abs(student1.probt1a-student1.probt1ax)) <.0001
)

## Now Task 2
student1.t2a <- AdjoinNetwork(student1,EMTask2a,as.IDname("t2a"))
if (NeticaVersion(sess)$number >600) {
stopifnot(
  setequal(names(student1.t2a),names(NetworkNodesInSet(student1,"t2a")))
)
stopifnot(setequal(student1$listNodes(),
 c("CliqueNode1", "Obs2a", "Skill1", "Skill2", "Skill3")))
}
DeleteNetwork(EMTask2a)

## Add findings
CompileNetwork(student1)
NodeFinding(student1.t2a$Obs2a) <- "Half"

student1.probt1a2a <- JointProbability(student1.prof)

if (NeticaVersion(sess)$number >600) {
AbsorbNodes(student1.t2a)
stopifnot(setequal(student1$listNodes(),
 c("CliqueNode1", "Skill1", "Skill2", "Skill3")))
}
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
if (NeticaVersion(sess)$number >600) {
stopifnot(setequal(student1$listNodes(),
 c("CliqueNode1", "Obs2b", "Obs2b1", "Skill1", "Skill2", "Skill3")))
}
DeleteNetwork(student1)
DeleteNetwork(EMTask2b)
DeleteNetwork(EMSMSystem)
stopSession(sess)

}
\keyword{ interface }
\keyword{ manip }
