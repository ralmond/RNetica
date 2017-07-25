\name{LearnFindings}
\alias{LearnFindings}
\title{Learn Netica conditional probabilities from findings.}
\description{
  This function updates the conditional probabilities associated with
  the given list of nodes based on the findings associated with that
  node and its parents.  Before calling this function the findings to be
  learned should be set using \code{\link{NodeFinding}}.
}
\usage{
LearnFindings(nodes, weight = 1)
}
\arguments{
  \item{nodes}{
    A list of active \code{\link{NeticaNode}} objects that reference the
    conditional probability tables to be updated.
  }
  \item{weight}{The weight of the current observation in terms of number
    of observations.  Negative weights unlearn previously learned cases.
  }
}
\details{

  For the purposes of this function, Netica regards the probabilities in
  Row \eqn{j} of the CPT for each selected node as having an independent
  Dirichlet distribution with parameters \eqn{(a_{j1},\ldots,a_{jK}) =
  n_j (p_{j1},\ldots,p_{jK})} where \eqn{p_{jk}} is the probability
  associated with State \eqn{k} in Row \eqn{j} and \eqn{n_j} is the
  experience associated with Row \eqn{j}.

  If \code{LearnFindings} is called on a node which is currently
  instantiated to State \eqn{k} and whose parents are currently
  instantiated to the configuration which selects Row \eqn{j} of the
  table, then \eqn{n'_j = n_j + weight} and \eqn{a'_{jk} =
  a_{jk}+weight} with all other values remaining the same.  The new
  conditional probabilities are \eqn{p'_{jk} = a'_{jk}/n'_j}.

  The function \code{\link{FadeCPT}} is often used between calls to
  \code{LearnFindings} to down weight old cases when the conditional
  probabilities are thought to be changing slowly over time.

}
\value{
  This returns the list of nodes whose conditional probability tables
  have been modified.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{ReviseCPTsByFindings_bn}

}
\author{Russell G. Almond}
\note{
  Do not confuse this function with \code{\link{NodeFinding}}.
  \code{NodeFinding} instantiates a node and updates all of the other
  beliefs associated with a node to reflect the new evidence.
  \code{LearnFindings} incorporates the current case (the currently
  instantiated set of findings) into the CPTs for the nodes.

  The \code{LearnFindings} function will not update the conditional
  probability table of a node unless \code{\link{NodeExperience}} has
  been set for that node.  Instead it will issue a warning and update
  the other nodes.

}
\seealso{
  \code{\link{NodeExperience}}, \code{\link{NodeProbs}},
  \code{\link{NodeFinding}}, \code{\link{FadeCPT}},
  \code{\link{RetractNetFindings}},  \code{\link{LearnCases}},
  \code{\link{LearnCPTs}} 
}
\examples{

abb <- CreateNetwork("ABB")
A <- NewDiscreteNode(abb,"A",c("A1","A2"))
B1 <- NewDiscreteNode(abb,"B1",c("B1","B2"))
B2 <- NewDiscreteNode(abb,"B2",c("B1","B2"))

AddLink(A,B1)
AddLink(A,B2)

A[] <- c(.5,.5)
NodeExperience(A) <- 10

B1["A1"] <- c(.8,.2)
B1["A2"] <- c(.2,.8)
B2["A1"] <- c(.8,.2)
B2["A2"] <- c(.2,.8)
NodeExperience(B1) <- c(10,10)
NodeExperience(B2) <- c(10,10)

## First Case
NodeFinding(A) <- "A1"
NodeFinding(B1) <- "B1"
NodeFinding(B2) <- "B2"

LearnFindings(list(A,B1))
## Probs for A & B1 modified, but B2 left alone
stopifnot(
  NodeExperience(A)==11,
  NodeExperience(B1)==c(11,10),
  NodeExperience(B2)==c(10,10),
  sum(abs(NodeProbs(A) - c(6,5)/11)) < .001,
  sum(abs(B1[["A1"]] - c(9,2)/11)) < .001,
  sum(abs(B1[["A2"]] - c(2,8)/10)) < .001,
  sum(abs(B2[["A1"]] - c(8,2)/10)) < .001,
  sum(abs(B2[["A2"]] - c(2,8)/10)) < .001
)

## Second Case
RetractNetFindings(abb)
NodeFinding(A) <- "A2"
NodeFinding(B1) <- "B1"
NodeFinding(B2) <- "B1"

LearnFindings(list(A,B1))
## Probs for A & B1 modified, but B2 left alone
stopifnot(
  NodeExperience(A)==12,
  NodeExperience(B1)==c(11,11),
  NodeExperience(B2)==c(10,10),
  sum(abs(NodeProbs(A) - c(6,6)/12)) < .001,
  sum(abs(B1[["A1"]] - c(9,2)/11)) < .001,
  sum(abs(B1[["A2"]] - c(3,8)/11)) < .001,
  sum(abs(B2[["A1"]] - c(8,2)/10)) < .001,
  sum(abs(B2[["A2"]] - c(2,8)/10)) < .001
)

## Retract Case 2
LearnFindings(list(A,B1),-1)
## Back to where we were before Case 1
stopifnot(
  NodeExperience(A)==11,
  NodeExperience(B1)==c(11,10),
  NodeExperience(B2)==c(10,10),
  sum(abs(NodeProbs(A) - c(6,5)/11)) < .001,
  sum(abs(B1[["A1"]] - c(9,2)/11)) < .001,
  sum(abs(B1[["A2"]] - c(2,8)/10)) < .001,
  sum(abs(B2[["A1"]] - c(8,2)/10)) < .001,
  sum(abs(B2[["A2"]] - c(2,8)/10)) < .001
)

DeleteNetwork(abb)

}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
