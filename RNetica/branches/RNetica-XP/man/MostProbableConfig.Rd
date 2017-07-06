\name{MostProbableConfig}
\alias{MostProbableConfig}
\title{
  Finds the configuration of the nodes most likely to have lead to
  observed findings.
}
\description{
  Findings a set of values for each of the nodes in \code{nodelist} such
  that the probability of that value set is highest given the state of
  any findings entered into the network.  This is sometimes called the
  \dQuote{Most Probable Explanation} for the findings.

}
\usage{
MostProbableConfig(net,nth = 0)
}
\arguments{
  \item{net}{
    An active and compiled \code{\link{NeticaBN}}.
  }
  \item{nth}{
    Leave this at its default value of zero, it is for future expansion.
  }
}
\details{

  The most probable configuration of the nodes in the Bayesian network
  is the set of values for each of the nodes in the network which have
  the highest joint probability.  This may or may not be thee same as
  setting the value of each node to the value that maximizes its
  \code{\link{NodeBeliefs}()}.  Pearl (1988) describes a special
  max-propagation algorithm which can calculate the most likely
  configuration of nodes in a Bayesian network.  This function runs that
  algorithm.  The probability that is maximized is the posterior
  probability given the findings.

  Note that this produces a configuration over all of the nodes in the
  network, not just the nodes in some particular set.  The Netica
  documentation suggests running \code{\link{AbsorbNodes}()} over the
  unnecessary nodes first.  Another possibility (if the set of
  interesting nodes is small) is to call
  \code{\link{JointProbability}()} on the affected nodes and then
  find the max of that.
  
}
\value{
  A character vector whose names are the names of the nodes in the
  network (see \code{\link{NetworkAllNodes}(net)}) and whose values are
  the names of the states that maximize the posterior probability given
  the findings.
}
\references{

  Pearl, J. (1988) \emph{Probabilistic Reasoning in Intelligent Systems:
    Networks of Plausible Inference.} Morgan Kaufmann.

  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{MostProbableConfig_bn}
}
\author{
  Russell Almond
}
\note{

  The Bayesian network literature also discusses algorithms for the 2nd,
  3rd, 4th, etc. most likely findings.  These algorithms are slightly
  more difficult to implement, but are possible on future plans for the
  Netica API, as it offers the \code{nth} argument to the function
  \nref{MostProbableConfig_bn}.  At this point in time, it is an error
  to set \code{nth} to anything but 0.
}
\section{Warning}{

  The documentation for the Netica function \nref{MostProbableConfig_bn}
  states that likelihood findings (\code{\link{NodeLikelihood}()}) are not
  handled properly in \code{MostProbableConfig()}.  Seems to indicate
  that this works properly, but some caution is still advised.

}
\seealso{
  \code{\link{NeticaBN}}, \code{\link{NodeBeliefs}()},
  \code{\link{EnterNegativeFinding}()},
  \code{\link{RetractNodeFinding}()}, \code{\link{NodeFinding}()}
  \code{\link{JointProbability}()},
  \code{\link{FindingsProbability}()}
}
\examples{

EMSMMotif <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","EMSMMotif.dne",
                           sep=.Platform$file.sep))

CompileNetwork(EMSMMotif)
obs <- NetworkNodesInSet(EMSMMotif,"Observable")
prof <- NetworkNodesInSet(EMSMMotif,"Proficiency")

NodeFinding(obs$Obs1a1) <- "Right"
NodeFinding(obs$Obs1a2) <- "Wrong"
NodeFinding(obs$Obs1b1) <- "Right"
NodeFinding(obs$Obs1b2) <- "Wrong"

mpe <- MostProbableConfig(EMSMMotif)

## Observed values should be set at their findings level.
stopifnot (
  mpe$Obs1a1 == "Right",
  mpe$Obs1a2 == "Wrong",
  mpe$Obs1b1 == "Right",
  mpe$Obs1b2 == "Wrong"
)

## MPE for just proficiency variables.
mpe[names(prof)]

DeleteNetwork(EMSMMotif)

}
\keyword{ interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
