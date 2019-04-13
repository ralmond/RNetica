\name{woe}
\alias{woe}
\alias{ewoe}
\title{Calculates the weight of evidence for a hypothesis
}
\description{
  Calculates the weight of evidence provided by the current findings for
  the specified hypothesis.  A hypothesis consists of a statement that a
  particular set of nodes (\var{hnodes}) will fall in a specified set
  of states (\var{hstatelists}).  The function \code{ewoe} calculates
  the expected weight of evidence for unobserved nodes.
}
\usage{
woe(enodes, hnodes, hstatelists)
ewoe(enodes, hnodes, hstatelists)
}
\arguments{
  \item{enodes}{A list of \code{\linkS4class{NeticaNode}}s whose
    [expected] weight of evidence is to be calculated.}
  \item{hnodes}{A list of \code{\linkS4class{NeticaNode}}s
    whose values are of interest.  As a special case, a single
    \code{NeticaNode} is treated as a list of length one.}
  \item{hstatelists}{A list of character vectors the same length as
    \var{hnodes} corresponding to the hypothesized state of the nodes
    and representing states of the corresponding node.
    As a special case, a character vector is turned into a list of
    length one.}
}
\details{

  Good (1985) defines the weight of evidence \eqn{E} for a hypothesis
  \eqn{H} as 
  \deqn{W(H:E) = log \frac{P(E|H)}{P(E|\not H)} =
    log \frac{P(H|E)}{P(\not H|E)} - log \frac{P(H)}{P(\not H)}.}

  For the function \code{woe}, the evidence is taken as all findings
  that do not include the hypothesis nodes (\code{hnodes}; findings for
  \code{hnodes} are retracted).

  A hypothesis is defined as a set of nodes and a set of possible values
  that those nodes can take on.  Thus \code{hnodes} is a list of nodes,
  and \code{hstatelists} is a corresponding list of states, each element
  of the list corresponding to a node.  Note that each element of the
  list can be a vector indicating the hypothesis that the corresponding
  node is in one of the list of corresponding states.  Thus
  \code{hnodes=list(Skill1))} and
  \code{hstatelists=list(c("High","Med"))} would indicate that
  the hypothesis is that Skill1 is either High or Med.  Only hypotheses
  that are a Cartesian product of such variable assignments are
  supported.

  Note that if the hypothesis involves a single variable, there is a
  simpler way to calculate weights of evidence which may be useful.  In
  this case, it is sufficient to create a history of the
  \code{\link{NodeBeliefs}} of the target variable as the evidence is
  being entered.  This can then be processed with
  \code{\link[CPTtools]{woeHist}} or \code{\link[CPTtools]{woeBal}}.

  The expected weight of evidence (\code{ewoe}) looks at potential
  future observations to find which might have the highest weight of
  evidence.  The expected weight of evidence is
  \deqn{EWOE(H:E) = \sum_{e in E} W(H:e) P(e|H) .}

  Madigan and Almond (1995) note that the expected weight of evidence
  can be calculated simultaneously for a number of different nodes.  The
  function \code{ewoe} calculates the EWOE for all of the nodes in
  \code{targets}.

  The \code{\link{MutualInfo}} function will calculate the mutual
  information between a single hypothesis node an several potential
  evidence nodes.  As this is a native Netica function, it may be
  faster.  
  
}
\value{
  The function \code{woe} returns the weight of evidence for the
  specified hypothesis in centibans (100*log10\eqn{W(H:E)}).
}
\references{

 Good, I.J. (1985).  Weight of Evidence:  A brief survey.
 In Bernardo, J., DeGroot, M., Lindley,
 D. and Smith, A. (eds).  \emph{Bayesian Statistics 2}. North Holland.
 249--269. 

 Madigan, D. and Almond, R. G. (1995).  Test selection strategies for
 belief networks.  In Fisher, D. and Lenz, H. J. (eds).  \emph{Learning
 from Data:  AI and Statistics V}.  Springer-Verlag.  89--98.

 
}
\author{Russell Almond}
\seealso{
  \code{\link[CPTtools]{woeHist}}, \code{\link[CPTtools]{woeBal}},
  \code{\link{NodeFinding}}, \code{\link{NodeLikelihood}},
  \code{\link{RetractNodeFinding}}, \code{\link{MutualInfo}}
}
\examples{

sess <- NeticaSession()
startSession(sess)

aced2 <- ReadNetworks(file.path(library(help="RNetica")$path,
                           "sampleNets","ACEDMotif2.dne"), session=sess)
aced2.obs <- NetworkNodesInSet(aced2,"Observables")
aced2.prof <- NetworkNodesInSet(aced2,"Proficiencies")

sgp <- aced2.prof$SolveGeometricProblems
CompileNetwork(aced2)
probHist <- matrix(NA,4,NodeNumStates(sgp),
                   dimnames=list(paste("Evidence",1:4,sep=""),
                                 NodeStates(sgp)))
probHist[1,] <- NodeBeliefs(sgp)
rownames(probHist)[1] <- "*Baseline*"

NodeFinding(aced2.obs$CommonRatioMediumTask) <- "True"
probHist[2,] <- NodeBeliefs(sgp)
rownames(probHist)[2] <- "CommonRatioMedium=True"
woe1 <- woe(aced2.obs$CommonRatioMediumTask,
            list(sgp),list(c("High","Medium")))

NodeFinding(aced2.obs$RecursiveRuleMediumTask) <- "False"
probHist[3,] <- NodeBeliefs(sgp)
rownames(probHist)[3] <- "RecursiveRuleMedium=False"
woe2 <- woe(aced2.obs$RecursiveRuleMediumTask,
            list(sgp),list(c("High","Medium")))

NodeFinding(aced2.obs$VisualMediumTask) <- "True"
probHist[4,] <- NodeBeliefs(sgp)
rownames(probHist)[4] <- "VisualMedium=True"
woe3 <- woe(aced2.obs$VisualMediumTask,
            list(sgp),list(c("High","Medium")))

woehist <- woeHist(probHist,c("High","Medium"),"Low")
stopifnot(all(abs((woehist-c(woe1,woe2,woe3)))<.0001))

unobsed <- sapply(aced2.obs,NodeFinding)=="@NO FINDING"
woe(aced2.obs[!unobsed],
    list(aced2.prof$VerbalRuleGeometric,
         aced2.prof$ExplicitGeometric),
    list(c("High"),c("High","Medium")))

ewe <- ewoe(aced2.obs[unobsed],sgp,c("High","Medium"))
ram <- MutualInfo(sgp,aced2.obs[unobsed])
stopifnot(all(order(ewe)==order(ram)))


}
\keyword{ math }
\keyword{ graphs }
