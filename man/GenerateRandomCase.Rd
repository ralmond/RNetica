\name{GenerateRandomCase}
\alias{GenerateRandomCase}
\title{Generates random cases for nodes in a Netica network}
\description{

  This function generates a random instantiation of the nodes in
  \code{nodelist} using the current (that is posterior to any findings
  entered into the net) joint probability distribution of those nodes in
  the network.

}
\usage{
GenerateRandomCase(nodelist, method = "Default", timeout = 100, rng = NULL)
}
\arguments{
  \item{nodelist}{A list of active \code{\link{NeticaNode}} objects, all
  of which belong to the same network.}
  \item{method}{A character scalar used to describe the method used
  select the random numbers.  This should have one of the values
  \code{"Join_Tree_Sampling"}, \code{"Forward_Sampling"} or
  \code{"Default_Sampling"} (see details).  Only the first letter is
  used and case is ignored, so \code{"J"}, \code{"F"} and \code{"D"} are
  legal values.}
  \item{timeout}{This is a number describing how long to carry on
  computations under the forward sampling method.  It is ignored under
  the join tree sampling method or when the default sampling method
  turns out to be join tree.}
  \item{rng}{This either be an existing \code{\link{NeticaRNG}} object
  or \code{NULL} in which case the default random number generator for
  the net is used.}
}
\details{

  The function visits each node in \code{nodelist} and randomly sets a
  finding for that node based on the current beliefs about that node.
  This takes into account any findings previously entered into the
  graph   (including the previously sampled nodes in the list).   In
  particular, to generate multiple cases, the findings need to be
  retracted (using \code{\link{RetractNodeFinding}(node)} or
  \code{\link{RetractNetFindings}(net)} between each generation.

  Netica supports three methods for doing the sampling:
  \describe{

    \item{Join_Tree_Sampling.}{  For each node in turn, the beliefs are
    calculated and a random state is selected and entered as a finding
    (with beliefs propagating).  The network must be compiled for this
    method to work.}

    \item{Forward_Sampling.}{  Random cases are generated directly using
    equations for continuous nodes if these are available.  Random results
    not compatible with the current findings are rejected.  This method is
    not guaranteed to converge, and may be quite slow if the current set
    of findings has a low probability.  It will only run for a period of
    time indicated by \code{timeout} and returns a negative value if it
    does not complete successfully.}

    \item{Default_Sampling.}{  Netica figures out which method is better to
    use.  It uses forward sampling if either rejections aren't a problem
    (presumably because there are no findings) or if the network is
    uncompiled.  Otherwise it uses join tree sampling.}
  }

  The \code{rng} argument can be used to associate a random number
  generator with the generation (see \code{\link{NeticaRNG}}).  If the
  \code{rng} argument is \code{NULL}, then the default random number
  generator for the network is used.  This is either a random number
  generator associated with the network using
  \code{\link{NetworkSetRNG}}, or else the default Netica random number
  generator. 
}
\value{
  Invisibly returns 0 if the case was successfully generated or -1 if
  the case could not be generated (using the forward sampling method).
  In the latter case, a warning is issued as well.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GenerateRandomCase_bn}
}
\author{Russell Almond}
\seealso{
  \code{\link{NetworkSetRNG}()}, \code{\link{NeticaRNG}()},
  \code{\link{NodeFinding}}, \code{\link{RetractNetFindings}}
  \code{\link{ReadFindings}}, \code{\link{NeticaCaseStream}}
}
\examples{

irt5 <- ReadNetworks(paste(library(help="RNetica")$path,
                           "sampleNets","IRT5.dne",
                           sep=.Platform$file.sep))

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5)

GenerateRandomCase(irt5.x)
sapply(irt5.x,NodeFinding)

RetractNetFindings(irt5)

GenerateRandomCase(irt5.x)
sapply(irt5.x,NodeFinding)

casefile <- tempfile("testcase",fileext=".cas")

filestream <- CaseFileStream(casefile)

## This generates a fixed series of random cases and saves them to a
## file. 
N <- 10L
rnodes <- c(list(irt5.theta),irt5.x)
casefile <- tempfile("irt5testcase",fileext=".cas")
filestream <- CaseFileStream(casefile)
rng <- NewNeticaRNG(123456779)
WithOpenCaseStream(filestream,
  WithRNG(rng,
    for (n in 1L:N) {
      GenerateRandomCase(rnodes,rng=rng)
      WriteFindings(rnodes,filestream,n)
      lapply(rnodes,RetractNodeFinding) # Only retract findings for
                                        # generated nodes
    }))

## With constructs force closure even on error exit.
stopifnot(!isNeticaRNGActive(rng),
          !isCaseStreamOpen(filestream))

DeleteNetwork(irt5)

}
\keyword{ interface }
\keyword{ datagen }
