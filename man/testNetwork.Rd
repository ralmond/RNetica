\name{testNetwork}
\alias{testNetwork}
\title{Performs a classification recovery test on a network.}
\description{

  This function takes a set of cases represented with a
  \code{\linkS4class{CaseStream}} and a network, and trys to classify the
  values of the \code{targetNodes} using the data in the cases.  It
  returns an object of class \code{\linkS4class{NetworkTester}} which
  contains statistics about the recovery of the target nodes from the
  data.
  

}
\usage{
testNetwork(targetNodes, dataStreams, ignoreNodes = list())
}
\arguments{
  \item{targetNodes}{A list of active \code{\linkS4class{NeticaNode}}
    objects which represent the nodes to be tested.}
  \item{dataStreams}{A list of active (open)
    \code{\linkS4class{CaseStream}} objects containing the test data.}
  \item{ignoreNodes}{A (possibly empty) list of active
    \code{\linkS4class{NeticaNode}} objects whose values will be ignored
    (not instantiated) during the tests.}
}
\details{

  The tester behaves a follows:  It opens a
  \code{\linkS4class{NetworkTester}} object to hold the results.  It
  then runs through the case streams.  For each case, it instantiates
  each node in the network to the findings as found in the case stream,
  except for those nodes in \code{targetNodes} or \code{ignoreNodes}.
  It then looks at the \code{NodeBeliefs} associated with the
  \code{targetNodes} and compares these with the actual value of the
  target nodes as found in the case stream.  It accumulates statistics
  related to the accuracy.

  After all case streams are processed, the
  \code{\linkS4class{NetworkTester}} object is returned and its
  statistics can be queried. See \code{\link{testerConfusion}} for a
  list of available results.

}
\value{

  An object of class \code{\linkS4class{NetworkTester}}.


}
\references{

  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 7.

  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{NewNetTester_bn}, \nref{DeleteNetTester_bn},
  \nref{TestWithCaseset_bn}, \nref{GetTestConfusion_bn},
  \nref{GetTestErrorRate_bn}, \nref{GetTestLogLoss_bn},
  \nref{GetTestQuadraticLoss_bn}

}
\author{Russell Almond}
\note{

  Netica's internal net tester object can add additional case sets at a
  later time.  However, RNetica, closes this object at the completion of
  the call to \code{testNetwork}, so a new test needs to be performed to
  add more data to the test.

}
\seealso{

  The output of \code{\link{testNetwork}} is \code{\linkS4class{NetworkTester}}.
 
  Accessors for \code{\linkS4class{NetworkTester}}
  \code{\link{testerConfusion}}, \code{\link{testerErrorRate}},
  \code{\link{testerIgnore}}, \code{\link{testerKappa}}, 
  \code{\link{testerLambda}}, \code{\link{testerLogLoss}},
  \code{\link{testerNet}}, \code{\link{testerQuadraticLoss}}, 
  \code{\link{testerTarget}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

irt5 <- ReadNetworks(system.file("sampleNets","IRT5.dne",
                                 package="RNetica"),
                     session=sess)

irt5.theta <- NetworkFindNode(irt5,"Theta")
irt5.x <- NetworkFindNode(irt5,paste("Item",1:5,sep="_"))

CompileNetwork(irt5)


## This generates a fixed series of random cases and saves them to a
## file. 
N <- 100L
rnodes <- c(list(irt5.theta),irt5.x)
casefile <- tempfile("irt5testcase",fileext=".cas")
filestream <- CaseFileStream(casefile, session=sess)
rng <- NewNeticaRNG(123456779, session=sess)
WithOpenCaseStream(filestream,
  WithRNG(rng,
    for (n in 1L:N) {
      GenerateRandomCase(rnodes,rng=rng)
      WriteFindings(rnodes,filestream,n)
      lapply(rnodes,RetractNodeFinding) # Only retract findings for
                                        # generated nodes
    }))


irt5.test <- testNetwork(list(irt5.theta),OpenCaseStream(filestream))
summary(irt5.test)


DeleteNetwork(irt5)
stopSession(sess)

}
\keyword{ interface }
\keyword{ classif }
