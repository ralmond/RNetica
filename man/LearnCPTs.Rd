\name{LearnCPTs}
\alias{LearnCPTs}
\title{Learn Conditional Probability Tables with Missing Data.}
\description{
  This function updates the conditional probabilities associated with
  the given list of nodes based on the findings associated with that
  node and its parents found in the \code{caseStream} argument, which
  should be a \code{\link{NeticaCaseStream}} object.  Unlike
  \code{\link{LearnCases}}, these algorithms can support cases with
  missing or latent variables.
}
\usage{
LearnCPTs(caseStream, nodelist, method = "COUNTING", maxIters = 1000L, maxTol = 1e-06, weight = 1)
}
\arguments{
  \item{caseStream}{This should be a \code{\link{NeticaCaseStream}}
    object, or else an object which can be made into a case stream:
    either a pathname for a case file, or a data frame of the format
    described in \code{\link{MemoryCaseStream}}.  The case stream can be
    either opened or closed.  If closed it is reopened before updating.
    In either case, it is closed at the end of the function.
    \bold{Warning}, due to a bug in Netica, memory streams are not
    working and should not be used with Netica API 5.04 or earlier.  See
    below. 
  }
  \item{nodelist}{
    A list of active \code{\link{NeticaNode}} objects that reference the
    conditional probability tables to be updated.
  }
  \item{method}{A character scalar giving the name of the method to be
    used.  This should be one of \dQuote{GRADIENT}, \dQuote{EM} or
    \dQuote{COUNTING} (the default).  See details.
  }
  \item{maxIters}{An integer scalar giving the maximum number of
    interactions for the EM and gradient decent algorithms.
  }
  \item{maxTol}{A real scalar giving the difference in log-likelihood
    required before the EM or gradient decent algorithms to be
    considered converged.
  }
  \item{weight}{
    A multiplier for the weights of the cases in terms of number
    of observations.  Negative weights unlearn previously learned cases.
  }
}
\details{

  This function attempts to update the conditional probability tables of
  the nodes named in \code{nodelist} using the data referenced in the
  first argument.  Three different algorithms are available:
  \emph{Counting}, \emph{EM} and \emph{Gradient Decent}.  The
  \emph{Counting} algorithm cannot handle cases with missing data or
  latent variables in the model.  The \code{method} argument determines
  which method is used.

  The \emph{Counting} algorithm is the same as the one used in
  \code{\link{LearnCases}}.  Cases where either the parent or the child
  variable is missing are ignored when updating the conditional
  probability table for the node, that is the neither affect the
  \code{\link{NodeExperience}} or the \code{\link{NodeProbs}}.  As a
  consequence, models with latent variables cannot be fit with this
  algorithm.

  The \emph{EM} is similar to the \emph{Counting} algorithms, but does more
  intelligent things with missing observations (particularly, missing
  parent variables).  In particular, the complete data case of the
  \emph{EM} algorithm is the same as the counting algorithm.

  The \emph{Gradient Decent} algorithm is an alternative iterative
  algorithm.  According to the Netica documentation it is similar to
  back propagation in neural networks.  Again according to Netica, it is
  faster than EM, but more likely to find a local maxima.  It appears
  not to respect prior information about the conditional probability
  tables, and it sets the node experience to \code{-Inf}.

  Both \code{EM} and \code{Gradient Decent} are an iterative algorithms.
  For these algorithms \code{maxIters} gives the maximum number of
  iterations, and \code{maxTol} gives the convergence criteria (required
  difference in log likelihood).  These parameters are ignored for the
  \emph{Counting} algorithm.  Currently, Netica gives no indication of
  whether the algorithm terminated by achieving convergence (difference
  in log likelihood less than \code{maxTol}) or by exceeding
  \code{maxIters}.  Norsys says they will fix this in an upcoming
  release.
  
  If the case stream has a column \code{NumCases}, then the weight
  assigned to Row \eqn{j} is \code{weight*NumCases[j]}.  If the case
  stream does not have such a column, then it is treated as if each
  column has weight 1.  (Among other purposes, this allows case data to
  be stored in a compact format where all of the possible cases are
  enumerated along with a count of repetitions.)  Note that negative
  weights will unlearn cases.

  
}
\value{
  Currently, \code{NULL} is returned.  In the future, an object
  containing details about the convergence will be returned.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{LearnCPTS_bn}, \nref{NewLearner_bn}, \nref{SetLearnerMaxTol_bn},
  \nref{SetLearnerMaxTol_bn} 
}
\author{Russell G. Almond}
\note{
  The \code{LearnCPTs} function will not update the conditional
  probability table of a node unless \code{\link{NodeExperience}} has
  been set for that node.  Instead it will issue a warning and update
  the other nodes.
}
\section{Netica Bugs}{

  In version 5.04 of the Netica API, there is no indication of whether
  the call to LearnCPTs_bn has converged (terminated because the
  difference in log likelihood is less than \code{maxTol}) or not
  (terminated because the number of iterations exceeded
  \code{maxIters}).  Norsys has indicated that they will add this
  functionality to a later release.

  In version 5.04 of the Netica API, there is a problem with using
  Memory Streams that seems to affect the functions
  \code{\link{LearnCases}} and \code{\link{LearnCPTs}}.  Until this
  problem is fixed, most uses of Memory Streams will require file
  streams instead.  Write the case file using
  \code{\link{write.CaseFile}}, and then create a file stream using
  \code{\link{CaseFileStream}}. 
}
\seealso{
  \code{\link{NodeExperience}}, \code{\link{NodeProbs}},
  \code{\link{NodeFinding}}, \code{\link{FadeCPT}},
  \code{\link{RetractNetFindings}},  \code{\link{LearnFindings}}
  \code{\link{LearnCases}}
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
casesabb <-
  data.frame(A=c("A1","A1","A1","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","B2","B1","B1"),
             B2=c("B1","B1","B1","B1","B2","B2","B2","B2","B2","B1"))
## LearnCPTs(casesabb,list(A,B1))
## There is currently a bug in Netica, so that this function does not
## work with memory streams.  As a work around, use proper file streams
## instead.

outfile <- tempfile("casesabb",fileext=".cas")
write.CaseFile(casesabb,outfile)
LearnCPTs(outfile,list(A,B1))

## Probs for A & B1 modified, but B2 left alone
stopifnot(
  NodeExperience(A)==20,
  NodeExperience(B1)==c(15,15),
  NodeExperience(B2)==c(10,10),
  sum(abs(NodeProbs(A) - .5)) < .001,
  sum(abs(B1[["A1"]] - c(11,4)/15)) < .001,
  sum(abs(B1[["A2"]] - c(4,11)/15)) < .001,
  sum(abs(B2[["A1"]] - c(8,2)/10)) < .001,
  sum(abs(B2[["A2"]] - c(2,8)/10)) < .001
)


## Missing Data
## NAs in parents affect both parent and child.
casesabb1 <-
  data.frame(A=c("A1","A1","NA","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","NA","B1","B1"),
             B2=c("B1","B1","B1","NA","B2","B2","B2","B2","B2","B1"))

outfile1 <- tempfile("casesabb1",fileext=".cas")
write.CaseFile(casesabb1,outfile1)
LearnCPTs(outfile1,list(A,B1,B2))

stopifnot(
  NodeExperience(A)==29,
  NodeExperience(B1)==c(19,19),
  NodeExperience(B2)==c(13,15),
  sum(abs(NodeProbs(A) - c(14,15)/29)) < .001,
  sum(abs(B1[["A1"]] - c(13,6)/19)) < .001,
  sum(abs(B1[["A2"]] - c(6,13)/19)) < .001,
  sum(abs(B2[["A1"]] - c(10,3)/13)) < .001,
  sum(abs(B2[["A2"]] - c(3,12)/15)) < .001
)

DeleteNetwork(abb)

####################################
## Start again with EM learning.

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
casesabb <-
  data.frame(A=c("A1","A1","A1","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","B2","B1","B1"),
             B2=c("B1","B1","B1","B1","B2","B2","B2","B2","B2","B1"))
## LearnCPTs(casesabb,list(A,B1),method="EM")
## There is currently a bug in Netica, so that this function does not
## work with memory streams.  As a work around, use proper file streams
## instead.

outfile <- tempfile("casesabb",fileext=".cas")
write.CaseFile(casesabb,outfile)
LearnCPTs(outfile,list(A,B1),method="EM")

## Complete data, this should look identical to the counting case.
## Note that NodeExperience is no longer an integer
stopifnot(
  abs(NodeExperience(A)-20) < .001,
  sum(abs(NodeExperience(B1)-c(15,15))) < .001,
  NodeExperience(B2)==c(10,10),
  sum(abs(NodeProbs(A) - .5)) < .001,
  sum(abs(B1[["A1"]] - c(11,4)/15)) < .001,
  sum(abs(B1[["A2"]] - c(4,11)/15)) < .001,
  sum(abs(B2[["A1"]] - c(8,2)/10)) < .001,
  sum(abs(B2[["A2"]] - c(2,8)/10)) < .001
)


## Missing Data
## EM deals more intelligently with missing data.
casesabb1 <-
  data.frame(A=c("A1","A1","NA","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","NA","B1","B1"),
             B2=c("B1","B1","B1","NA","B2","B2","B2","B2","B2","B1"))

outfile1 <- tempfile("casesabb1",fileext=".cas")
write.CaseFile(casesabb1,outfile1)
LearnCPTs(outfile1,list(A,B1,B2),method="EM")

stopifnot(
  NodeExperience(A)>29,
  NodeExperience(B1)>c(19,19),
  NodeExperience(B2)>c(13,15)
)

## EM can handle complete latent variable case.
casesabb2 <-
  data.frame(B1=c("B1","B1","B1","B2","B2","B2","B2","NA","B1","B1"),
             B2=c("B1","B1","B1","NA","B2","B2","B2","B2","B2","B1"))

outfile2 <- tempfile("casesabb2",fileext=".cas")
write.CaseFile(casesabb1,outfile2)
LearnCPTs(outfile1,list(A,B1,B2),method="EM")

stopifnot(
  NodeExperience(A)>39,
  NodeExperience(B1)>c(24,23),
  NodeExperience(B2)>c(14,20)
)


DeleteNetwork(abb)

####################################
## One more time with Gradient Decent learning.

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
casesabb <-
  data.frame(A=c("A1","A1","A1","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","B2","B1","B1"),
             B2=c("B1","B1","B1","B1","B2","B2","B2","B2","B2","B1"))
## LearnCPTs(casesabb,list(A,B1),method="GRADIENT")
## There is currently a bug in Netica, so that this function does not
## work with memory streams.  As a work around, use proper file streams
## instead.

outfile <- tempfile("casesabb",fileext=".cas")
write.CaseFile(casesabb,outfile)
LearnCPTs(outfile,list(A,B1),method="GRADIENT")

## Complete data, this should look identical to the counting case.
## Note that NodeExperience is no longer used, and the posterior
## distribution no longer reflects the prior.
stopifnot(
  NodeExperience(B2)==c(10,10),
  sum(abs(NodeProbs(A) - .5)) < .001,
  sum(abs(B1[["A1"]] - c(3,2)/5)) < .001,
  sum(abs(B1[["A2"]] - c(2,3)/5)) < .001,
  sum(abs(B2[["A1"]] - c(8,2)/10)) < .001,
  sum(abs(B2[["A2"]] - c(2,8)/10)) < .001
)

## Gradient algorithm sets experience to -infinity, so need to reset.
NodeExperience(A) <- 10
NodeExperience(B1) <- c(10,10)
NodeExperience(B2) <- c(10,10)


## Missing Data
## GRADIENT deals more intelligently with missing data.
casesabb1 <-
  data.frame(A=c("A1","A1","NA","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","NA","B1","B1"),
             B2=c("B1","B1","B1","NA","B2","B2","B2","B2","B2","B1"))

outfile1 <- tempfile("casesabb1",fileext=".cas")
write.CaseFile(casesabb1,outfile1)
LearnCPTs(outfile1,list(A,B1,B2),method="GRADIENT")

## Gradient algorithm sets experience to -infinity, so need to reset.
NodeExperience(A) <- 10
NodeExperience(B1) <- c(10,10)
NodeExperience(B2) <- c(10,10)

## GRADIENT can handle complete latent variable case.
casesabb2 <-
  data.frame(B1=c("B1","B1","B1","B2","B2","B2","B2","NA","B1","B1"),
             B2=c("B1","B1","B1","NA","B2","B2","B2","B2","B2","B1"))

outfile2 <- tempfile("casesabb2",fileext=".cas")
write.CaseFile(casesabb1,outfile2)
LearnCPTs(outfile1,list(A,B1,B2),method="GRADIENT")

DeleteNetwork(abb)


}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
