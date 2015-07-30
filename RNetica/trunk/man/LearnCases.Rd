\name{LearnCases}
\alias{LearnCases}
\title{Learn Conditional Probability Tables from a Netica Case Stream
}
\description{
  This function updates the conditional probabilities associated with
  the given list of nodes based on the findings associated with that
  node and its parents found in the \code{caseStream} argument, which
  should be a \code{\link{NeticaCaseStream}} object.
}
\usage{
LearnCases(caseStream, nodelist, weight = 1)
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
  \item{weight}{
    A multiplier for the weights of the cases in terms of number
    of observations.  Negative weights unlearn previously learned cases.
}
}
\details{

  This is like calling the function \code{\link{LearnFindings}}
  repeatedly with the values of the nodes set to each of the case rows
  in turn.  Thus, it updates the conditional probability tables for each
  nodes based on observed counts in the case files, taking the current
  probability and the \code{\link{NodeExperience}} as the prior
  distribution.

  If the case stream has a column \code{NumCases}, then the weight
  assigned to Row \eqn{j} is \code{weight*NumCases[j]}.  If the case
  stream does not have such a column, then it is treated as if each
  column has weight 1.  (Among other purposes, this allows case data to
  be stored in a compact format where all of the possible cases are
  enumerated along with a count of repetitions.)  Note that negative
  weights will unlearn cases.

}
\value{
  This function returns the \code{\link{NeticaCaseStream}} used in the
  analysis.  This might have either been passed directly as the
  \code{caseStream} argument, or created from the value of the
  \code{caseStream} argument.  In either case, the stream is closed at
  the end of the function.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{ReviseCPTsByCaseFile_bn}
}
\author{Russell Almond}
\note{
  To learn without using the current probabilities as priors, call
  \code{\link{DeleteNodeTable}} first.
}
\section{Netica Bugs}{
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
  \code{\link{LearnFindings}}, \code{\link{DeleteNodeTable}},
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

casesabb <-
  data.frame(A=c("A1","A1","A1","A1","A1","A2","A2","A2","A2","A2"),
             B1=c("B1","B1","B1","B2","B2","B2","B2","B2","B1","B1"),
             B2=c("B1","B1","B1","B1","B2","B2","B2","B2","B2","B1"))
## LearnCases(casesabb,list(A,B1))
## There is currently a bug in Netica, so that this function does not
## work with memory streams.  As a work around, use proper file streams
## instead.

outfile <- tempfile("casesabb",fileext=".cas")
write.CaseFile(casesabb,outfile)
LearnCases(outfile,list(A,B1))

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

DeleteNetwork(abb)

}
\keyword{ interface }
\keyword{ model }
