\name{FadeCPT}
\alias{FadeCPT}
\title{Fades a Netica Conditional Probability Table}
\description{
  This function fades a Netica conditional probability table associated
  with a node (that is, it makes it closer to uniform).  This is used
  when learning conditional probabilities over time, so that newer
  observations will have more weight than older ones.
}
\usage{
FadeCPT(node, degree = 0.2)
}
\arguments{
  \item{node}{
    A \code{\link{NeticaNode}} object.
}
  \item{degree}{
    A scalar value between 0 and 1 providing the amount of fading to be
    done.  A degree of 1 produces a uniform distribution and a degree of
    0 leaves the CPT unchanged. 
}
}
\details{

  This is essentially an exponential filter, with \code{1-degree} as the
  retained weight.  Calling it once with degree of \eqn{1-d} and again
  with degree \eqn{1-f} is equivalent to calling it once with degree
  \eqn{1-df}.

  If \code{prob} are the current probabilities associated with a row of
  the CPT, and \code{expr} is the current experience, then the new
  probabilities will be \code{newprob = normalize(prob* exper *
  (1-degree) + degree)}, and the new experience will be the
  normalization constant.

  This function is often used together with \code{\link{LearnFindings}}
  to down weight old cases when the conditional probabilities are thought
  to be changing slowly over time.

}
\value{
  This function returns the node object.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{FadeCPTable_bn}

}
\author{Russell Almond}
\note{
  Frequently the degree is made time dependent.  If \code{dt} is the
  time elapsed since the last observation, the degree is frequently an
  expression like \code{1-expt(R,dt)}, where \code{R} is a constant less
  than 1 which controls how quickly the CPT is faded.
}
\seealso{
  \code{\link{NodeExperience}}, \code{\link{NodeProbs}},
  \code{\link{LearnFindings}}
}
\examples{

aaa <- CreateNetwork("AAA")
A <- NewDiscreteNode(aaa,paste("A",1:5,sep=""),c("true","false"))

for( i in 1:length(A)) {
  NodeProbs(A[[i]]) <- c(.8,.2)
  NodeExperience(A[[i]]) <- 10
}

deg <- .2
expected <- NodeProbs(A[[1]])*10*(1-deg)+deg

FadeCPT(A[[1]], deg)
stopifnot(
  sum(abs(NodeProbs(A[[1]])-expected/sum(expected))) < .0001,
  abs(NodeExperience(A[[1]])-sum(expected)) < .001
)

## Fading by deg then by deg2 is the same as fading by
## 1-(1-deg)*(1-deg2)
deg2 <- .3
FadeCPT(A[[1]],deg2)
FadeCPT(A[[2]], 1-(1-deg)*(1-deg2))
stopifnot (
  sum(abs(NodeProbs(A[[1]]) - NodeProbs(A[[2]]))) < .0001
)

## Fade by two time units.
lambda <- .8
FadeCPT(A[[3]],1-lambda^2)

## Special cases
FadeCPT(A[[4]],0)
FadeCPT(A[[5]],1)

stopifnot (
  sum(abs(NodeProbs(A[[4]]) -c(.8,.2))) < .0001,
  sum(abs(NodeProbs(A[[5]]) -c(.5,.5))) < .0001
)

DeleteNetwork(aaa)

}
\keyword{ interface }
\keyword{ model }% __ONLY ONE__ keyword per line
