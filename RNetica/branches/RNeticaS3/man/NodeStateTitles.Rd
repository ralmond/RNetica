\name{NodeStateTitles}
\alias{NodeStateTitles}
\alias{NodeStateTitles<-}
\alias{NodeStateComments}
\alias{NodeStateComments<-}
\title{
  Accessors for the titles and comments associated with states of Netica nodes.
}
\description{
  Each state of a \code{\link{NeticaNode}} can have a longer title or
  comments associated with it.  These functions get or set the titles
  or comments.
}
\usage{
NodeStateTitles(node)
NodeStateTitles(node) <- value
NodeStateComments(node)
NodeStateComments(node) <- value
}
\arguments{
  \item{node}{
    An active \code{NeticaNode} object whose state titles or comments will be
    accessed. 
  }
  \item{value}{
    A character vector of length \code{\link{NodeNumStates}(node)} which
    provides the new state titles or names.
  }
}
\details{
  The titles are meant to be a more human readable version of the state
  names and are not subject the the \code{\link{IDname}} restrictions.
  These are displayed in the Netica GUI in certain display modes.  The
  comments are meant to be a longer free form notes.

  Both titles and comments are returned as a named character vector with
  names corresponding to the state names.  Therefore one can change a
  single state title or comment by accessing it either using the state
  number or the state name.
}
\value{
  Both \code{NodeStateTitles()} and \code{NodeStateComments()} return a
  character vector of length \code{NodeNumStates(node)} giving the
  titles or comments respectively.  The names of this vector are
  \code{NodeStates(node)}.

  The setter methods return the modified \code{NeticaNode} object invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeStateTitle_bn},\nref{SetNodeStateTitle_bn},  
  \nref{GetNodeStateComment_bn},\nref{SetNodeStateComment_bn}
}
\author{
  Russell Almond
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeStates}()},
  \code{\link{NodeLevels}()}
}
\examples{
cnet <- CreateNetwork("CreativeNet")

orig <- NewDiscreteNode(cnet,"Originality", c("H","M","L"))
NodeStateTitles(orig) <- c("High","Medium","Low")
NodeStateComments(orig)[1] <- "Produces solutions unlike those typically seen."

stopifnot(
  NodeStateTitles(orig) == c("High","Medium","Low"),
  grep("solutions unlike", NodeStateComments(orig))==1,
  NodeStateComments(orig)[3]==""
  )

sol <- NewDiscreteNode(cnet,"Solution",
       c("Typical","Unusual","VeryUnusual"))
stopifnot(
  all(NodeStateTitles(sol) == ""),
  all(NodeStateComments(sol) == "")
  )

NodeStateTitles(sol)["VeryUnusual"] <- "Very Unusual"
NodeStateComments(sol) <- paste("Distance from typical solution",
                      c("<1", "1--2", ">2"))
stopifnot(
  NodeStateTitles(sol)[3]=="Very Unusual",
  NodeStateComments(sol)[1] == "Distance from typical solution <1"
  )

DeleteNetwork(cnet)

}
\keyword{ interface }
\keyword{ attribute }% __ONLY ONE__ keyword per line
