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
    An active \code{\linkS4class{NeticaNode}} object whose state titles
    or comments will be accessed. 
  }
  \item{value}{
    A character vector of length \code{\link{NodeNumStates}(\var{node})} which
    provides the new state titles or comments.
  }
}
\details{
  The titles are meant to be a more human readable version of the state
  names and are not subject the \code{\link{IDname}} restrictions.
  These are displayed in the Netica GUI in certain display modes.  The
  comments are meant to be a longer free form notes.

  Both titles and comments are returned as a named character vector with
  names corresponding to the state names.  Therefore one can change a
  single state title or comment by accessing it either using the state
  number or the state name.
}
\value{
  Both \code{NodeStateTitles()} and \code{NodeStateComments()} return a
  character vector of length \code{NodeNumStates(\var{node})} giving the
  titles or comments respectively.  The names of this vector are
  \code{NodeStates(\var{node})}.

  The setter methods return the modified \code{\linkS4class{NeticaNode}}
  object invisibly. 
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
  \code{\linkS4class{NeticaNode}}, \code{\link{NodeStates}()},
  \code{\link{NodeLevels}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)
cnet <- CreateNetwork("CreativeNet", session=sess)

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
stopSession(sess)
}
\keyword{ interface }
\keyword{ attribute }
