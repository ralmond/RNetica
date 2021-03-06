\name{NodeVisStyle}
\alias{NodeVisStyle}
\alias{NodeVisStyle<-}
\title{
  Gets/sets the nodes visual appearance in Netica.
}
\description{

  Netica internally has a number of styles it can use to draw a node,
  these including, "Default", "Absent", "Shape", "LabeledBox",
  "BeliefBars", "BeliefLine", and "Meter".  The function
  \code{NodeVisStyle()} returns how the node will be displayed, or
  sets how it will be displayed.

}
\usage{
NodeVisStyle(node)
NodeVisStyle(node) <- value
}
\arguments{
 \item{node}{
    A \code{\linkS4class{NeticaNode}} object whose style is to be determined.
  }
  \item{value}{
    A character string giving the new style.  Must be one of
    \code{"Default"}, \code{"Absent"}, \code{"Shape"},
    \code{"LabeledBox"}, \code{"BeliefBars"}, \code{"BeliefLine"}, or
    \code{"Meter"}. 
  }
}
\details{

  The visual style of the node doesn't make much different in RNetica,
  as R does not display the node.  However, it will control the
  appearance when the node is loaded into the Netica GUI.

}
\value{

  A character string which is one of the values "Default", "Absent",
  "Shape", "LabeledBox", "BeliefBars", "BeliefLine", or "Meter", or
  \code{NA} if an error occurred.

  The setter method returns the modified \code{node} object.

}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeVisStyle_bn}, \nref{SetNodeVisStyle_bn}, 

}
\author{
  Russell Almond
}
\note{

  The Netica documentation indicates that in the future additional
  parameters can be added to the style, for example:
  "LabeledBox,CornerRoundingRadius=3,LineThickness=2"

  With Netica API 5.10 (current on Windows but not Unix/Mac OS) the
  default value seems to be "None" and not "Default".

}
\seealso{
  \code{\linkS4class{NeticaNode}}, \code{\link{NodeVisPos}()}
}
\examples{
sess <- NeticaSession()
startSession(sess)
snet <- CreateNetwork("StylishNet", session=sess)

snode <- NewDiscreteNode(snet,"StyleMe")
stopifnot((NodeVisStyle(snode)=="Default" || NodeVisStyle(snode)=="None"))

NodeVisStyle(snode) <- "Meter"
stopifnot(NodeVisStyle(snode)=="Meter")

DeleteNetwork(snet)
stopSession(sess)

}
\keyword{ interface }
\keyword{ attribute }
