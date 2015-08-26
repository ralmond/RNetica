\name{NetworkNodeSetColor}
\alias{NetworkNodeSetColor}
\title{
  Returns or sets a display colour to use with a Netica node.b
}
\description{
  Returns the display colour associated with a node set or sets the node
  set colour to a specified value.  The colour of the node in the Netica
  GUI will be the colour of the highest priority node set associated with
  the node (see \code{\link{NetworkSetPriority}()}.
}
\usage{
NetworkNodeSetColor(net, setname, newcolor)
}
\arguments{
  \item{net}{
    An active \code{\link{NeticaBN}} object representing the network.
  }
  \item{setname}{
    A character scalar giving the name of the node set to be coloured.
  }
  \item{newcolor}{
    An optional scalar of any of the three kind of R colours, i.e., either a
    colour name (an element of \code{\link[grDevices]{colors}()}),
    a hexadecimal string of the form \code{"#rrggbb"} or
    \code{"#rrggbbaa"} (see \code{\link[grDevices]{rgb}()}), or an
    integer \code{i} meaning \code{\link[grDevices]{palette}()[i]}.
    Non-string values are coerced to integer.  There are two special
    values:  \code{NA} is used to indicate that the set should not have
    a colour associated with it.  If \code{newcolor} is missing, then the
    existing colour is returned and not changed.
  }
}
\details{
  Netica determines the visual style of a node by stepping through the
  node sets to which the node belongs in priority order (see
  \code{\link{NetworkSetPriority}()}) .  Each node set
  can either have a colour set, or a flag set to indicate that the next
  node in order or priority should be used to determine the appearance
  of the node.  The expression \code{NetworkNodeSetColor(net,setname,NA)}
  sets the flag so that membership in \code{setname} does not affect the
  display of the node.

  The function \code{NetworkNodeSetColor(net,setname,colour)} sets the
  colour associated with the visual display of these nodes (this is only
  visible when the network is open in the Netica GUI).  The colour can be
  specified in any of the usual ways that colours are specified in R (see
  \code{\link[grDevices]{col2rgb}()}).  The special value \code{NA} is
  used to indicate that the set should be \sQuote{transparent}, that is
  the colour of the next set in priority should be used to colour the node.

  The function \code{NetworkNodeSetColor(net,setname)}, with the third
  argument missing, returns the current node set colour instead of
  setting it.
}
\value{
  The old value of the node colour as hexadecimal string value of the
  form \code{"#rrggbb"}. 
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLurl/Manual/index.html}:
  \nref{ReorderNodesets_bn}, \nref{SetNodesetColor_bn}
}
\author{
  Russell Almond
}
\note{
  The colors of the built-in Netica node sets serve as the ultimate
  default for the display of nodes.  These cannot be set or queried
  through this function.  (This is a limitation of the Netica API).
}
\seealso{
  \code{\link{NeticaNode}}, \code{\link{NodeSets}()},
  \code{\link{NetworkNodeSets}()}, \code{\link[grDevices]{col2rgb}()},
  \code{\link[grDevices]{rgb}()},
  \code{\link{NetworkNodesInSet}()}, \code{\link{NetworkSetPriority}()}
}
\examples{

nsnet <- CreateNetwork("NodeSetExample")

Ability <- NewContinuousNode(nsnet,"Ability")

X1 <- NewDiscreteNode(nsnet,"Item1",c("Right","Wrong"))
EssayScore <- NewDiscreteNode(nsnet,"EssayScore",paste("level",5:0,sep="_"))

Value <- NewContinuousNode(nsnet,"Value")
NodeKind(Value) <- "Utility"
Placement <- NewDiscreteNode(nsnet,"Placement",
     c("Advanced","Regular","Remedial"))
NodeKind(Placement) <- "Decision"

NodeSets(Ability) <- "ReportingVariable"
NodeSets(X1) <- "Observable"
NodeSets(EssayScore) <- c("ReportingVariable","Observable")

## Default colour is NA (transparent)
stopifnot(
  is.na(NetworkNodeSetColor(nsnet,"Observable"))
)

## Make Reporting variables a pale blue
NetworkNodeSetColor(nsnet,"ReportingVariable",rgb(1,.4,.4))
stopifnot(
  NetworkNodeSetColor(nsnet,"ReportingVariable") == "#ff6666"
)
## Using R (nee X11) color list.
NetworkNodeSetColor(nsnet,"Observable","wheat2")
stopifnot(
  NetworkNodeSetColor(nsnet,"ReportingVariable") == "#ff6666"
)

DeleteNetwork(nsnet)

}
\keyword{ interface }
\keyword{ attributes }% __ONLY ONE__ keyword per line
