\name{NodeEquation}
\alias{NodeEquation}
\alias{NodeEquation<-}
\alias{EquationToTable}
\title{Gets or sets the equation Netica uses to calculate the CPT for a node
}
\description{

  Netica contains a facility to calculate the conditional probability
  table for a node from an equation.  \code{NodeEquation()} gets or sets
  the equation.  \code{EquationToTable()} recalculates the conditional
  probability table associated with the node.
}
\usage{
NodeEquation(node)
NodeEquation(node,autoconvert=TRUE) <- value
EquationToTable(node, numSamples = 25, sampUnc = TRUE, addExist = TRUE)
}
\arguments{
  \item{node}{
    An active \code{\link{NeticaNode}} object that references the node
    whose equation is to manipulated.
  }
  \item{autoconvert}{
    A logical value that indicates whether or not the CPT should be
  recalculated after the equation is set.
  }
  \item{value}{
    A character value giving the equation.  If it has length greater
  than one, it is collapsed with newlines between.
  }
  \item{numSamples}{
   In some cases Netica uses sampling to calculate the CPT.  If it does,
  then this is the number of sample. 
  }
  \item{sampUnc}{
    A logical flag indicating whether or not sampling uncertainty should
  be added to the values.  Note that setting this to FALSE could cause
  zero probabilities for configurations not realized in the sampling,
  which may or may not be a good thing.
  }
  \item{addExist}{
   A logical flag indicating whether or not the sampled values should be
  added to (TRUE) or replace (FALSE) the existing CPT.  Can be used to
  create blended CPTs. 
  }
}
\details{

  This is a fairly minimilistic support for Netica's equation feature.
  Netica equations are strings, but have a very specific syntax (see the
  Netica manual for details).  The RNetica code does no checking before
  passing the value to Netica.

  The function \code{EquationToTable()} builds a conditional probability
  table from the equation and must be called before Netica will update
  the table used in calculations.  The documentation for this function is
  somewhat unclear.  In particular, it is not clear when Netica uses
  sampling to calculating the CPT (this should not be needed in most of
  the examples I've worked with).

  There are two differences between the RNetica implementation and the
  default Netica behavior.  First, equations can be fairly complex.  If
  \code{value} is a character vector, RNetica will concatenate it into a
  single string before passing it to Netica.  Second, by default RNetica
  automatically recalculates the table when the equation is set.  This
  is usually the desired behavior, but can be suppressed by setting
  \code{autoconvert=FALSE}.  

  Constants play a special role in Netica formulas.  A formula can
  reference the value of a constant node even if it is not a marked
  parent of the node whose equation is being defined.  It appears as if
  the value of the constant must be set before the table is created.

}
\value{
  The function \code{NodeValue} returns the equation as a character
  scalar.  The function \code{EquationToTable} returns the \code{node}
  argument invisibly.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{GetNodeEquation_bn}, \nref{SetNodeEquation_bn}, \nref{EquationToTable_bn}

  The reference document for Netica equations:
  \url{http://www.norsys.com/WebHelp/NETICA/X_Equations.htm}
}
\author{
  Russell Almond
}
\note{
  I personally find the Netica equation syntax to be verbose and
  unwieldy.  I have found it easier to calculate the CPTs directly in R
  (using functions from the CPTtools package) and then entering those
  CPTs into Netica.  The functions are provided here mainly for
  completeness.
}

\seealso{
  \code{\link{NodeValue}()},\code{\link{NodeKind}()},
  \code{\link{NodeProbs}()}, \code{\link{Extract.NeticaNode}}
}
\examples{

grn <- CreateNetwork("GradedResponseTest")

## Set up the variables in our network
skill <- NewDiscreteNode(grn,"Skill",c("High","Medium","Low"))
NodeLevels(skill) <- c(1,0,-1)

score1 <- NewDiscreteNode(grn,"Score1",
  c("FullCredit","PartialCredit","NoCredit"))

## Set up a couple of constants for use in formulae
a1 <- NewContinuousNode(grn,"A1")
NodeKind(a1) <- "Constant"
b1_1 <- NewContinuousNode(grn,"B1_1")
NodeKind(b1_1) <- "Constant"
b1_2 <- NewContinuousNode(grn,"B1_2")
NodeKind(b1_2) <- "Constant"
diffB1 <- NewContinuousNode(grn,"DiffB1")

NodeLevels(diffB1) <- seq(-4,4,.5)

NodeValue(a1) <- 1
NodeValue(b1_1) <- -1.5
NodeValue(b1_2) <- 0

## Note, this will generate an error if the values of the constants are
## not set first.
NodeEquation(diffB1) <- "DiffB1() = B1_2 - B1_1"

## I think this should return 1.5, but it return NA.  I'm not sure what
## is happening here?
CalcNodeValue(diffB1)

## This is the rather clunky format for Netica formulae.  This
## implements a graded response model.
dsformula <- c(
"p(Score1 | Skill) =",
   " (Score1==FullCredit)? 1/(1+exp(-1.7*(A1/sqrt(1)*Skill-B1_2)))  :",
   " (Score1==PartialCredit) ? 1/(1+exp(-1.7*(A1/sqrt(1)*Skill-B1_1))) -",
                             " 1/(1+exp(-1.7*(A1/sqrt(1)*Skill-B1_2))) :",
   "1 - 1/(1+exp(-1.7*(A1/sqrt(1)*Skill-B1_1)))"
)

AddLink(skill,score1)
NodeEquation(score1) <- dsformula

score1[]
## Expected value:
#   Skill Score1.FullCredit Score1.PartialCredit Score1.NoCredit
#1   High         0.8455347            0.1404016      0.01406363
#2 Medium         0.5000000            0.4275735      0.07242648
#3    Low         0.1544653            0.5461019      0.29943281

NodeValue(b1_1) <- -2
score1[]    ## Change not propagated yet

EquationToTable(score1)
score1[]  ## Now it changes


DeleteNetwork(grn)

}
\keyword{ Interface }
\keyword{ manip }% __ONLY ONE__ keyword per line
