\name{orderVars}
\alias{orderVars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts columns of a data frame to ordered factors.}
\description{
  
  In many educational Bayes nets, the states are naturally ordered.
  This function converts the columns of the data frame which 
  correspond to the node names to ordered factors whose levels
  are labeled according to the node's states.
  

}
\usage{
orderVars(data, nodes)
}
\arguments{
  \item{data}{A data frame whose columns (at least some of them)
    correspond to the nodes.}
  \item{nodes}{A named list of node objects giving the characterization
    of each column.}
}
\details{

  This function is very useful when reading in case files,
  or other files where the variable states are labeled with the strings.
  
  It loops over the columns of the data frame.  If there is a member
  of the node list with the same name, then it changes that column into
  an ordered factor using \code{\link{NodeStates}(node)} as the levels.

  Columns which do not correspond to nodes in the list 
  are left unchanged.

}
\value{
  
  A copy of the data frame with the relevant objects converted to 
  ordered factors.
}
\author{Russell Almond}
\seealso{
  \code{\link{NodeStates}}, \code{\link[base]{ordered}}
}
\examples{

SetupDefaultSession()
casefile <- system.file("testData", "abctestcases.cas", package="RNetica")
CaseFileDelimiter("\t")
CaseFileMissingCode("*")
cases <- read.CaseFile(casefile)
cases

abc <- local_create_nets("abc")
a <- NewDiscreteNode(abc,"A",paste0("A",1:3))
b <- NewDiscreteNode(abc,"B",paste0("B",1:3))
c <- NewDiscreteNode(abc,"C",paste0("C",1:3))

orderVars(cases,list(A=a,B=b,C=c))


withr::deferred_run()  # Stops Netica.

}
\keyword{ manip}
\keyword{ utilities}
