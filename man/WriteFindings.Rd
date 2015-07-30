\name{WriteFindings}
\alias{WriteFindings}
\title{Appends the current findings to a Netica case file.}
\description{
  This function writes the current findings for a network as a row in a
  Netica case file.  If \code{filename} already exists, the new row is
  appended on the end of the file.  Variables that are not instantiated
  are written out using the missing code.
}
\usage{
WriteFindings(nodes, pathOrStream, id = -1L, freq = -1.0)
}
\arguments{
  \item{nodes}{
    The a list of active \code{\link{NeticaNode}} objects to be written out.
  }
  \item{pathOrStream}{Either a character scalar giving the path name of
    the file to which the results are to be written, or a
    \code{\link{NeticaCaseStream}} object.  It is recommended that it have
    the extension \dQuote{.cas}.
  }
  \item{id}{An integer scalar giving the case ID.  The default value of
    \code{-1} suppresses the writing of cases.  If an ID is supplied for
    the first case, it should be supplied for all cases.
  }
  \item{freq}{An integer scalar giving the number of cases with the
    currently instantiated set of findings.  The default value \code{-1}
    suppresses writing the cases, implicitly assuming that all cases
    have weight 1.  If supplied for the first row, this should be
    supplied for all rows.
  }
}
\details{
  A case file is a table where the rows represent cases, and the columns
  represent variables.  \code{WriteFindings} writes out the
  currently instantiated value of the nodes in \code{nodeset}.  If a
  node in \code{nodeset} does not currently have a finding attached,
  then the value of \code{\link{CaseFileMissingCode}()} is printed out
  instead.  The values in the columns are separated by the value of
  \code{\link{CaseFileDelimiter}()}.

  There are two special columns in the file.  The column \dQuote{IDnum}
  is set to the value \code{id}, which should contain an integer case
  number.  The column \dQuote{NumCases} is set to the value of
  \code{freq} which should give a weight to assign to the case (in
  various algorithms when \code{freq} is supplied, it is treated as if
  that case was repeated \code{weight} times).  Assigning either of
  these fields a value of \code{-1} means the corresponding column is
  appended to the output.

  The function \code{WriteFindings} will create a new file
  associated with \code{filename} if it does not exist.  In that case it
  will write out a header row containing the variable names followed by
  the current findings as the first case row.  Subsequent calls to
  \code{WriteFindings} with the same \code{filename} append
  additional rows to the end of the file.  In such cases, the
  \code{nodelist} should be the same, and if \code{id} or \code{freq}
  was \code{-1}, it should be in the following calls as well.

}
\value{
  Returns the \code{caseOrStream} argument invisibly.  Note that the
  values of \code{\link{getCaseStreamPos}(stream)},
  \code{\link{getCaseStreamLastId}(stream)}, and
  \code{\link{getCaseStreamLastFreq}(stream)} will be updated to reflect
  the values from the last read record.
}
\references{
  \newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
  \nref{WriteNetFindings_bn}
}
\author{Russell G. Almond}
\seealso{
  \code{\link{CaseFileDelimiter}}, \code{\link{CaseFileMissingCode}},
  \code{\link{NodeFinding}}, \code{\link{RetractNetFindings}}
  \code{\link{ReadFindings}}, \code{\link{NeticaCaseStream}}
}
\examples{

abc <- CreateNetwork("ABC")
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

AddLink(A,B)
AddLink(A,C)
AddLink(B,C)

## Outputfilename
casefile <- tempfile("testcase",fileext=".cas")
filestream <- CaseFileStream(casefile)
stopifnot(is.CaseFileStream(filestream),
          isCaseStreamOpen(filestream))

## Case 1
NodeFinding(A) <- "A1"
NodeFinding(B) <- "B1"
NodeFinding(C) <- "C1"
WriteFindings(list(A,B,C),casefile,1)
RetractNetFindings(abc)

## Case 2
NodeFinding(A) <- "A2"
NodeFinding(B) <- "B2"
NodeFinding(C) <- "C2"
WriteFindings(list(A,B,C),casefile,2)
RetractNetFindings(abc)

## Case 3
NodeFinding(A) <- "A3"
NodeFinding(B) <- "B3"
## C will be missing
WriteFindings(list(A,B,C),casefile,3)
RetractNetFindings(abc)


DeleteNetwork(abc)

}
\keyword{ interface }
\keyword{ IO }% __ONLY ONE__ keyword per line
