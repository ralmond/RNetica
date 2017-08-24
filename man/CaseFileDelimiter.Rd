\name{CaseFileDelimiter}
\alias{CaseFileDelimiter}
\alias{CaseFileMissingCode}
\title{Gets or sets special characters for case files.
}
\description{
  The function \code{CaseFileDelimiter} sets the field delimiter used
  when writing case files.  The function \code{CaseFileMissingCode} sets
  the character code used for missing values in case files.  If called
  with a null argument, then the current value is returned.
}
\usage{
CaseFileDelimiter(newdelimiter = NULL, session=getDefaultSession())
CaseFileMissingCode(newcode = NULL, session=getDefaultSession())
}
\arguments{
  \item{newdelimiter}{A character scalar containing the new delimiter.  It must
    be either a comma, a space, or a tab.
  }
  \item{session}{An object of type \code{\linkS4class{NeticaSession}}
    which defines the reference to the Netica workspace.}
  \item{newcode}{The character to be used as a delimiter.  It must
    be either an asterisk ("*"), a question mark ("?"), a space, (" ")
    or the empty string ("").
  }
}
\details{
  Case files are essentially a comma separated value files, although tab
  and space are allowed as alternative delimiters.  The space and empty
  string are only allowed as missing value codes when the delimiter is a
  comma.

  The value of the delimiter is global, and applies to all case files
  written from this point on.

  When the argument is null (the default) the current value is returned
  without changing it.
}
\value{
  The value of the delimiter or missing code before the function call as
  a string.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html}:
    \nref{SetCaseFileDelimChar_ns}, \nref{SetMissingDataChar_ns}
    
}
\author{Russell G. Almond}
\note{
  The default R missing code "NA" does not work with Netica.
}
\seealso{
  \code{\link{WriteFindings}}, \code{\link{WriteFindings}},
  \code{\link{read.CaseFile}}, \code{\linkS4class{CaseStream}}
}
\examples{
sess <- NeticaSession()
startSession(sess)

defaultDelim <- CaseFileDelimiter(session=sess) # Get default
d1 <- CaseFileDelimiter("\t", session=sess)
d2 <- CaseFileDelimiter(" ", session=sess)
d3 <- CaseFileDelimiter(",", session=sess)

defaultMiss <- CaseFileMissingCode(session=sess) # Get default
m1 <- CaseFileMissingCode("*", session=sess)
m2 <- CaseFileMissingCode("?", session=sess)
m3 <- CaseFileMissingCode(" ", session=sess)
m4 <- CaseFileMissingCode("", session=sess)
\dontrun{
 ## This should throw an error.
 CaseFileDelimiter(" ", session=sess)
}

m5 <- CaseFileMissingCode("?", session=sess)

d4<- CaseFileDelimiter(" ", session=sess)
\dontrun{
  ## This should throw an error
  CaseFileMissingCode(" ", session=sess)
}
## But this is okay
CaseFileMissingCode("*", session=sess)

stopifnot(d1==defaultDelim, d2=="\t", d3==" ", d4==",")
stopifnot(m1==defaultMiss, m2=="*", m3=="?", m4==" ", m5=="")

## restore defaults
CaseFileDelimiter(defaultDelim, session=sess)
CaseFileMissingCode(defaultMiss, session=sess)

stopSession(sess)
  
}
\keyword{ interface }
\keyword{ IO }

