\name{local_create_nets}
\alias{local_create_nets}
\alias{local_copy_nets}
\alias{local_load_nets}
\alias{local_RNetica_net}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Temporary Netica Networks}
\description{

  A common case when testing, is to need to create a network then make
  sure it is deleted when the testing is completed.  In the
  \code{testthat} framework these are called \code{local_} functions.
  They wrap the functions \code{\link{CreateNetwork}},
  \code{\link{CopyNetworks}}, and \code{\link{ReadNetworks}} with a 
  call to \code{withr::\link[withr::defer]{defer}} function to ensure
  that \code{\link{DeleteNetwork}()} is called when the network is no
  longer needed.

}
\usage{
local_create_nets(names, session=getDefaultSession(), env=parent.frame())
local_copy_nets(nets, newnames, options = character(0), env = parent.frame())
local_load_nets(netpaths, session=getDefaultSession(), env=parent.frame())
local_RNetica_net(filename, session=getDefaultSession(), env=parent.frame())
}
\arguments{
  \item{names}{Character vector of name(s) for the new network(s).}
  \item{nets}{The network to be copied or a list of networks to be copied.}
  \item{netpaths}{The pathnames of the network files loaded networks to be copied.}
  \item{newnames}{A character vector of new names for the copied networks.}
  \item{options}{Options passed to \code{\link{CopyNetworks}}.}
  \item{filename}{The name of a file in the "sampleNets" directory of
    the \code{RNetica} package.}
  \item{session}{A \code{\link{NeticaSession}} object in which the nets
    should be to created.}
  \item{env}{An environment passed to
    \code{withr::\link[withr]{defer}},
    \code{\link{DeleteNetwork}} will be run when this environment exits.}
}
\details{

  The intended use for this is inside a
  \code{testthat::\link[testthat::test_that]{test_that}} test. In
  particular, usually when testing there is a need to delete the network
  after the testing is completed.  This function implements the
  testthat \code{local_} protocol.  The \code{env} argument is passed to
  \code{withr::\link[withr]{defer}} to control when the cleanup code is
  run.  The default value runs when the calling function exits.

  The \code{load_RNetica_net} is a special shortcut for loading one of
  the built-in networks distributed with \code{RNetica}.  These can be
  found by running
  \code{list.files(system.file("sampleNets",package="RNetica"))}. 


}
\value{
  
  Returns a \code{\linkS4class{NeticaBN}} object or a list of such
  objects (if called with a list of names, paths, or nets).
  

}
\references{

  See \code{vignette("test-fixtures","testthat")} for information on the
  use of \code{"local_"} files in \code{testthat}.

}
\author{Russell Almond}
\note{
  Additional sample networks are distributed with the \code{PNetica}
  package. 

}
\seealso{
  \code{\link{NeticaSession}}, \code{\link{getDefaultSession}},
  \code{\link{CreateNetworks}}, \code{\link{ReadNetworks}},
  \code{\link{CopyNetworks}}, \code{withr::\link[withr::defer]{defer}}
  \code{\link{SetupDefaultSession}}

}
\examples{
SetupDefaultSession()
testLoader <- function() {
  bna <- local_RNetica_net("System.dne")
  print(bna)
}
GetNamedNetworks("System") # Should be null
testLoader() # should be active
GetNamedNetworks("System") # Should be inactive (deleted)

withr::deferred_run()  # Stops Netica.
}
\keyword{ utilties }
\concept{ testing }
\concept{ NeticaBN }

