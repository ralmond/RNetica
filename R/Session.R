
### Package name string for .Call (this allows me to temporarily change the
### Name of the package while getting forked.
RNetica <- "RNeticaXR"


### Redo a Netica Session as an R6 Object

## R does not provide enough support for manipulating pointer objects
## in R code.   In particular, new("externalptr-class") does not
## necessarily produce a null pointer.  So we provide a method.  Also,
## a simple test for whether or not the pointer is NULL.
CCodeLoaded <- FALSE

externalptr <- function () {
  if (CCodeLoaded) {
    .Call("RX_make_exptr",NULL,PACKAGE=RNetica)
  } else {
    ## We are building prototoype objects during the namespace loading
    ## cycle, but before the C code is loaded.  Just create a pointer
    ## with an arbitrary address.
    new("externalptr")
  }
}

is_null_ptr <- function(ptr) {
  if (!is(ptr,"externalptr-class")) return (NA)
  .Call("RX_is_null_ptr",ptr,PACKAGE=RNetica)
}


NeticaSession <-
  setRefClass("NeticaSession",
              fields=c(LicenseKey="character",
                       SessionName="character",
                       NeticaHandle="externalptr",
                       Checking="character",
                       maxmem="numeric",
                       nets="environment"),
              methods=list(
                  initialize = function(...,
                                        SessionName=paste("RNetica Session",
                                                          date()),
                                        autostart=FALSE){
                    sess <- callSuper(...,SessionName=SessionName,
                                      NeticaHandle=externalptr(),
                                      nets=new.env(parent=emptyenv()))

                    ## MASTERSESSIONLIST[[SessionName]] <<- sess
                    if (autostart) startSession(sess)
                    sess
                  },
                  isActive = function() {
                    .Call("RN_isSessionActive",.self,PACKAGE=RNetica)
                  },
                  neticaVersion = function() {
                    .Call("RN_Session_Version",.self,PACKAGE=RNetica)
                  },
                  reportErrors = function(maxreport=9,clear=TRUE) {
                    counts <-
                      .Call("RN_Session_errors",.self,as.integer(maxreport),
                            as.logical(clear),PACKAGE=RNetica)
                    names(counts) <- c("Errors","Warnings","Notices","Reports")
                    invisible(counts)
                  },
                  clearErrors = function(severity="XXX_ERR") {
                    .Call("RN_ClearSessionErrors",.self,
                          as.character(severity),PACKAGE=RNetica)
                    invisible(.self)
                  },
                  listNets = function(pattern=".*") {
                    objects(envir=nets,pattern=pattern)
                  },
                  findNet = function(netname) {
                    nets[[netname]]
                  },
                  show = function() {
                    cat("Netica Session named ",SessionName,"\n")
                    if (isActive()) {
                       cat("  Session is currently active.\n")
                    } else {
                       cat("  Session is not currently active.\n")
                    }
                    netnames = listNets()
                    if (length(netnames) < 7) {
                      cat("  Open networks: ",netnames,".\n")
                    } else {
                      cat("  Open networks: ",netnames[1:6],"...\n")
                      cat("    and ",length(netnames)-6, "others.\n")
                    }
                  }
              ))

setGeneric("startSession",function (session) standardGeneric("startSession"))
setMethod("startSession","NeticaSession", function (session) {
  .Call("RN_start_Session",session,PACKAGE=RNetica)
  ecount <- session$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(session)
})
setGeneric("stopSession",function (session) standardGeneric("stopSession"))
setMethod("stopSession","NeticaSession", function (session) {
  netnames <- session$listNets()
  for (nn in netnames)
    session$findNet(nn)$deactivate()
  .Call("RN_stop_Session",session,PACKAGE=RNetica)
  ecount <- session$reportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
  invisible(session)
  })

setGeneric("restartSession",function (session)
  standardGeneric("restartSession"))
setMethod("restartSession","NeticaSession", function(session) {
  stopSession(session)
  startSession(session)
})

############################
## These were functions of the hidden Netica environment variable before,
## Now they are their own functions.

## This function returns the version number as a list with two named
## components, the first is the version number (expressed as an
## integer).  The second is the message string sent back from the
## version command.
NeticaVersion <- function (session=getDefaultSession()) {
  session$neticaVersion()
}


## This function reports on any errors, and if <clear> is TRUE clears
## them as well.  It returns a vector given the counts of errors of
## various types.  This is mostly used internally:  The R functions
## call Netica through .Call and the call ReportErrors to report on
## errors.
ReportErrors <- function(maxreport=9,clear=TRUE,session=getDefaultSession()) {
  session$reportErrors(maxreport,clear)
}
## * Clears all errors at a given severity (and lower?)
## * sev -- should be either NULL (all arguments) or a single character
## * string, one of "NOTHING_ERR", "REPORT_ERR", "NOTICE_ERR",
## * "WARNING_ERR", "ERROR_ERR", or "XXX_ERR"
ClearAllErrors <- function(severity="XXX_ERR",session=getDefaultSession()) {
  session$clearErrors(severity)
}






## Needs to text if C pointer is Nil, so needs object specific
## C code.
setGeneric("is.active",
           function(x) return(NA))
setMethod("is.active","NeticaSession",function(x) x$isActive())

## ## Searches active sessions looking for network by name.
## ## May return multiple results if different sessions have networks of
## ## the same name.
## findNetByName <- function (name) {
##   result <- list()
##   for (sessName in objects(MASTERSESSIONLIST)) {
##     net <- MASTERSESSIONLIST[[sessName]]$findNet(name)
##     if (!is.null(net)) result <- c(result,net)
##   }
##   result
## }




##########################################################
## DefaultSession
##########################################################
## For backwards compatability, we need a default session argument that
## we can add to any function.
## Assume that there is a variable called "DefaultNeticaSession" in the global
## environment

getDefaultSession <- function() {
  if (exists("DefaultNeticaSession",envir=.GlobalEnv))
    defSess <- get("DefaultNeticaSession",envir=.GlobalEnv)
  else {
    if (!interactive())
      stop("Could not find DefaultNeticaSession")
    cat("No ",dQuote("DefaultNeticaSession"), " variable exists in the global enviornment.\n")
    cat("Creating one will modify the global environment.\n")
    yn <- prompt("Should RNetica create one? (y/N)")
    if (!grepl("y",yn,ignore.case=TRUE)) {
      stop("Could not find DefaultNeticaSession")
    }
    if (exists("NeticaLicenseKey",envir=.GlobalEnv)) {
      key <- get("NeticaLicenseKey",envir=.GlobalEnv)
      defSess <- NeticaSession$new(LicenseKey=key)
    } else {
      defSess <- NeticaSession$new()
    }
    assign("DefaultNeticaSession",defSess,envir=.GlobalEnv)
  }
  if (!defSess$isActive())
    startSession(defSess)
  defSess
}


##These functions start are also supplied for backwards compatability.
StartNetica <- function(license=LicenseKey, checking=NULL,
                        maxmem=NULL,
                        session = NeticaSession(LicenseKey=license,
                                                checking=checking,
                                                maxmem=maxmem)) {
  if (!is.active(session))
    startSession(session)
  assign("DefaultNeticaSession",session,.GlobalEnv)
  session
}

StopNetica <- function(session=getDefaultSession()) {
  stopSession(session)
}


