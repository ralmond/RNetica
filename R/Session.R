
### Package name string for .Call (this allows me to temporarily change the
### Name of the package while getting forked.
RNetica <- "RNetica"


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
  CCodeLoader()
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
                                        LicenseKey=options("NeticaLicenseKey")[[1]],
                                        SessionName=paste("RNetica Session",
                                                          date()),
                                        autostart=FALSE){
                    sess <- callSuper(...,LicenseKey=ifelse(is.null(LicenseKey),
                                                            "",LicenseKey),
                                      SessionName=SessionName,
                                      NeticaHandle=externalptr(),
                                      nets=new.env(parent=emptyenv()))

                    ## MASTERSESSIONLIST[[SessionName]] <<- sess
                    if (autostart) startSession(sess)
                    sess
                  },
                  isActive = function() {
                    CCodeLoader()
                    .Call("RN_isSessionActive",.self,PACKAGE=RNetica)
                  },
                  neticaVersion = function() {
                    .Call("RN_Session_Version",.self,PACKAGE=RNetica)
                  },
                  reportErrors = function(maxreport=9,clear=TRUE,
                                          call = sys.call(sys.parent())) {
                    if (!isActive())
                      stop("Session is inactive.")
                    allErrs <-
                      .Call("RN_Session_errors",.self,as.integer(maxreport),
                            as.logical(clear),PACKAGE=RNetica)
                    NeticaCondition(allErrs,call)
                  },
                  signalErrors = function(maxreport=9,clear=TRUE,
                                          call = sys.call(sys.parent())) {
                    e <- reportErrors(maxreport, clear, call)
                    if (inherits(e,"error")) stop(e)
                    if (inherits(e,"warning")) warn(e)
                    if (inherits(e,"condition")) signalCondition(e)
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

str.NeticaSession <- function(object,...) {
    object$show()
    invisible(object)
}

## This is added to suppress a warning about adding arguments in the
## documentation.
NeticaSession <-
  function(..., LicenseKey = options("NeticaLicenseKey")[[1]],
           SessionName = paste("RNetica Session", date()),
           Checking = character(), maxmem = integer()) {
    getRefClass("NeticaSession")$new(...,LicenseKey=LicenseKey,
                                     SessionName=SessionName,
                                     Checking=Checking,
                                     maxmem=maxmem)
}
    


setGeneric("startSession",function (session) standardGeneric("startSession"))
setMethod("startSession","NeticaSession", function (session) {
  CCodeLoader()
  .Call("RN_start_Session",session,PACKAGE=RNetica)
  session$signalErrors()
  invisible(session)
})
setGeneric("stopSession",function (session) standardGeneric("stopSession"))
setMethod("stopSession","NeticaSession", function (session) {
  netnames <- session$listNets()
  for (nn in netnames)
    session$findNet(nn)$deactivate()
  session$signalErrors()
  .Call("RN_stop_Session",session,PACKAGE=RNetica)
  invisible(session)
  })

setGeneric("restartSession",function (session)
  standardGeneric("restartSession"))
setMethod("restartSession","NeticaSession", function(session) {
  stopSession(session)
  startSession(session)
})

##################################################
## Error Handling Function

NeticaCondition <- function(allErrs,call) {
  names(allErrs) <- c("Fatal","Error","Warning","Notice","Report")
  counts <- sapply(allErrs,length)
  msg <- character(0)
  if (!any(counts>0)) return (NULL) # No errors
  names(counts) <- c("Fatal","Error","Warning","Notice","Report")
  if (counts["Fatal"]>0) {
    class <- c("fatal", "error")
    msg <- c(msg, paste("Netica fatal error: ",allErrs$Fatal))
  }
  else if (counts["Error"]>0) {
    class <- c("error")
    msg <- c(msg, paste("Netica error: ",allErrs$Error))
  }
  else if (counts["Warning"]>0) {
    class <- c("warning")
    msg <- c(msg, paste("Netica warning: ",allErrs$Warning))
  }
  else {
    class <- c("message",class)
    msg <- c(msg, paste("Netica notice: ",allErrs$Notice))
    msg <- c(msg, paste("Netica report: ",allErrs$Report))
  }
    
  message <- paste(msg,collapse="\n")
  cond <- c(message=message, call=call, counts=counts, allErrs)
  class(cond) <- c("NeticaCondition",class,"condition")
  cond
}

flogErrors <- function (allErrs) {
  if (is(allErrs,"fatal"))
    futile.logger::flog.fatal(conditionMessage(allErrs))
  else if (is(allErrs,"error"))
    futile.logger::flog.error(conditionMessage(allErrs))
  if (is(allErrs,"warning"))
    futile.logger::flog.warn(conditionMessage(allErrs))
  if (is(allErrs,"message"))
    futile.logger::flog.info(conditionMessage(allErrs))
  if(length(allErrs$Fatal) > 0L){
    lapply(allErrs$Fatal,futile.logger::flog.fatal)
  }
  if(length(allErrs$Error) > 0L){
    lapply(allErrs$Warning,futile.logger::flog.error)
  }
  if(length(allErrs$Warning) >0L){
    lapply(allErrs$Warning,futile.logger::flog.warn)
  }
  if(length(allErrs$Notice) > 0L){
    lapply(allErrs$Notice,futile.logger::flog.info)
  }
  if(length(allErrs$Report) > 0L){
    lapply(allErrs$Report,futile.logger::flog.debug)
  }
}

##' Now assume that the argument will be a Netica Condition.
##' @Depricated
logErrors <- function (allErrs) {
  flogErrors(allErrs)
  counts <- c("Errors"=NA_integer_,"Warnings"=NA_integer_,
              "Notices"=NA_integer_,"Reports"=NA_integer_)
  counts["Errors"] <- length(allErrs$Error)
  counts["Warnings"] <- length(allErrs$Warning)
  counts["Notices"] <- length(allErrs$Notice)
  counts["Reports"] <- length(allErrs$Report)
  invisible(counts)
}

print.NeticaCondition <- function (x,...) {
  print.condition(x,...)

  lapply(x$Fatal,
         function (e) print(paste("Fatal Netica Error:",e)))
  lapply(x$Error,
         function (e) print(paste("Netica Error:",e)))
  lapply(x$Warning,
         function (e) print(paste("Netica Warning:",e)))
  lapply(x$Notice,
         function (e) print(paste("Netica Note:",e)))
  lapply(x$Report,
         function (e) print(paste("Netica Report:",e)))
  invisible(x)
}

##' @depricated
printErrors <- function (allErrs)
  print(allErrs)


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
  err <-   session$reportErrors(maxreport,clear)
  if (!is.null(err))
    print(err)
  invisible(err)
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
setMethod("is.active","list",function(x) sapply(x, is.active))

setMethod("toString","NeticaSession",function(x,...) {
  if (is.active(x))
    paste("<Netica Session:",x$Name,">")
  else
    paste("<Inactive Netica Session:",x$Name,">")
})

setMethod("print","NeticaSession", function(x, ...) {
  cat(toString(x),"\n")
})

setMethod("as.character", "NeticaSession", function(x, ...) {
  toString(x)
})

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
    yn <- readline("Should RNetica create one? (y/N)")
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
StartNetica <- function(license=options("NeticaLicenseKey")[[1]],
                        checking=NULL,
                        maxmem=NULL,
                        session = NeticaSession(LicenseKey=license,
                                                Checking=checking,
                                                maxmem=maxmem)) {
  if (!is.active(session))
    startSession(session)
  assign("DefaultNeticaSession",session,.GlobalEnv)
  session
}

StopNetica <- function(session=getDefaultSession()) {
  stopSession(session)
}


