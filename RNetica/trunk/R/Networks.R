## Networks.R
## Basic functions Relating to Networks.


## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL


##These functions start and stop the Netica API Environment.
StartNetica <- function(license=LicenseKey, checking=NULL,
                        maxmem=NULL) {
  invisible(.C("RN_start_Netica",as.character(LicenseKey),
     as.character(checking),as.double(maxmem)))
}
StopNetica <- function() {
  invisible(.C("RN_stop_Netica"))
}

## This function returns the version number as a list with two named
## components, the first is the version number (expressed as an
## integer).  The second is the message string sent back from the
## version command.
NeticaVersion <- function () {
  .Call("RN_Netica_Version")
}


## This function reports on any errors, and if <clear> is TRUE clears
## them as well.  It returns a vector given the counts of errors of
## various types.
ReportErrors <- function(maxreport=999999,clear=TRUE) {
  counts <- .C("RN_report_errors",as.integer(maxreport),
                as.integer(clear),counts=integer(4))$counts
  names(counts) <- c("Errors","Warnings","Notices","Reports")
  invisible(counts)
}
## * Clears all errors at a given severity (and lower?)
## * sev -- should be either NULL (all arguments) or a single character
## * string, one of "NOTHING_ERR", "REPORT_ERR", "NOTICE_ERR", 
## * "WARNING_ERR", "ERROR_ERR", or "XXX_ERR"
ClearAllErrors <- function(severity="XXX_ERR") {
  .C("RN_ClearAllErrors",as.character(severity))
}


## This function creates the Bayesian Network objects.
## *Vectorized*
CreateNetwork <- function (names) {
  if (!is.character(names) || length(names) == 0) {
    stop("Network names not supplied.")
  }
  handles <- .Call("RN_New_Net",names)
  if (length(handles)==1) handles <- handles[[1]]
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("CreateNetwork: Netica Errors Encountered, see console for details.")
  }
  handles
}

toString.NeticaBN <- function(x,...) {
  paste("<Netica BN:",as.character(x),">")
}
  
print.NeticaBN <- function(x, ...) {
  cat(toString(x),"\n")
}
  
DeleteNetwork <- function (nets) {
  if (!isList(nets)) nets <- list(nets)
  if (!all(sapply(nets,class)=="NeticaBN")) {
    stop("Expected List of NeticaBN objects")
  }
  .Call("RN_Delete_Net",names)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("DeleteNetwork: Netica Errors Encountered, see console for details.")
  }
}

toString.DeletedNeticaBN <- function(x,...) {
  paste("<Deleted Netica BN:",as.character(x),">")
}
  
print.DeletedNeticaBN <- function(x, ...) {
  cat(toString(x),"\n")
}

## Returns a network by its position in the list.
GetNthNet <- function (n) {
  n <- as.integer(n)
  if (any(is.na(n))) {
    stop("GetNthNets: Expected vector of integers")
  }
  handles <- .Call("RN_GetNth_Nets",n)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNthNets: Netica Errors Encountered, see console for details.")
  }
  handles
}

## Returns a network by its name.
GetNamedNets <- function (namelist) {
  namelist <- as.character(namelist)
  handles <- .Call("RN_Named_Nets",n)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNamedNets: Netica Errors Encountered, see console for details.")
  }
  handles
}

CopyNets <- function (nets, newnamelist, options) {
  if (!isList(nets)) nets <- list(nets)
  if (!all(sapply(nets,class)=="NeticaBN")) {
    stop("Expected List of NeticaBN objects")
  }
  if (length(nets)!=length(newnamelist)) {
    stop("Number of new names doesn't match number of old nets")
  }
  options <- paste(options,collapse=",")
  handles <- .Call("RN_Copy_Nets",nets,newnamelist,options)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNamedNets: Netica Errors Encountered, see console for details.")
  }
}
