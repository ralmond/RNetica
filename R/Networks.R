## Networks.R
## Basic functions Relating to Networks.


## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL


##These functions start and stop the Netica API Environment.
StartNetica <- function(license=LicenseKey, checking=NULL,
                        maxmem=NULL) {
  .C("RN_start_Netica",as.character(LicenseKey),
     as.character(checking),as.double(maxmem))
}
StopNetica <- function() {
  .C("RN_stop_Netica")
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
    stop("Netica Errors Encountered, see console for details.")
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
  if (!all(sapply(nets,class)=="NeticaBN"))
    stop("Expected List of NeticaBN objects")
  }
  .Call("RN_Delete_Net",names)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("Netica Errors Encountered, see console for details.")
  }
}

toString.DeletedNeticaBN <- function(x,...) {
  paste("<Deleted Netica BN:",as.character(x),">")
}
  
print.DeletedNeticaBN <- function(x, ...) {
  cat(toString(x),"\n")
}
