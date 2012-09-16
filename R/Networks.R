## Networks.R
## Basic functions Relating to Networks.


## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL

## tests a string to see if it is a legal Netica name.
is.IDname <- function (x) {
  if (!is.character(x)) return(rep(FALSE,length(x)))
  grepl("^[[:alpha:]][[:alnum:]_]{,29}$",x) & (nchar(x)<31)
}

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
## various types.  This is mostly used internally:  The R functions
## call Netica through .Call and the call ReportErrors to report on
## errors. 
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
  goodNames <- is.IDname(names)
  if (any(!goodNames)) {
    stop("Illegal Netica Names, ",names[!goodNames])
  }
  handles <- .Call("RN_New_Net",names)
  if (length(handles)==1) handles <- handles[[1]]
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("CreateNetwork: Netica Errors Encountered, see console for details.")
  }
  handles
}


## Tests to see if the handle attached to a BN object is live or not.
## Returns NA if the object is not a network.
is.active <- function (bn) {
  if(!is(bn,"NeticaBN")) return(NA)
  return(.Call("RN_isBNActive",bn))
}

toString.NeticaBN <- function(x,...) {
  if (is.active(x))
    paste("<Netica BN:",as.character(x),">")
  else 
    paste("<Deleted Netica BN:",as.character(x),">")
}
  
print.NeticaBN <- function(x, ...) {
  cat(toString(x),"\n")
}

is.NeticaBN <- function (x) {
  is(x,"NeticaBN")
}

DeleteNetwork <- function (nets) {
  if (is(nets,"NeticaBN") && length(nets) ==1) {
    nets <- list(nets)
  }
  if (any(!sapply(nets,is,"NeticaBN"))) {
    stop("Expected a list of Netica networks, got, ",nets)
  }
  handles <- .Call("RN_Delete_Net",nets)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("DeleteNetwork: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}

## Returns a network by its position in the list.
GetNthNet <- function (n) {
  ## Netica uses 0 based indexing, but R convention is 1-based.
  ## So convert here.
  n <- as.integer(n-1)
  if (any(is.na(n))) {
    stop("GetNthNets: Expected vector of integers")
  }
  handles <- .Call("RN_GetNth_Nets",n)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNthNets: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  handles
}

## Returns a network by its name.
GetNamedNets <- function (namelist) {
  namelist <- as.character(namelist)
  handles <- .Call("RN_Named_Nets",namelist)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNamedNets: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  handles
}

CopyNets <- function (nets, newnamelist, options=character(0)) {
  if (is(nets,"NeticaBN") && length(nets) ==1) nets <-list(nets)
  if (!all(sapply(nets,is,"NeticaBN"))) {
    stop("Expected a list of Netica networks, got, ",nets)
  }
  if (length(nets)!=length(newnamelist)) {
    stop("Number of new names doesn't match number of old nets")
  }
  goodNames <- is.IDname(newnamelist)
  if (any(!goodNames)) {
    stop("Illegal Netica Names, ",newnamelist[!goodNames])
  }
  newnamelist <- as.character(newnamelist)
  options <- paste(options,collapse=",")
  handles <- .Call("RN_Copy_Nets",nets,newnamelist,options)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNamedNets: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  handles
}
