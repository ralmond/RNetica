## Networks.R
## Basic functions Relating to Networks.


## tests a string to see if it is a legal Netica name.
is.IDname <- function (x) {
  if (!is.character(x)) return(rep(FALSE,length(x)))
  result <- grepl("^[[:alpha:]][[:alnum:]_]{,29}$",x) & (nchar(x)<31)
  ifelse(is.na(result),FALSE,result)
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
  handles <- .Call("RN_New_Nets",names)
  if (length(handles)==1) handles <- handles[[1]]
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("CreateNetwork: Netica Errors Encountered, see console for details.")
  }
  handles
}


## Tests to see if the handle attached to a BN object is live or not.
## Returns NA if the object is not a network.
is.active <- function (x) {
  if(is.NeticaBN(x)) 
     return(.Call("RN_isBNActive",x))
  if (is.NeticaNode(x))
     return(.Call("RN_isNodeActive",x))
  if (is.list(x)) {
    return(sapply(x,is.active))
  }
  return(NA)
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

Ops.NeticaBN <- function(e1, e2) {
  ok <- switch(.Generic, "=="=0, "!=" =1, -1)
  if (ok<0) {
    warning(.Generic, " not implemented for Netica networks.")
    return(NA)
  }
  truth <- (ok == 0)  ## inversts sign of truth for !=
  if (is.list(e2)) { ##Comparing scalar to list
    if (all(sapply(e2,is.NeticaBN))) {
      return (sapply(e2,function(ee) e1==ee))
    } else {
      return (!truth)
    }
  }
  bothdeleted <- !is.active(e1) && !is.active(e2)
  if (is.na(bothdeleted)) return(!truth) ## At least one non-bn
  if (bothdeleted) {
    ## Both deleted, use cached names.
    return(ifelse(as.character(e1)==as.character(e2),truth,!truth))
  }
  ## Okay have two valid NeticaBNs or one valid one and one inactive.
  ## Either way we can get by by comparing pointers.
  return(ifelse(identical(attr(e1,"Netica_bn"),attr(e2,"Netica_bn")),
                truth,!truth))
}

  
DeleteNetwork <- function (nets) {
  if (is.NeticaBN(nets) && length(nets) ==1) {
    nets <- list(nets)
  }
  if (any(!sapply(nets,is.NeticaBN))) {
    stop("Expected a list of Netica networks, got, ",nets)
  }
  handles <- .Call("RN_Delete_Nets",nets)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("DeleteNetwork: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}

## Returns a network by its position in the list.
GetNthNetwork <- function (n) {
  ## Netica uses 0 based indexing, but R convention is 1-based.
  ## So convert here.
  n <- as.integer(n-1)
  if (any(is.na(n))) {
    stop("GetNthNets: Expected vector of integers")
  }
  handles <- .Call("RN_GetNth_Nets",n)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNthNetworks: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  handles
}

## Returns a network by its name.
GetNamedNetworks <- function (namelist) {
  namelist <- as.character(namelist)
  handles <- .Call("RN_Named_Nets",namelist)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNamedNetworks: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  handles
}

CopyNetworks <- function (nets, newnamelist, options=character(0)) {
  if (is.NeticaBN(nets)) nets <-list(nets)
  if (!all(sapply(nets,is.NeticaBN))) {
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
    stop("CopyNetworks: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  handles
}

########################################################################
## Network File IO
########################################################################
WriteNetworks <- function (nets, paths) {
  if (is.NeticaBN(nets) && length(nets) ==1) {
    nets <- list(nets)
  }
  if (any(!sapply(nets,is.NeticaBN))) {
    stop("Expected a list of Netica networks, got, ",nets)
  }
  paths <- as.character(paths)
  if (any(is.na(paths))) {
    stop("Expected a list of pathnames, got, ",paths)
  }
  if (length(nets) != length(paths)) {
    stop("Lengths of net and pathname lists are different")
  }
  handles <- .Call("RN_Write_Nets",nets,paths)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("WriteNetwork: Netica Errors Encountered, see console for details.")
  }
  ## Save filenames for later recovery of network.
  for (i in 1:length(handles)) {
    if (!is.null(handles[[i]]))
      attr(handles[[i]],"Filename") <- paths[i]
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}


ReadNetworks <- function (paths) {
  ##If they pass a network object, try to extract a path attribute.
  if (is.NeticaBN(paths) && !is.null(attr(paths,"Filename"))) {
    return(ReadNetworks(attr(paths,"Filename")))
  }
  if (is.list(paths) && length(paths) >0 && is.NeticaBN(paths[[1]])) {
    return(lapply(paths,ReadNetworks))
  }
  paths <- as.character(paths)
  if (any(is.na(paths))) {
    stop("Expected a list of pathnames, got, ",paths)
  }
  handles <- .Call("RN_Read_Nets",paths)
  ecount <- ReportErrors()
  ## Save filenames for later recovery of network.
  for (i in 1:length(handles)) {
    if (!is.null(handles[[i]]))
      attr(handles[[i]],"Filename") <- paths[i]
  }
  if (ecount[1]>0) {
    stop("WriteNetwork: Netica Errors Encountered, see console for details.")
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}

GetNetworkFileName <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  pathname <- .Call("RN_GetNetFilename",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNetworkFileName: Netica Errors Encountered, see console for details.")
  }
  pathname
}

################################################################
## Getters and Setters for High Level Net properities
################################################################

NetworkName <- function (net) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  name <- .Call("RN_GetNetName",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetworkName: Netica Errors Encountered, see console for details.")
  }
  name
}

"NetworkName<-" <- function (net, value) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  if (length(value)>1 || !is.IDname(value)) {
    stop("Illegal Netica Name, ",value)
  }
  handle <- .Call("RN_SetNetName",net,value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("SetNetworkName: Netica Errors Encountered, see console for details.")
  }
  handle
}

NetworkTitle <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  title <- .Call("RN_GetNetTitle",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetworkTitle: Netica Errors Encountered, see console for details.")
  }
  title
}

"NetworkTitle<-" <- function (net, value) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  if (length(value)>1) {
    warning("Only first element used as title.")
  }
  value <- as.character(value)

  handle <- .Call("RN_SetNetTitle",net,value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("SetNetworkTitle: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

NetworkComment <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  comment <- .Call("RN_GetNetComment",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetworkComment: Netica Errors Encountered, see console for details.")
  }
  comment
}

"NetworkComment<-" <- function (net, value) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  value <- as.character(value)
  if (any(is.na(value))) {
    stop("Non-character titles in ", value)
  }
  value <- paste(value,collapse="\n")
  handle <- .Call("RN_SetNetComment",net,value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("SetNetworkComment: Netica Errors Encountered, see console for details.")
  }
  invisible(handle)
}

GetNetworkAutoUpdate <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  autoupdate <- .Call("RN_GetNetAutoUpdate",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("GetNetAutoUpdate: Netica Errors Encountered, see console for details.")
  }
  autoupdate
}

SetNetworkAutoUpdate <- function (net, newautoupdate) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  if (length(newautoupdate) >1) {
    warning("Additional newautoupdate values ignored.")
  }
  newautoupdate <- as.logical(newautoupdate[1])
  oldautoupdate <- .Call("RN_SetNetAutoUpdate",net,newautoupdate)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("SetNetworkAutoupdate: Netica Errors Encountered, see console for details.")
  }
  oldautoupdate
}

WithoutAutoUpdate <- function (net,expr) {
  oldautoupdate <- SetNetworkAutoUpdate(net,FALSE)
  tryCatch(expr,
           finally = SetNetworkAutoUpdate(net,oldautoupdate))
}


NetworkUserField <- function (net, fieldname) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  if (length(fieldname)>1 || !is.IDname(fieldname)) {
    stop("Illegal Netica Field Name, ",fieldname)
  }
  value <- .Call("RN_GetNetUserField",net,fieldname)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetUserField: Netica Errors Encountered, see console for details.")
  }
  value
}

"NetworkUserField<-" <- function (net, fieldname, value) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  if (length(fieldname)>1 || !is.IDname(fieldname)) {
    stop("Illegal Netica Field Name, ",fieldname)
  }
  value <- as.character(value)
  if (length(value)>1 || is.na(value)) {
    stop("Illegal field value.")
  }
  handle <- .Call("RN_SetNetUserField",net,fieldname,value)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("SetNetUserField: Netica Errors Encountered, see console for details.")
  }
  handle
}

NetworkAllUserFields <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  values <- .Call("RN_GetAllNetUserFields",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetUserField: Netica Errors Encountered, see console for details.")
  }
  values
}

NetworkUndo <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  flag <- .Call("RN_Undo",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetworkUndo: Netica Errors Encountered, see console for details.")
  }
  if (flag <0) {
    warning("Empty Undo Stack.")
  }
  invisible(flag)
}

NetworkRedo <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  flag <- .Call("RN_Redo",net)
  ecount <- ReportErrors()
  if (ecount[1]>0) {
    stop("NetworkRedo: Netica Errors Encountered, see console for details.")
  }
  if (flag <0) {
    warning("Empty Redo Stack.")
  }
  invisible(flag)
}
