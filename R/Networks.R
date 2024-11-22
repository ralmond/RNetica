## Networks.R
## Basic functions Relating to Networks.


## tests a string to see if it is a legal Netica name.
is.IDname <- function (x) {
  if (!is.character(x)) return(rep(FALSE,length(x)))
  result <- grepl("^[[:alpha:]][[:alnum:]_]{,29}$",x) & (nchar(x)<31)
  ifelse(is.na(result),FALSE,result)
}


as.IDname <- function (x, prefix="y", maxlen=25) {
  if (maxlen >30) stop("Netica limits names to 30 characters.")
  y <- as.character(x)
  alphastart <- grepl("^[[:alpha:]].*",y)
  if (any(!alphastart)) {
    y[!alphastart] <- paste(prefix,y[!alphastart],sep="")
  }
  y <- gsub("[^[:alnum:]_]","_",y)
  toolong <- which(nchar(y) > maxlen)
  for (i in toolong) {
    y[i] <- paste(substr(y[i],1,maxlen-3),
               sum(utf8ToInt(substr(y[i],maxlen-3,1000))) %% 100,
               sep="_")
  }
  y
}




NeticaBN <-
  setRefClass("NeticaBN",
              fields=c(Name="character",
                       PathnameName="character",
                       Netica_bn="externalptr",
                       Session="NeticaSession",
                       nodes="environment"),
              methods=list(
                  initialize = function(Name=".Prototype",
                                        Session=NeticaSession(SessionName=".Prototype"),...){
                    net <- callSuper(Name=Name,Session=Session,
                                     nodes=new.env(parent=emptyenv()),
                                     Netica_bn=externalptr(),
                                     ...)
                    net
                  },
                  isActive = function() {
                    .Call("RN_isBNActive",.self,PACKAGE=RNetica)
                  },
                  reportErrors = function(maxreport=9,clear=TRUE,
                                          call = sys.call(sys.parent())) {
                    Session$reportErrors(maxreport,clear, call)
                  },
                  signalErrors = function(maxreport=9,clear=TRUE,
                                          call = sys.call(sys.parent())) {
                    e <- reportErrors(maxreport, clear, call)
                    if (inherits(e,"error")) stop(e)
                    if (inherits(e,"warning")) warn(e)
                    if (inherits(e,"condition")) signalCondition(e)
                  },
                  clearErrors = function(severity="XXX_ERR") {
                    Session$clearErrors(severity)
                  },
                  listNodes = function() {
                    objects(envir=nodes)
                  },
                  findNode = function(nodename) {
                    nodes[[nodename]]
                  },
                  searchNodes = function(pattern) {
                    objects(pattern=pattern,envir=nodes)
                  },
                  deactivateNodes = function() {
                    nodenames <- listNodes()
                    for (nn in nodenames) {
                      nd <- findNode(nn)
                      if (is(nd,"NeticaNode")) {
                        nd$deactivate()
                      }
                    }
                  },
                  deactivate = function() {
                    deactivateNodes()
                    .Call("RN_DeactivateBN",.self,PACKAGE=RNetica)
                  },
                  show = function() {
                    cat("Netica Network named ",Name,"\n")
                    if (isActive()) {
                       cat("  Network is currently active.\n")
                     } else {
                       cat("  Network is not currently active.\n")
                    }
                    nodenames = listNodes()
                    if (length(nodenames) < 7) {
                      cat("  Nodes : ",nodenames,".\n")
                    } else {
                      cat("  Nodes: ",nodenames[1:6],"...\n")
                      cat("    and ",length(nodenames)-6, "others.\n")
                    }
                  }
              ))


## This function creates the Bayesian Network objects.
## *Vectorized*
CreateNetwork <- function (names,session=getDefaultSession()) {
  if (!is.character(names) || length(names) == 0) {
    stop("Network names not supplied.")
  }
  goodNames <- is.IDname(names)
  if (any(!goodNames)) {
    stop("Illegal Netica Names, ",names[!goodNames])
  }
  existing <- sapply(names, function (name) {
    if (!is.null(session$findNet(name)) && is.active(session$findNet(name)))
      TRUE
    else
      FALSE
  })
  if (any(existing)) {
    stop("Network(s): ",names[existing]," are already active Netica networks.")
  }
  handles <- .Call("RN_New_Nets",names,session,PACKAGE=RNetica)
  if (length(handles)==1) handles <- handles[[1]]
  session$signalErrors()
  handles
}


## Redo as generic
## Tests to see if the handle attached to a BN object is live or not.
## Returns NA if the object is not a network.
setMethod("is.active","NeticaBN",function(x) x$isActive())


setMethod("toString","NeticaBN",function(x,...) {
  if (is.active(x))
    paste("<Netica BN:",x$Name,">")
  else
    paste("<Deleted Netica BN:",x$Name,">")
})

setMethod("print","NeticaBN", function(x, ...) {
  cat(toString(x),"\n")
})

setMethod("as.character", "NeticaBN", function(x, ...) {
  toString(x)
})

## Need this function so RStudio won't choke on Node objects.
str.NeticaBN <- function (object, ...) {
    object$show()
    invisible(object)
}


is.NeticaBN <- function (x) {
  is(x,"NeticaBN")
}

setMethod("Compare",c("NeticaBN","NeticaBN"), function(e1, e2) {
  ok <- switch(.Generic, "=="=0, "!=" =1, -1)
  if (ok<0) {
    warning(.Generic, " not implemented for Netica networks.")
    return(NA)
  }
  truth <- (ok == 0)  ## inversts sign of truth for !=

  bothdeleted <- !is.active(e1) && !is.active(e2)
  if (is.na(bothdeleted)) return(!truth) ## At least one non-bn
  if (bothdeleted) {
    ## Both deleted, use cached names.
    return(ifelse(
        e1$Name==e2$Name && all(e1$PathnameName == e2$PathnameName),
        truth,!truth))
  }
  ## Okay have two valid NeticaBNs or one valid one and one inactive.
  ## Either way we can get by by comparing pointers.
  return(ifelse(identical(e1$Netica_bn,e2$Netica_bn),
                truth,!truth))
})

setMethod("is.element",c("NeticaBN","list"),
          function (el,set) is.element(list(el),set))



DeleteNetwork <- function (nets) {
  if (is.NeticaBN(nets) && length(nets) ==1) {
    nets <- list(nets)
  }
  session <- nets[[1]]$Session
  if (any(!sapply(nets,is.NeticaBN))) {
    stop("Expected a list of Netica networks, got, ",nets)
  }
  for (net in nets)
    net$deactivateNodes()
  handles <- .Call("RN_Delete_Nets",nets,session,PACKAGE=RNetica)
  session$signalErrors()
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}

## Returns a network by its position in the list.
GetNthNetwork <- function (n,session=getDefaultSession()) {
  ## Netica uses 0 based indexing, but R convention is 1-based.
  ## So convert here.
  n <- as.integer(n-1)
  if (any(is.na(n))) {
    stop("Expected vector of integers")
  }
  if (!is(session,"NeticaSession") && is.active(session)) {
    stop("Expected an active Netica Session got ", session)
  }

  handles <- .Call("RN_GetNth_Nets",n,session,PACKAGE=RNetica)
  session$signalErrors()
  if (length(handles)==1) handles <- handles[[1]]
  handles
}

GetNamedNetworks <- function (namelist, session=getDefaultSession()) {
  result <- lapply(namelist,function(name) session$nets[[name]])
  if (length(result)==1L) result <- result[[1]]
  result
}


## Returns a network by its name.
CheckNamedNetworks <- function (namelist, session=getDefaultSession()) {
  namelist <- as.character(namelist)
  handles <- .Call("RN_Named_Nets",namelist,session,PACKAGE=RNetica)
  session$signalErrors()
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
  session <- nets[[1]]$Session

  existing <- sapply(newnamelist, function (name) {
    if (!is.null(session$findNet(name)) && is.active(session$findNet(name)))
      TRUE
    else
      FALSE
  })
  if (any(existing)) {
    stop("Network(s): ",newnamelist[existing]," are already active Netica networks.")
  }

  handles <- .Call("RN_Copy_Nets",nets,newnamelist,options,session,
                   PACKAGE=RNetica)
  session$signalErrors()
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
  if (missing(paths)) {
    paths <- sapply(nets,GetNetworkFileName)
    if (any(nchar(paths)==0)) {
      stop("File names missing for net with no associated file name.")
    }
  }
  paths <- as.character(paths)
  if (any(is.na(paths))) {
    stop("Expected a list of pathnames, got, ",paths)
  }
  if (length(nets) != length(paths)) {
    stop("Lengths of net and pathname lists are different")
  }
  session <- nets[[1]]$Session
  handles <- .Call("RN_Write_Nets",nets,paths,session,PACKAGE=RNetica)
  session$signalErrors()
  ## Save filenames for later recovery of network.
  for (i in 1:length(handles)) {
    handles[[i]]$PathnameName <- paths[i]
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}


ReadNetworks <- function (paths,session=getDefaultSession(),loadVisual=TRUE) {
  ##If they pass a network object, try to extract a path attribute.
  if (is.NeticaBN(paths)) {
    if (length(paths$PathnameName) == 0) {
      stop("No filename available for ",paths)
    }
    if (missing(session)) {
      session <- paths$Session
    }
    return(ReadNetworks(paths$PathnameName,session,loadVisual))
  }
  if (is.list(paths) && length(paths) >0 && is.NeticaBN(paths[[1]])) {
    return(lapply(paths,function(path) ReadNetworks(path,session,loadVisual)))
  }
  paths <- as.character(paths)
  if (any(is.na(paths))) {
    stop("Expected a list of pathnames, got, ",paths)
  }
  handles <- .Call("RN_Read_Nets",paths,session,loadVisual,PACKAGE=RNetica)
  session$signalErrors()
  ## Re-register networks under new name.
  ## Save filenames for later recovery of network.
  for (i in 1:length(handles)) {
    handles[[i]]$PathnameName <- paths[i]
  }
  if (length(handles)==1) handles <- handles[[1]]
  invisible(handles)
}


## Internal switches back and forth between using the cached value in
## the object and the internal Netica value.
GetNetworkFileName <- function (net,internal=FALSE) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  if (internal) {
    pathname <- .Call("RN_GetNetFilename",net,PACKAGE=RNetica)
    net$signalErrors() 
    pathname
  } else
    net$PathnameName
}

################################################################
## Getters and Setters for High Level Net properities
################################################################

NetworkSession <- function (net) {
  net$Session
}


NetworkName <- function (net, internal=FALSE) {
  if (!is.NeticaBN(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  if (internal) {
    if (!is.active(net)) {
      stop("Network ",net,"is not currently active")
      }
    name <- .Call("RN_GetNetName",net,PACKAGE=RNetica)
    net$signalErrors()
    name
  } else {
    net$Name
  }
}

"NetworkName<-" <- function (net, value) {
  if (!is.NeticaBN(net) || !is.active(net)) {
    stop("Expected an active Netica network, got, ",net)
  }
  oldname <- NetworkName(net)
  session <- net$Session
  if (length(value)>1 || !is.IDname(value)) {
    stop("Illegal Netica Name, ",value)
  }
  if (oldname==value) {
    mes <- simpleMessage(paste("Network is already named ",oldname,"skipping"),
                         call = sys.call())
    signalCondition(mes)
    return (net)
  }
  handle <- .Call("RN_SetNetName",net,value,session,PACKAGE=RNetica)
  session$signalErrors()
  handle$Name <- value
  rm(list=oldname,envir=session$nets)
  session$nets[[value]] <- handle
  handle
}

NetworkTitle <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  title <- .Call("RN_GetNetTitle",net,PACKAGE=RNetica)
  net$signalErrors()
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

  handle <- .Call("RN_SetNetTitle",net,value,PACKAGE=RNetica)
  net$signalErrors()
  invisible(handle)
}

NetworkComment <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  comment <- .Call("RN_GetNetComment",net,PACKAGE=RNetica)
  net$signalErrors()
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
  handle <- .Call("RN_SetNetComment",net,value,PACKAGE=RNetica)
  net$signalErrors()
  invisible(handle)
}


GetNetworkAutoUpdate <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  autoupdate <- .Call("RN_GetNetAutoUpdate",net,PACKAGE=RNetica)
  net$signalErrors()
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
  oldautoupdate <- .Call("RN_SetNetAutoUpdate",net,newautoupdate,PACKAGE=RNetica)
  net$signalErrors()
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
  value <- .Call("RN_GetNetUserField",net,fieldname,PACKAGE=RNetica)
  net$signalErrors()
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
  handle <- .Call("RN_SetNetUserField",net,fieldname,value,PACKAGE=RNetica)
  net$signalErrors()
  handle
}

NetworkAllUserFields <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  values <- .Call("RN_GetAllNetUserFields",net,PACKAGE=RNetica)
  net$signalErrors()
  values
}

## A utility function for converting objects to strings and vise
## versa.
dputToString <- function (obj) {
  con <- textConnection(NULL,open="w")
  tryCatch({dput(obj,con);
           textConnectionValue(con)},
           finally=close(con))
}

dgetFromString <- function (str) {
  con <- textConnection(str,open="r")
  tryCatch(dget(con), finally=close(con))
}


NetworkUserObj <- function (net, fieldname) {
  str <- NetworkUserField(net,fieldname)
  if (is.na(str)) return(NULL)
  dgetFromString(str)
}

"NetworkUserObj<-" <- function (net, fieldname, value) {
  sval <- dputToString(value)
  ## Sometimes R "helpfully" breaks this into multiple lines.
  if (length(sval) > 1)
    sval <- paste(sval,collapse=" ")
  NetworkUserField(net,fieldname) <- sval
  net
}


NetworkUndo <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  flag <- .Call("RN_Undo",net,PACKAGE=RNetica)
  net$signalErrors()
  if (flag <0) {
    warning("Empty Undo Stack.")
  }
  invisible(flag)
}

NetworkRedo <- function (net) {
  if (!is.NeticaBN(net)) {
    stop("Expected a Netica network, got, ",net)
  }
  flag <- .Call("RN_Redo",net,PACKAGE=RNetica)
  net$signalErrors()
  if (flag <0) {
    warning("Empty Redo Stack.")
  }
  invisible(flag)
}


### "Local" network creation tools.  These use withr::defer to ensure
### that the network is deleted when the environment (by default the
### calling function) is exited.

local_create_nets <- function(names, session=getDefaultSession(),
                              env=parent.frame()) {
  nets <- CreateNetwork(names,session)
  withr::defer(DeleteNetwork(nets),env)
  nets
}

local_load_nets <- function(netpaths, session=getDefaultSession(),
                            env=parent.frame()) {
  nets <- ReadNetworks(netpaths,session)
  withr::defer(DeleteNetwork(nets),env)
  nets
}
local_copy_nets <- function(nets, newnames, options=character(0), env=parent.frame()) {
  newnets <- CopyNetworks(nets,newnames,options)
  withr::defer(DeleteNetwork(newnets),env)
  newnets
}

## These are just wrappers to make grabbing examples from the packages easier.

local_RNetica_net <- function(filename, session=getDefaultSession(),
                              env=parent.frame())
  local_load_nets(system.file(file.path("sampleNets",filename),
                              package="RNetica",
                              mustWork=TRUE),session,env)
