## Functions to call when the package is loaded.

## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL


### I'm trying to set the value of EVERY_STATE to the value found in
### the Netica code.  But I have a problem.  If I call
### RN_GetEveryState to fetch the value when the namespace is loaded,
### then I have a problem because the dynamic library has not yet been
### loaded.  If I call when the namespace is attached, then the value
### has already been locked and setting the local value does not
### affect the global value.

### Work around.  I'll give EVERY_STATE the internal value, and let
### EV_STATE be the internal value.  I'll then switch them in the
### accessor function ([ and [[) in Edges.R

## Give it external value
EVERY_STATE <- -9999
## Give a prebinidng for internal value to get around locked namespace
## problem.
EV_STATE <- NULL


.onLoad <- function(libname, pkgname) {
  ## Need to explicitly load libnetica/Netica.dll before loading RNetica
  ## Also need the local=FALSE flag.
  ## RE: http://www.stat.ucdavis.edu/~duncan/R/dynload/
  ## if (file.exists(file.path(libname,pkgname,"Netica","libnetica.so")))
  ##   dyn.load(file.path(libname,pkgname,"Netica","libnetica.so"),local=FALSE)
  cat("Library Path: ",file.path(libname,pkgname,
                                 paste0("libs",Sys.getenv("R_ARCH"))), "\n")
  cat("Files: ",paste(list.files(file.path(libname,pkgname,
                                           paste0("libs",
                                                  Sys.getenv("R_ARCH")))),
                      collapse=", "),"\n")
  if (file.exists(file.path(libname,pkgname,"libs",
                            Sys.getenv("R_ARCH"),"Netica.dll")))
      dyn.load(file.path(libname,pkgname,"libs",
                         Sys.getenv("R_ARCH"),"Netica.dll"),
               local=FALSE)
  library.dynam("RNetica", pkgname, libname)
}

.onAttach <- function(libname, pkgname) {
  .C("RN_Define_Symbols",PACKAGE=RNetica)
  assignInMyNamespace("EV_STATE",.Call("RN_GetEveryState",PACKAGE=RNetica))
  assignInMyNamespace("CCodeLoaded",TRUE)
##   ##StartNetica()
}

.onDetach <- function(libpath) {
   ##StopNetica()
}

.onUnload <- function(libpath) {
  library.dynam.unload("RNetica", libpath)
}
