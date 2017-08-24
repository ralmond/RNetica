## Functions to call when the package is loaded.

## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL


## .onLoad <-
## function(libname, pkgname)
## {
##  library.dynam("RNetica", pkgname, libname)
##   ## Deep voodoo needed to get around R safeguards
##   ## Need to do it in Load before export
##   assignInMyNamespace("EVERY_STATE",.Call("RN_GetEveryState",PACKAGE="RNetica"))
## }

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
## es <- local({
##   esval <- -Inf
##   function(v) {
##     if (!missing(v)) {
##       error("Can't set the value of EVERY_STATE.")
##     }
##     if (!is.finite(esval))
##       esval <<- .Call("RN_GetEveryState",PACKAGE=RNetica)
##     esval
##   }})
## EVERY_STATE <-
##   delayedAssign("EVERY_STATE",.Call("RN_GetEveryState",PACKAGE=RNetica))
.onload <- function(libname, pkgname) {
  ## Now fetched from getDefaultLicense
   ## if(exists("NeticaLicenseKey",mode="character")) {
   ##   assignInMyNamespace("LicenseKey", NeticaLicenseKey)
   ##   #cat("License Key Installed.")
   ## }
  assignInMyNamespace("CCodeLoaded", FALSE)
}

.onAttach <- function(libname, pkgname) {
  .C("RN_Define_Symbols",PACKAGE=RNetica)
  assignInMyNamespace("CCodeLoaded", TRUE)
  assignInMyNamespace("EV_STATE",.Call("RN_GetEveryState",PACKAGE=RNetica))
##   ##StartNetica()
}

.onDetach <- function(libpath) {
   ##StopNetica()
}

.onUnload <- function(libpath) {
  library.dynam.unload("RNetica", libpath)
}
