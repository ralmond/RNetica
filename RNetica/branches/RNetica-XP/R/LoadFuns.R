## Functions to call when the package is loaded.

## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL


## .onLoad <-
## function(libname, pkgname)
## {
##  library.dynam("RNeticaXR", pkgname, libname)
##   ## Deep voodoo needed to get around R safeguards
##   ## Need to do it in Load before export
##   assignInMyNamespace("EVERY_STATE",.Call("RN_GetEveryState",PACKAGE="RNeticaXR"))
## }

## Give it starter value so it can be found
EVERY_STATE <- -Inf
EVERY_STATE <- function() {
  EVERY_STATE
}

.onLoad <- function(libname, pkgname) {
   if(exists("NeticaLicenseKey",mode="character")) {
     assignInMyNamespace("LicenseKey", NeticaLicenseKey)
     #cat("License Key Installed.")
   }
   assignInMyNamespace("CCodeLoaded", FALSE)
}

.onAttach <- function(libname, pkgname) {
  .C("RN_Define_Symbols",PACKAGE="RNeticaXR")
   assignInMyNamespace("CCodeLoaded", TRUE)
##   ##StartNetica()
}

.onDetach <- function(libpath) {
   ##StopNetica()
}

.onUnload <- function(libpath) {
  library.dynam.unload("RNeticaXR", libpath)
}
