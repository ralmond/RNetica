## Functions to call when the package is loaded.

## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL

##These functions start and stop the Netica API Environment.
StartNetica <- function(license=LicenseKey, checking=NULL,
                        maxmem=NULL) {
  invisible(.C("RN_start_Netica",as.character(license),
     as.character(checking),as.double(maxmem), PACKAGE="RNetica"))
}
StopNetica <- function() {
  invisible(.C("RN_stop_Netica",PACKAGE="RNetica"))
}

.onLoad <-
function(libname, pkgname)
{
 library.dynam("RNetica", pkgname, libname)
  ## Deep voodoo needed to get around R safeguards
  ## Need to do it in Load before export
  assignInMyNamespace("EVERY_STATE",.Call("RN_GetEveryState",PACKAGE="RNetica"))
}  

## Give it starter value so it can be found
EVERY_STATE <- -Inf
EVERY_STATE <- function() {
  EVERY_STATE
}

.onAttach <- function(libname, pkgname) {
  if(exists("NeticaLicenseKey",mode="character")) {
    assignInMyNamespace("LicenseKey", NeticaLicenseKey)
    #cat("License Key Installed.")
  }
  StartNetica()
}

.onDetach <- function(libpath) {
  StopNetica()
}

.onUnload <- function(libpath) {
 library.dynam.unload("RNetica", libpath)
}
