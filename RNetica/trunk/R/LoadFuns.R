## Functions to call when the package is loaded.

## Variable which stores the Netica License string (if you have one).
LicenseKey <- NULL

##These functions start and stop the Netica API Environment.
StartNetica <- function(license=LicenseKey, checking=NULL,
                        maxmem=NULL) {
  invisible(.C("RN_start_Netica",as.character(LicenseKey),
     as.character(checking),as.double(maxmem), PACKAGE="RNetica"))
}
StopNetica <- function() {
  invisible(.C("RN_stop_Netica",PACKAGE="RNetica"))
}

.onLoad =
function(libname, pkgname)
{
 library.dynam("RNetica", pkgname, libname)
}  

## Give it starter value so it can be found
EVERY_STATE <- -Inf

.onAttach <- function(libname, pkgname) {
  if(exists("NeticaLicenseKey",mode="character"))
    LicenseKey <- NeticaLicenseKey
  StartNetica()
  ## Deep voodoo needed to get around R safeguards
  assignInNamespace("EVERY_STATE",.Call("RN_GetEveryState",PACKAGE="RNetica"),"RNetica")
}

.Last.lib <- function(libpath) {
  StopNetica()
}

.onUnload <- function(libpath) {
 library.dynam.unload("RNetica", libpath)
}
