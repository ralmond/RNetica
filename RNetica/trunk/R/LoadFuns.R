## Functions to call when the package is loaded.

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

.onLoad =
function(libname, pkgname)
{
 library.dynam("RNetica", pkgname, libname)
}  

.onAttach <- function(libname, pkgname) {
  if(exists("NeticaLicenseKey",mode="character"))
    LicenseKey <- NeticaLicenseKey
  StartNetica()
}

.Last.lib <- function(libpath) {
  StopNetica()
}

.onUnload <- function(libpath) {
 library.dynam.unload("RNetica", libpath)
}
