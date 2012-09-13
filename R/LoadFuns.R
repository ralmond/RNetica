## Functions to call when the package is loaded.

.onLoad =
function(libname, pkgname)
{
 library.dynam("RNetica", pkgname, libname)
}  
