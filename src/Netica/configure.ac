## Need to switch location of Netica URL based on OS.
## Unfortunately, need to use configure to do this as there is no portable
## conditional in make.


AC_INIT([Netica], [1.0]) dnl RNetica
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi

AC_CANONICAL_TARGET


case "${target_os}" in
     linux*)
        NETICA_ZIP=https://norsys.com/downloads/NeticaAPI_Linux_607.zip
     	NETICA_DIR=Netica_API_607
	;;
     darwin*)
	NETICA_ZIP=https://norsys.com/downloads/NeticaAPI_Mac.zip
     	NETICA_DIR=Netica_API_504
     	;;
     cigwin*|mingw*)
	NETICA_ZIP=https://norsys.com/downloads/NeticaAPI_Win_510.zip
     	NETICA_DIR=Netica_API_510
     	;;
esac	

NETICA_LIC=https://norsys.com/downloads/LicAgree.txt
AC_CONFIG_FILES([Makefile])
AC_SUBST(NETICA_ZIP)
AC_SUBST(NETICA_LIC)
AC_SUBST(NETICA_DIR)
AC_OUTPUT
