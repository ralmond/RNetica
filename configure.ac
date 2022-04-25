# An input file for autoconf to configure 
# 
#
# require at least autoconf 2.61
AC_PREREQ([2.69])

AC_INIT([RNetica],[0.8]) dnl RNetica, 1.8-2

 dnl Get the C compiler, including any values set by the user
 dnl We need this to perform compilation and link checks.
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_PROG_CC

dnl ======================================================

## check for pkg-config
AC_CHECK_PROG(PKGCONFIG,pkg-config,yes)

## default to assuming Netica library has been found
NETICA_GOOD="no"
NETICA_VERSION=504

## also use pkg-config to check for Netica
##

if test x"${PKGCONFIG}" == x"yes"; then
   ## check via pkg-config for nlopt
   if pkg-config --exists netica; then
       ## obtain cflags and obtain libs
       NETICA_HOME=$(pkg-config --variable=prefix netica)
       NETICA_CFLAGS=$(pkg-config --cflags netica)
       NETICA_LIBS=$(pkg-config --libs netica)
       NETICA_VERSION=$(pkg-config --modversion netica)
       NETICA_GOOD="yes"
   fi
fi


AC_ARG_WITH([netica],
            AS_HELP_STRING([--with-netica=PATH],
                           [the path to the Netica API directory]),
            [NETICA_HOME=${withval}]
            )
if test -z "${NETICA_HOME}" ; then
   NETICA_GOOD="no"
else 
   AC_MSG_CHECKING([existance of NETICA_HOME or --with-netica])
   AC_MSG_RESULT($NETICA_HOME)
   AS_IF([test ! -e "${NETICA_HOME}/src/Netica.h"],
   [NETICA_GOOD="yes"])
fi


## in case neither of the two methods above worked, download Netica from Norsys 
## web site
if test x"${NETICA_GOOD}" = x"no"; then
   AC_MSG_NOTICE([Need to download Netica from Norsys.com])

   (cd src/Netica; ./configure; make NeticaAPI) || exit 1
   NETICA_HOME="$(pwd)/src/Netica/Netica_API_504"
   AS_IF([test ! -d "${NETICA_HOME}"],
   	 [NETICA_HOME="$(pwd)/src/Netica/Netica_API_607";
	  NETICA_VERSION=6.0.7])
    AC_MSG_CHECKING([Downloaded Netica])
    AC_MSG_RESULT($NETICA_HOME)
fi

## Netica has non-standard path to lib64 directory.
## which is different in different versions:  Arg!!

NETICA_LIB="${NETICA_HOME}/lib"
AC_CHECK_SIZEOF([long])
AS_IF([test $ac_cv_sizeof_long -eq 8],
      [NETICA_LIB="${NETICA_HOME}/lib/64bit"])
AS_IF([test $ac_cv_sizeof_long -eq 4],
      [NETICA_LIB="${NETICA_HOME}/lib/32bit"])
## Now, if this version doesn't exist, fall back.
AS_IF([test ! -d "${NETICA_LIB}"],
      [ NETICA_LIB="${NETICA_HOME}/lib"])
AC_MSG_CHECKING([Location of Netica library])
AC_MSG_RESULT($NETICA_LIB)


NETICA_INCDIR="${NETICA_HOME}/src"
AS_IF([ test x"${NETICA_LIBS}" == x""],
     [NETICA_LIBS="-L${NETICA_LIB} -lnetica"])

# NETICA_LIBS="${NETICA_LIBS} -Wl,-rpath,${NETICA_LIBPATH}"
NETICA_CFLAGS="-I${NETICA_INCDIR} -DNETICA_VERSION=${NETICA_VERSION//.}"
## This is for fixing the links in the shared object to point to the
# install directory. See Rblpapi example.
# Need to substitute @<:@ and @:>@ for M4 quote characters []
# NETICA_LIBPATH=$(${R_HOME}/bin/Rscript -e 'cat(file.path(.libPaths()[1], "RNetica", "Netica"))')
NETICA_LIBPATH=$(${R_HOME}/bin/Rscript -e 'cat(file.path(.libPaths()@<:@1@:>@, "RNetica", "Netica"))')


echo "****************************************"
echo "Configuration information:"
echo ""
echo "Netica settings"
echo ""
echo "R Architecture:     ${R_ARCH}"
echo "Netica API DIRECTORY:     ${NETICA_HOME}"
echo "Netica Lib Flags:     ${NETICA_LIBS}"
echo "Netica CFlags Directory: ${NETICA_CFLAGS}"
echo "Netica version: ${NETICA_VERSION}"
echo "Netica RPATH: ${NETICA_LIBPATH}"
echo ""
echo "Compilation flags:        ${CFLAGS} ${LANGUAGE_DEFS}"
echo "Link flags:               ${LIBS}"
AC_SUBST(NETICA_LIB)
AC_SUBST(NETICA_LIBPATH)
AC_SUBST(NETICA_LIBS)
AC_SUBST(NETICA_INCDIR)
AC_SUBST(NETICA_VERSION)
AC_SUBST(NETICA_CFLAGS)
AC_CONFIG_HEADERS(src/config.h)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

