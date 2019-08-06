# An input file for autoconf to configure 
# 
#
AC_INIT([RNetica], 1.1) dnl RNetica, 1.1-8

 dnl Get the C compiler, including any values set by the user
 dnl We need this to perform compilation and link checks.
#AC_PROG_CC

dnl ======================================================

dnl DEFAULT_NETICA_HOME="/home/ralmond/software/Netica_API_504"
dnl DEFAULT_NETICA_HOME="/opt/Netica_API_504"
AC_ARG_WITH([netica],
            AC_HELP_STRING([--with-netica=PATH],
                           [the path to the Netica API directory]),
            [NETICA_HOME=${withval}]
            )
echo "++++++++++++++++++++++++"
echo "Netica Home is ${NETICA_HOME}"
if test -z "${NETICA_HOME}" ; then
  echo "Using pkg-config for Netica"
  PKG_CHECK_MODULES([NETICA],[Netica],
    [NETICA_HOME = "${NETICA_EXEC_PREFIX}"],
    [AC_MSG_ERROR([Netica package not supplied with --with-netica, with NETICA_HOME variable or pkg-config])])
else 
  echo "Using NETICA_HOME for Netica"
  NETICA_LIB="${NETICA_HOME}/lib"
dnl Netica has non-standard path to lib64 directory.
  AC_CHECK_SIZEOF([long])
  AS_IF([test $ac_cv_sizeof_long -eq 8],
     [NETICA_LIB="${NETICA_HOME}/lib/64bit"])
dnl Fallback for MacOS X which does not have a separate 64 bit directory
  if test ! -d "${NETICA_LIB}"; then
    NETICA_LIB="${NETICA_HOME}/lib"
  fi
  AC_DEFINE([NETICA_LIBS],"-L${NETICA_LIB} -lnetica")
  NETICA_INCDIR="${NETICA_HOME}/src"
  AC_DEFINE([NETICA_CFLAGS],"-I${NETICA_INCDIR}")
fi
AC_SUBST(NETICA_LIBS)
AC_SUBST(NETICA_CFLAGS)
echo "****************************************"
echo "Configuration information:"
echo ""
echo "Netica settings"
echo ""
echo "R Architecture:     ${R_ARCH}"
echo "Netica API DIRECTORY:     ${NETICA_HOME}"
echo "Netica Lib Flags:     ${NETICA_LIBS}"
echo "Netica CFlags Directory: ${NETICA_CFLAGS}"
echo ""
echo "Compilation flags:        ${PKG_CFLAGS} ${LANGUAGE_DEFS}"
echo "Link flags:               ${PKG_LIBS}"


AC_OUTPUT(src/Makevars)
#chmod +x cleanup

