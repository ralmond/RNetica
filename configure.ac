# An input file for autoconf to configure 
# 
#
AC_INIT([RNetica], 1.1) dnl RNetica, 1.1-8

 dnl Get the C compiler, including any values set by the user
 dnl We need this to perform compilation and link checks.
#AC_PROG_CC

dnl ======================================================

dnl DEFAULT_NETICA_HOME="/home/ralmond/software/Netica_API_504"
DEFAULT_NETICA_HOME="/opt/Netica_API_504"
AC_ARG_WITH([netica-home],
            AC_HELP_STRING([--with-netica=PATH],
                           [the path to the Netica API directory]),
            [NETICA_HOME=${withval}],
            [NETICA_HOME=${DEFAULT_NETICA_HOME}]
            )
NETICA_LIB="${NETICA_HOME}/lib"
dnl Netica has non-standard path to lib64 directory.
AC_CHECK_SIZEOF([long]);
AS_IF([test $ac_cv_sizeof_long" -eq 8],
      [NETICA_LIB="${NETICA_HOME}/lib/64bit"])
NETICA_INCDIR="${NETICA_HOME}/src"
AC_SUBST(NETICA_LIB)
AC_SUBST(NETICA_INCDIR)

echo "****************************************"
echo "Configuration information:"
echo ""
echo "Netica settings"
echo ""
echo "Netica API DIRECTORY:     ${R_ARCHb}"
echo "Netica API DIRECTORY:     ${NETICA_HOME}"
echo "Netica Lib DIRECTORY:     ${NETICA_LIB}"
echo "Netica Include Directory: ${NETICA_INCDIR}"
echo ""
echo "Compilation flags:        ${PKG_CLAGS} ${LANGUAGE_DEFS}"
echo "Link flags:               ${PKG_LIBS}"


AC_OUTPUT(src/Makevars)
#chmod +x cleanup

