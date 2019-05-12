
# check for one particular file of the sources 
AC_INIT([ocaml-fdkaac],[0.3.0],[savonet-users@lists.sourceforge.net])

VERSION=$PACKAGE_VERSION
AC_MSG_RESULT([configuring $PACKAGE_STRING])

AC_BASE_CHECKS()

AC_MSG_CHECKING([whether to build shared modules])
if test -n "${OCAML_HAS_FIRST_CLASS_MODULES}"; then
  AC_MSG_RESULT([yes])
else
  OCAML_DYNLINK=""
  AC_MSG_RESULT([first-class modules not supported by the compiler, disabling shared modules])
fi

PKG_PROG_PKG_CONFIG()
PKG_CONFIG_CHECK_MODULE([fdk-aac],[0.1.1])

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(INC)
AC_SUBST(requires)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_OUTPUT
