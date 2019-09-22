
# check for one particular file of the sources 
AC_INIT([ocaml-fdkaac],[0.3.1],[savonet-users@lists.sourceforge.net])

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

AC_MSG_CHECKING([whether pkg-config knows about fdk-aac])
if ! $PKG_CONFIG $PKG_CONFIG_OPTIONS --exists fdk-aac; then
  AC_MSG_ERROR([fdk-aac.pc not found.. Do you need to set PKG_CONFIG_PATH?])
else
  fdkaac_version=`$PKG_CONFIG $PKG_CONFIG_OPTIONS --modversion fdk-aac`
  if ! $PKG_CONFIG $PKG_CONFIG_OPTIONS --atleast-version=0.1.1 fdk-aac; then
    AC_MSG_ERROR([requires version >= $2, found ${fdkaac_version}])
  else
    if ! $PKG_CONFIG $PKG_CONFIG_OPTIONS --max-version=0.1.6 fdk-aac; then
      AC_MSG_ERROR([requires version <= 0.1.6, found ${fdkaac_version}])
    else
      AC_MSG_RESULT([ok])
    fi
  fi
fi
CFLAGS="$CFLAGS `$PKG_CONFIG $PKG_CONFIG_OPTIONS --cflags fdk-aac`"
CPPFLAGS="$CPPFLAGS `$PKG_CONFIG $PKG_CONFIG_OPTIONS --cflags fdk-aac`"
LIBS="$LIBS `$PKG_CONFIG $PKG_CONFIG_OPTIONS --libs-only-l fdk-aac`"
LDFLAGS="$LDFLAGS `$PKG_CONFIG $PKG_CONFIG_OPTIONS --libs-only-L fdk-aac`"

# substitutions to perform
AC_SUBST(VERSION)
AC_SUBST(INC)
AC_SUBST(requires)

# Finally create the Makefile and samples
AC_CONFIG_FILES([Makefile],[chmod a-w Makefile])
AC_CONFIG_FILES([src/META])
AC_CONFIG_FILES([src/Makefile])
AC_OUTPUT
