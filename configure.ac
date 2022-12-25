dnl Process this file with autoconf to produce a configure script.
# Copyright (C) 2000-2022 Free Software Foundation, Inc.
#
# Author: Ludovic Courtès, Simon Josefsson, Nikos Mavrogiannopoulos
#
# This file is part of Guile-GnuTLS.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
# USA

AC_INIT([Guile-GnuTLS],
        m4_esyscmd([build-aux/git-version-gen .tarball-version]),
        [gnutls-help@lists.gnutls.org], [guile-gnutls],
        [https://gitlab.com/gnutls/guile])

AC_MSG_NOTICE([$PACKAGE_NAME $PACKAGE_VERSION $PACKAGE_TARNAME])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])

AM_INIT_AUTOMAKE([-Wall gnu])
AM_SILENT_RULES([yes])

AC_CONFIG_HEADERS([config.h])

dnl Checks for programs.
AC_PROG_CC
AM_PROG_AR
LT_INIT([disable-static])

PKG_CHECK_MODULES(GNUTLS, [gnutls])

ac_enable_srp=yes
AC_MSG_CHECKING([whether to disable SRP authentication support])
AC_ARG_ENABLE(srp-authentication,
  AS_HELP_STRING([--disable-srp-authentication],
                 [disable the SRP authentication support]),
  ac_enable_srp=$enableval)
if test x$ac_enable_srp != xno; then
 AC_MSG_RESULT(no)
 AC_DEFINE([ENABLE_SRP], 1, [enable SRP authentication])
else
 ac_full=0
 AC_MSG_RESULT(yes)
fi
AM_CONDITIONAL(ENABLE_SRP, test "$ac_enable_srp" != "no")

dnl Guile bindings.
AC_ARG_WITH([guile-site-dir], AS_HELP_STRING([--with-guile-site-dir=DIR],
    [guile site directory for gnutls, default is guile system settings]),
    [guilesitedir="${withval}"], [guilesitedir='$(GUILE_SITE)'])
AC_ARG_WITH([guile-site-ccache-dir], AS_HELP_STRING([--with-guile-site-ccache-dir=DIR],
    [guile ccache directory for gnutls, default is guile system settings]),
    [guilesiteccachedir="${withval}"], [guilesiteccachedir='$(GUILE_SITE_CCACHE)'])
AC_ARG_WITH([guile-extension-dir], AS_HELP_STRING([--with-guile-extension-dir=DIR],
    [guile extension directory for gnutls, default is guile system settings]),
    [guileextensiondir="${withval}"], [guileextensiondir='$(GUILE_EXTENSION)'])
AC_SUBST([guilesitedir])
AC_SUBST([guilesiteccachedir])
AC_SUBST([guileextensiondir])
maybe_guileextensiondir="\"$guileextensiondir\""

AC_PATH_PROG([guile_snarf], [guile-snarf])
if test "x$guile_snarf" = "x"; then
   AC_MSG_ERROR([`guile-snarf' from Guile not found.])
fi

dnl Check for 'guild', which can be used to compile Scheme code
dnl on Guile 2.x.
AC_PATH_PROG([GUILD], [guild])
AC_SUBST([GUILD])

GUILE_PKG([3.0 2.2 2.0])
GUILE_PROGS
GUILE_SITE_DIR
GUILE_FLAGS

# Backward compatibility with <guile-2.2 m4 macro that is used
# due to autreconf of several CI machine.
# We need to guess the locations of ccache and extension
if test -z "${GUILE_SITE_CCACHE}"; then
    AC_MSG_NOTICE([Found <guile-2.2 m4, macro emulating])

    AC_MSG_CHECKING([for GUILE_SITE_CCACHE via pkg-config])
    GUILE_SITE_CCACHE=`$PKG_CONFIG --variable=siteccachedir guile-$GUILE_EFFECTIVE_VERSION`
    AC_MSG_RESULT([${GUILE_SITE_CCACHE}])
    if test -z "${GUILE_SITE_CCACHE}"; then
            AC_MSG_CHECKING([for GUILE_SITE_CCACHE via guile])
            GUILE_SITE_CCACHE=`$GUILE -c "(display (if (defined? '%site-ccache-dir) (%site-ccache-dir) \"\"))"`
            AC_MSG_RESULT([${GUILE_SITE_CCACHE}])
    fi
    AC_SUBST([GUILE_SITE_CCACHE])

    AC_MSG_CHECKING([for GUILE_EXTENSION])
    GUILE_EXTENSION=`$PKG_CONFIG --print-errors --variable=extensiondir guile-$GUILE_EFFECTIVE_VERSION`
    AC_MSG_RESULT([${GUILE_EXTENSION}])
    AC_SUBST([GUILE_EXTENSION])
fi

save_CFLAGS="$CFLAGS"
save_LIBS="$LIBS"
CFLAGS="$CFLAGS $GUILE_CFLAGS"
LIBS="$LIBS $GUILE_LDFLAGS"
AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <libguile.h>], [scm_from_locale_string ("")])],
  [], [AC_MSG_ERROR([A sufficiently recent GNU Guile not found.])])
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gcc supports -fgnu89-inline])
_gcc_cflags_save="$CFLAGS"
CFLAGS="${CFLAGS} -fgnu89-inline"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])], gnu89_inline=yes, gnu89_inline=no)
AC_MSG_RESULT($gnu89_inline)
CFLAGS="$_gcc_cflags_save"

# Optional Guile functions.
save_CFLAGS="$CFLAGS"
save_LIBS="$LIBS"
CFLAGS="$CFLAGS $GUILE_CFLAGS"
LIBS="$LIBS $GUILE_LDFLAGS"
AC_CHECK_FUNCS([scm_gc_malloc_pointerless])
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

# Do we need to hard-code $guileextensiondir in gnutls.scm?
# This is not necessary when $guileextensiondir is equal to
# Guile's 'extensiondir' as specified in 'guile-MAJOR.MINOR.pc'.
if test "$guileextensiondir" = "`$PKG_CONFIG guile-$GUILE_EFFECTIVE_VERSION --variable extensiondir`" \
     || test "$guileextensiondir" = '$(GUILE_EXTENSION)'; then
  maybe_guileextensiondir='#f'
fi

AC_SUBST([maybe_guileextensiondir])
AM_CONDITIONAL([HAVE_GUILD], [test "x$GUILD" != "x"])
AM_CONDITIONAL([CROSS_COMPILING], [test "x$cross_compiling" = "xyes"])
AM_CONDITIONAL(HAVE_GCC_GNU89_INLINE_OPTION, test "$gnu89_inline" = "yes"])
AM_CONDITIONAL(HAVE_GCC, test "$GCC" = "yes")

AC_CONFIG_FILES([guile/pre-inst-guile], [chmod +x guile/pre-inst-guile])
AC_CONFIG_FILES([
  Makefile
  doc/Makefile
  guile/Makefile
  guile/src/Makefile
])

AC_OUTPUT

AC_MSG_NOTICE([Summary of build options:

  Version:           ${VERSION}
  Build system:      ${build}
  Host system:       ${host}
  Install prefix:    ${prefix}
  Compiler:          ${CC}
  CFLAGS:            ${CFLAGS}
  CPPFLAGS:          ${CPPFLAGS}
  LDFLAGS:           ${LDFLAGS}
  Library types:     Shared=${enable_shared}, Static=${enable_static}
  GNUTLS:
         LIBS:       ${GNUTLS_LIBS}
       CFLAGS:       ${GNUTLS_CFLAGS}
          SRP:       $ac_enable_srp
  Guile version:     $GUILE_EFFECTIVE_VERSION
           GUILE:    $GUILE
          CFLAGS:    $GUILE_CFLAGS
         LDFLAGS:    $GUILE_LDFLAGS
         sitedir:    $guilesitedir - $GUILE_SITE
   siteccachedir:    $guilesiteccachedir - $GUILE_SITE_CCACHE
    extensiondir:    $guileextensiondir - $GUILE_EXTENSION
     guile-snarf:    $guile_snarf
           GUILD:    $GUILD
])
