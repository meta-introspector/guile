dnl Process this file with autoconf to produce a configure script.
# Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

# Check for run-time guile features to enable
save_CFLAGS="$CFLAGS"
save_LIBS="$LIBS"
CFLAGS="$CFLAGS $GNUTLS_CFLAGS"
LIBS="$LIBS $GNUTLS_LIBS"
feature_gnutls_hmac_key_size=no
feature_gnutls_hmac_hash_copy=no
feature_gnutls_mac_aes_gmac=no
feature_gnutls_mac_shake=no
feature_gnutls_mac_gost28147_tc26z_imit=no
feature_gnutls_mac_magma_omac=no
feature_gnutls_mac_kuznyechik_omac=no
feature_gnutls_cipher_aes_xts=no
feature_gnutls_cipher_aes_siv=no
feature_gnutls_cipher_aes_192_gcm=no
feature_gnutls_cipher_magma_ctr_acpkm=no
feature_gnutls_cipher_kuznyechik_ctr_acpkm=no
feature_gnutls_cipher_gost28147_tc26z_cnt=no
feature_gnutls_cipher_chacha20=no
feature_gnutls_pk_ecdh_x448=no
feature_gnutls_pk_eddsa_ed448=no
feature_gnutls_sign_eddsa_ed448=no
feature_gnutls_ecc_curve_gost512c=no
feature_gnutls_ecc_curve_gost256x=no
feature_gnutls_ecc_curve_x448=no
feature_gnutls_ecc_curve_ed448=no

AC_MSG_CHECKING([whether gnutls comes with gnutls_hmac_get_key_size])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [gnutls_hmac_get_key_size (0)])],
  [feature_gnutls_hmac_key_size=yes
   AC_DEFINE([HAVE_GNUTLS_HMAC_GET_KEY_SIZE], 1, [Can bind gnutls_hmac_get_key_size])],
  [])
AC_MSG_RESULT($feature_gnutls_hmac_key_size)

AC_MSG_CHECKING([whether gnutls comes with gnutls_hmac/hash_copy])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [gnutls_hmac_copy (NULL);
    gnutls_hash_copy (NULL);
   ])],
  [feature_gnutls_hmac_hash_copy=yes
   AC_DEFINE([HAVE_GNUTLS_HMAC_COPY], 1, [Can bind gnutls_hmac_copy])
   AC_DEFINE([HAVE_GNUTLS_HASH_COPY], 1, [Can bind gnutls_hash_copy])],
  [])
AC_MSG_RESULT($feature_gnutls_hmac_hash_copy)

AC_MSG_CHECKING([whether gnutls supports the AES_GMAC algorithms])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_MAC_AES_GMAC_128 + GNUTLS_MAC_AES_GMAC_192 + GNUTLS_MAC_AES_GMAC_256])],
  [feature_gnutls_mac_aes_gmac=yes
   AC_DEFINE([HAVE_GNUTLS_MAC_AES_GMAC], 1, [The aes-gmac-* algorithms are defined])],
  [AC_DEFINE([NO_GNUTLS_MAC_AES_GMAC_128], 1, [The aes-gmac-128 is not available])
   AC_DEFINE([NO_GNUTLS_MAC_AES_GMAC_192], 1, [The aes-gmac-192 is not available])
   AC_DEFINE([NO_GNUTLS_MAC_AES_GMAC_256], 1, [The aes-gmac-256 is not available])])
AC_MSG_RESULT($feature_gnutls_mac_aes_gmac)

AC_MSG_CHECKING([whether gnutls supports the SHAKE algorithms])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_MAC_SHAKE_128 + GNUTLS_MAC_SHAKE_256])],
  [feature_gnutls_mac_shake=yes
   AC_DEFINE([HAVE_GNUTLS_MAC_SHAKE], 1, [The shake-* algorithms are defined])],
  [AC_DEFINE([NO_GNUTLS_MAC_SHAKE_128], 1, [The shake-128 algorithm is not available])
   AC_DEFINE([NO_GNUTLS_MAC_SHAKE_256], 1, [The shake-256 algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_mac_shake)

AC_MSG_CHECKING([whether gnutls supports the gost28147-tc26z-imit algorithm])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_MAC_GOST28147_TC26Z_IMIT])],
  [feature_gnutls_mac_gost28147_tc26z_imit=yes
   AC_DEFINE([HAVE_GNUTLS_MAC_GOST28147_TC26Z_IMIT], 1,
     [The gost28147_tc26z_imit algorithm is defined])],
  [AC_DEFINE([NO_GNUTLS_MAC_GOST28147_TC26Z_IMIT],
             1, [The gost28147-tc26z-imit algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_mac_gost28147_tc26z_imit)

AC_MSG_CHECKING([whether gnutls supports the magma-omac algorithm])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_MAC_MAGMA_OMAC])],
  [feature_gnutls_mac_magma_omac=yes
   AC_DEFINE([HAVE_GNUTLS_MAC_MAGMA_OMAC], 1,
     [The magma_omac algorithm is defined])],
  [AC_DEFINE([NO_GNUTLS_MAC_MAGMA_OMAC],
             1, [The magma-omac algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_mac_magma_omac)

AC_MSG_CHECKING([whether gnutls supports the kuznyechik-omac algorithm])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_MAC_KUZNYECHIK_OMAC])],
  [feature_gnutls_mac_kuznyechik_omac=yes
   AC_DEFINE([HAVE_GNUTLS_MAC_KUZNYECHIK_OMAC], 1,
     [The kuznyechik_omac algorithm is defined])],
  [AC_DEFINE([NO_GNUTLS_MAC_KUZNYECHIK_OMAC],
             1, [The kuznyechik-omac algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_mac_kuznyechik_omac)

AC_MSG_CHECKING([whether gnutls supports the ecdh-x448 public key algorithm])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_PK_ECDH_X448])],
  [feature_gnutls_pk_ecdh_x448=yes
   AC_DEFINE([HAVE_GNUTLS_PK_ECDH_X448], 1,
     [The ecdh_x448 public key algorithm is defined])],
  [AC_DEFINE([NO_GNUTLS_PK_ECDH_X448],
             1, [The ecdh-x448 public key algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_pk_ecdh_x448)

AC_MSG_CHECKING([whether gnutls supports the eddsa-ed448 public key algorithm])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_PK_EDDSA_ED448])],
  [feature_gnutls_pk_eddsa_ed448=yes
   AC_DEFINE([HAVE_GNUTLS_PK_EDDSA_ED448], 1,
     [The eddsa_ed448 public key algorithm is defined])],
  [AC_DEFINE([NO_GNUTLS_PK_EDDSA_ED448],
             1, [The eddsa-ed448 public key algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_pk_eddsa_ed448)

AC_MSG_CHECKING([whether gnutls supports the eddsa-ed448 signature algorithm])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_SIGN_EDDSA_ED448])],
  [feature_gnutls_sign_eddsa_ed448=yes
   AC_DEFINE([HAVE_GNUTLS_SIGN_EDDSA_ED448], 1,
     [The eddsa_ed448 signature algorithm is defined])],
  [AC_DEFINE([NO_GNUTLS_SIGN_EDDSA_ED448],
             1, [The eddsa-ed448 signature algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_sign_eddsa_ed448)

AC_MSG_CHECKING([whether gnutls supports the gost512c curve])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_ECC_CURVE_GOST512C])],
  [feature_gnutls_ecc_curve_gost512c=yes
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_GOST512C], 1,
     [The gost512c curve is supported])],
  [AC_DEFINE([NO_GNUTLS_ECC_CURVE_GOST512C],
             1, [The gost512c curve is not supported])])
AC_MSG_RESULT($feature_gnutls_ecc_curve_gost512c)

AC_MSG_CHECKING([whether gnutls supports the gost256[ABCD] curve])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_ECC_CURVE_GOST256A;
    GNUTLS_ECC_CURVE_GOST256B;
    GNUTLS_ECC_CURVE_GOST256C;
    GNUTLS_ECC_CURVE_GOST256D;])],
  [feature_gnutls_ecc_curve_gost256x=yes
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_GOST256A], 1,
     [The gost256a curve is supported])
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_GOST256B], 1,
     [The gost256b curve is supported])
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_GOST256C], 1,
     [The gost256c curve is supported])
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_GOST256D], 1,
     [The gost256d curve is supported])],
  [AC_DEFINE([NO_GNUTLS_ECC_CURVE_GOST256A],
             1, [The gost256a curve is not supported])
   AC_DEFINE([NO_GNUTLS_ECC_CURVE_GOST256B],
             1, [The gost256b curve is not supported])
   AC_DEFINE([NO_GNUTLS_ECC_CURVE_GOST256C],
             1, [The gost256c curve is not supported])
   AC_DEFINE([NO_GNUTLS_ECC_CURVE_GOST256D],
             1, [The gost256d curve is not supported])])
AC_MSG_RESULT($feature_gnutls_ecc_curve_gost256x)

AC_MSG_CHECKING([whether gnutls supports the x448 curve])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_ECC_CURVE_X448])],
  [feature_gnutls_ecc_curve_x448=yes
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_X448], 1,
     [The x448 curve is supported])],
  [AC_DEFINE([NO_GNUTLS_ECC_CURVE_X448],
             1, [The x448 curve is not supported])])
AC_MSG_RESULT($feature_gnutls_ecc_curve_x448)

AC_MSG_CHECKING([whether gnutls supports the ed448 curve])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_ECC_CURVE_ED448])],
  [feature_gnutls_ecc_curve_ed448=yes
   AC_DEFINE([HAVE_GNUTLS_ECC_CURVE_ED448], 1,
     [The ed448 curve is supported])],
  [AC_DEFINE([NO_GNUTLS_ECC_CURVE_ED448],
             1, [The ed448 curve is not supported])])
AC_MSG_RESULT($feature_gnutls_ecc_curve_ed448)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the aes-xts ciphers])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_AES_128_XTS])],
  [feature_gnutls_cipher_aes_xts=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_AES_128_XTS],
             1, [The aes-xts cipher algorithms are not available])
   AC_DEFINE([NO_GNUTLS_CIPHER_AES_256_XTS],
             1, [The aes-xts cipher algorithms are not available])])
AC_MSG_RESULT($feature_gnutls_cipher_aes_xts)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the aes-siv ciphers])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_AES_128_SIV])],
  [feature_gnutls_cipher_aes_siv=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_AES_128_SIV],
             1, [The aes-siv cipher algorithms are not available])
   AC_DEFINE([NO_GNUTLS_CIPHER_AES_256_SIV],
             1, [The aes-siv cipher algorithms are not available])])
AC_MSG_RESULT($feature_gnutls_cipher_aes_siv)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the aes-192-gcm cipher])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_AES_192_GCM])],
  [feature_gnutls_cipher_aes_192_gcm=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_AES_192_GCM],
             1, [The aes-192-gcm cipher algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_cipher_aes_192_gcm)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the magma-ctr-acpkm cipher])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_MAGMA_CTR_ACPKM])],
  [feature_gnutls_cipher_magma_ctr_acpkm=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_MAGMA_CTR_ACPKM],
             1, [The aes-192-gcm cipher algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_cipher_magma_ctr_acpkm)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the kuznyechik-ctr-acpkm cipher])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_KUZNYECHIK_CTR_ACPKM])],
  [feature_gnutls_cipher_kuznyechik_ctr_acpkm=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_KUZNYECHIK_CTR_ACPKM],
             1, [The aes-192-gcm cipher algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_cipher_kuznyechik_ctr_acpkm)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the gost28147-tc26z-cnt cipher])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_GOST28147_TC26Z_CNT])],
  [feature_gnutls_cipher_gost28147_tc26z_cnt=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_GOST28147_TC26Z_CNT],
             1, [The gost28147-tc26z-cnt cipher algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_cipher_gost28147_tc26z_cnt)
CFLAGS="$save_CFLAGS"
LIBS="$save_LIBS"

AC_MSG_CHECKING([whether gnutls supports the chacha20-* ciphers])
AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <gnutls/gnutls.h>
#include <gnutls/crypto.h>
], [GNUTLS_CIPHER_CHACHA20_32])],
  [feature_gnutls_cipher_chacha20=yes],
  [AC_DEFINE([NO_GNUTLS_CIPHER_CHACHA20_32],
             1, [The chacha20-32 cipher algorithm is not available])
   AC_DEFINE([NO_GNUTLS_CIPHER_CHACHA20_64],
             1, [The chacha20-64 cipher algorithm is not available])])
AC_MSG_RESULT($feature_gnutls_cipher_chacha20)
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

# Inclusion of example files in the manual requires the absolute
# directory of source, because texi2dvi and pdflatex do not have the
# same search path for included files. So a relative file name will
# never be found by both tools at the same time.
AC_SUBST([abs_top_srcdir])

AC_CONFIG_FILES([guile/pre-inst-guile], [chmod +x guile/pre-inst-guile])
AC_CONFIG_FILES([
  Makefile
  doc/Makefile
  doc/abs-top-srcdir.texi
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

AC_MSG_NOTICE([Summary of optional GnuTLS features:

     hmac-key-size:     $feature_gnutls_hmac_key_size
  {hmac,hash}-copy:     $feature_gnutls_hmac_hash_copy
])

AC_MSG_NOTICE([Availability of GnuTLS algorithms:

             mac/aes-gmac-*:     $feature_gnutls_mac_aes_gmac
                mac/shake-*:     $feature_gnutls_mac_shake
   mac/gost28147-tc26z-imit:     $feature_gnutls_mac_gost28147_tc26z_imit
             mac/magma-omac:     $feature_gnutls_mac_magma_omac
        mac/kuznyechik-omac:     $feature_gnutls_mac_kuznyechik_omac
           cipher/aes-*-xts:     $feature_gnutls_cipher_aes_xts
           cipher/aes-*-siv:     $feature_gnutls_cipher_aes_siv
         cipher/aes-192-gcm:     $feature_gnutls_cipher_aes_192_gcm
     cipher/magma-ctr-acpkm:     $feature_gnutls_cipher_magma_ctr_acpkm
cipher/kuznyechik-ctr-acpkm:     $feature_gnutls_cipher_kuznyechik_ctr_acpkm
 cipher/gost28147-tc26z-cnt:     $feature_gnutls_cipher_gost28147_tc26z_cnt
          cipher/chacha20-*:     $feature_gnutls_cipher_chacha20
])

AC_MSG_NOTICE([Availability of public key algorithms:

    pk-algorithm/ecdh-x448:     $feature_gnutls_pk_ecdh_x448
  pk-algorithm/eddsa-ed448:     $feature_gnutls_pk_eddsa_ed448
])

AC_MSG_NOTICE([Availability of signature algorithms:

  sign-algorithm/eddsa-ed448:     $feature_gnutls_sign_eddsa_ed448
])

AC_MSG_NOTICE([Availability of ECC curves:

       ecc-curve/gost512c:     $feature_gnutls_ecc_curve_gost512c
  ecc-curve/gost256{abcd}:     $feature_gnutls_ecc_curve_gost256x
           ecc-curve/x448:     $feature_gnutls_ecc_curve_x448
          ecc-curve/ed448:     $feature_gnutls_ecc_curve_ed448
])
