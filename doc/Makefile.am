# Guile-GnuTLS --- Guile bindings for GnuTLS.
# Copyright (C) 2007-2023 Free Software Foundation, Inc.
#
# This file is part of Guile-GnuTLS.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

info_TEXINFOS = gnutls-guile.texi
gnutls_guile_TEXINFOS = fdl-1.3.texi cha-copying.texi

infoimagesdir = $(infodir)
infoimages_DATA = $(IMAGES)
html_DATA = $(IMAGES)

AM_MAKEINFOFLAGS = -I $(top_srcdir)/doc
TEXI2DVI = texi2dvi $(AM_MAKEINFOFLAGS)
AM_MAKEINFOHTMLFLAGS = $(AM_MAKEINFOFLAGS) --no-split

MAINTAINERCLEANFILES =
EXTRA_DIST =

# Guile texinfos.

guile_texi = core.c.texi
BUILT_SOURCES        = $(guile_texi)
MAINTAINERCLEANFILES += $(guile_texi)
EXTRA_DIST           += $(guile_texi) extract-guile-c-doc.scm
guile_TEXINFOS       = gnutls-guile.texi $(guile_texi)

GUILE_FOR_BUILD =				\
  GUILE_AUTO_COMPILE=0				\
  $(GUILE) -q -L $(top_srcdir)/guile/modules

SNARF_CPPFLAGS = -I$(top_srcdir) -I$(top_builddir)			\
	 -I$(top_srcdir)/lib/includes -I$(top_builddir)/lib/includes	\
	 -I$(top_srcdir)/extra/includes				\
         -I$(top_srcdir)/guile/src -I$(top_builddir)/guile/src		\
	 $(GUILE_CFLAGS) $(GNUTLS_CFLAGS)

core.c.texi: $(top_srcdir)/guile/src/core.c
	$(MAKE) -C ../guile/src built-sources &&			\
	$(GUILE_FOR_BUILD) -l "$(srcdir)/extract-guile-c-doc.scm"	\
	   -e '(apply main (cdr (command-line)))'			\
	   -- "$^" "$(CPP)" "$(SNARF_CPPFLAGS) $(CPPFLAGS)"		\
	   > "$@"
