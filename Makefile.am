## Process this file with automake to produce Makefile.in
# Copyright (C) 2007-2022 Free Software Foundation, Inc.
#
# Author: Ludovic Courtès
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

AM_DISTCHECK_CONFIGURE_FLAGS = \
	--with-guile-site-dir='$$(datarootdir)/guile/site/$$(GUILE_EFFECTIVE_VERSION)' \
	--with-guile-site-ccache-dir='$$(libdir)/guile/$$(GUILE_EFFECTIVE_VERSION)/site-ccache' \
	--with-guile-extension-dir='$$(libdir)/guile/$$(GUILE_EFFECTIVE_VERSION)/extensions'

SUBDIRS = gl guile doc

# git-version-gen
EXTRA_DIST = $(top_srcdir)/.version
BUILT_SOURCES = $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo '$(VERSION)' > $@-t
	mv $@-t $@
dist-hook: gen-ChangeLog
	echo '$(VERSION)' > $(distdir)/.tarball-version

EXTRA_DIST += m4/gnulib-cache.m4

.PHONY: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
						> $(distdir)/cl-t &&	\
	    { rm -f $(distdir)/ChangeLog &&				\
	      mv $(distdir)/cl-t $(distdir)/ChangeLog; }		\
	fi
