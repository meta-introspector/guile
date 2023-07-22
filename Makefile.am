## Process this file with automake to produce Makefile.in
# Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

SUBDIRS = guile doc

EXTRA_DIST = cfg.mk CONTRIBUTING.md

# git-version-gen
EXTRA_DIST += $(top_srcdir)/.version
BUILT_SOURCES = $(top_srcdir)/.version
$(top_srcdir)/.version:
	echo '$(VERSION)' > $@-t
	mv $@-t $@
dist-hook: gen-ChangeLog
	echo '$(VERSION)' > $(distdir)/.tarball-version

.PHONY: gen-ChangeLog
gen-ChangeLog:
	$(AM_V_GEN)if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
						> $(distdir)/cl-t &&	\
	    { rm -f $(distdir)/ChangeLog &&				\
	      mv $(distdir)/cl-t $(distdir)/ChangeLog; }		\
	fi

EXTRA_DIST += $(top_srcdir)/build-aux/announce-gen
EXTRA_DIST += $(top_srcdir)/build-aux/do-release-commit-and-tag
EXTRA_DIST += $(top_srcdir)/build-aux/gendocs.sh
EXTRA_DIST += $(top_srcdir)/build-aux/git-version-gen
EXTRA_DIST += $(top_srcdir)/build-aux/gitlog-to-changelog
EXTRA_DIST += $(top_srcdir)/build-aux/gnu-web-doc-update
EXTRA_DIST += $(top_srcdir)/GNUmakefile
EXTRA_DIST += $(top_srcdir)/build-aux/gnupload
EXTRA_DIST += $(top_srcdir)/build-aux/config.rpath
EXTRA_DIST += $(top_srcdir)/maint.mk
EXTRA_DIST += $(top_srcdir)/build-aux/update-copyright
EXTRA_DIST += $(top_srcdir)/build-aux/useless-if-before-free
EXTRA_DIST += $(top_srcdir)/build-aux/vc-list-files
