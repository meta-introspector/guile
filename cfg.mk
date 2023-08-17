# Copyright (C) 2022-2023 Free Software Foundation, Inc.
#
# Author: Simon Josefsson
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

manual_title = Guile binding for GnuTLS

VC_LIST_ALWAYS_EXCLUDE_REGEX = doc/fdl-1.3.texi
update-copyright-env = UPDATE_COPYRIGHT_USE_INTERVALS=2

gendocs_options_ = -s gnutls-guile.texi

old_NEWS_hash = c9126a3b8c344c487f0b2754ad8d6d69

bootstrap-tools = autoconf,automake,makeinfo,libtoolize

upload_dest_dir_ = gnutls

local-checks-to-skip = sc_GPL_version sc_error_message_uppercase	\
	sc_prohibit_have_config_h

gl_public_submodule_commit =

exclude_file_name_regexp--sc_prohibit_test_minus_ao = ^m4/guile.m4$$
exclude_file_name_regexp--sc_readme_link_copying = ^guile/modules/system/documentation/README$$
exclude_file_name_regexp--sc_readme_link_install = $(exclude_file_name_regexp--sc_readme_link_copying)

VC_LIST_ALWAYS_EXCLUDE_REGEX = ^maint.mk|m4/lib-link.m4|m4/lib-prefix.m4|gl/top/|build-aux/gnupload$$

indent-guile:
	-$(AM_V_at)find . -name \*.scm | xargs emacs -Q --batch --eval '(mapc (lambda (file) (find-file file) (indent-region (point-min) (point-max)) (untabify (point-min) (point-max)) (delete-trailing-whitespace) (save-buffer) (kill-buffer)) command-line-args-left)' 2>&1 | grep -v '^Indenting region...'

sc_indent_guile:
	@if test -d $(srcdir)/.git				\
		&& git --version >/dev/null 2>&1			\
		&& command -v emacs > /dev/null; then \
	  $(MAKE) -s indent-guile && git diff --exit-code; \
	fi
