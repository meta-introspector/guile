# Copyright (C) 2022 Free Software Foundation, Inc.
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

gendocs_options_ = -s gnutls-guile.texi

old_NEWS_hash = d41d8cd98f00b204e9800998ecf8427e

bootstrap-tools = autoconf,automake,libtoolize,gnulib,makeinfo,tar,gzip

local-checks-to-skip = sc_GPL_version sc_error_message_uppercase	\
	sc_prohibit_have_config_h

exclude_file_name_regexp--sc_prohibit_test_minus_ao = ^m4/guile.m4$$
