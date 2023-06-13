;;; GnuTLS --- Guile bindings for GnuTLS.
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
;;;
;;; GnuTLS is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; GnuTLS is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with GnuTLS; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

(use-modules (guix build-system gnu)
             (guix gexp)
             (guix git)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages tls))

(package
  (name "guile-gnutls")
  (version "3.7.12-git")
  (source (git-checkout (url (dirname (current-filename)))))
  (build-system gnu-build-system)
  (arguments
   (list
    #:configure-flags
    ;; Tell the build system that we want Guile bindings installed to the
    ;; output instead of Guiles own module directory.
    #~(list "--disable-static"
            (string-append "--with-guile-site-dir="
                           "$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)")
            (string-append "--with-guile-site-ccache-dir="
                           "$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache")
            (string-append "--with-guile-extension-dir="
                           "$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions"))))
  (native-inputs
   (list automake autoconf libtool pkg-config texinfo gnutls guile-3.0))
  (inputs (list gnutls guile-3.0))
  (synopsis "Guile bindings to GnuTLS")
  (description
   "This package provides Guile bindings to GnuTLS, a library implementation
the @acronym{TLS, Transport-Layer Security} protocol.  It supersedes the Guile
bindings that were formerly provided as part of GnuTLS.")
  (home-page "https://gitlab.com/gnutls/guile/")
  (license license:lgpl2.1+))
