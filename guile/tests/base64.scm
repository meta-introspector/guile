;;; GnuTLS --- Guile bindings for GnuTLS
;;; Copyright (C) 2011-2022 Free Software Foundation, Inc.
;;;
;;; GnuTLS is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GnuTLS is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GnuTLS-EXTRA; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;;; USA.

;; Check the base64 encoding and decoding

(use-modules (gnutls)
             (gnutls build tests)
             (rnrs bytevectors))

(define hello
  "Hello, world!")

(run-test
 (lambda ()
   (unless (equal? (base64-encode hello)
                   "SGVsbG8sIHdvcmxkIQ==")
     (error "encoding failed"))
   (unless (equal? (base64-encode (string->utf8 hello))
                   (base64-encode hello))
     (error "encoding a bytevector failed"))
   (unless (equal? (base64-decode "SGVsbG8sIHdvcmxkIQ==")
                   (string->utf8 hello))
     (error "decoding failed"))
   (unless (equal? (base64-decode (string->utf8 "SGVsbG8sIHdvcmxkIQ=="))
                   (base64-decode "SGVsbG8sIHdvcmxkIQ=="))
     (error "decoding a bytevector failed"))))
