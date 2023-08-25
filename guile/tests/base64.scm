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
     (error "decoding a bytevector failed"))
   (unless (equal? (with-output-to-string
                     (lambda ()
                       (load-from-path "base16-example.scm")))
                   "\
The base16 encoding is: \"48656c6c6f2c20776f726c6421\"\n\
Decoding it back gives: \"Hello, world!\"\n")
     (error "base16 failed"))
   (unless (equal? (with-output-to-string
                     (lambda ()
                       (load-from-path "base64-example.scm")))
                   "\
The base64 encoding is: \"SGVsbG8sIHdvcmxkIQ==\"\n\
Decoding it back gives: \"Hello, world!\"\n")
     (error "base64 failed"))
   (unless (equal? (with-output-to-string
                     (lambda ()
                       (load-from-path "base64url-example.scm")))
                   "\
The base64-url encoding is: \"fn4gSGVsbG8sIHdvcmxkISB-fg\"\n\
Decoding it back gives: \"~~ Hello, world! ~~\"\n")
     (error "base64-url failed"))))
