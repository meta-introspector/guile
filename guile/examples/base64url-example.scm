;;; GnuTLS --- Guile bindings for GnuTLS.
;;; Copyright (C) 2023 Free Software Foundation, Inc.
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

(use-modules (rnrs bytevectors)
             (gnutls))

(define (base64->base64-url str)
  ;; Replace '+' with '-', '/' with '_', and remove the '=' padding
  ;; characters.
  (string-filter
   (lambda (c)
     (not (eqv? c #\=)))
   (string-map
    (lambda (c)
      (case c
        ((#\+) #\-)
        ((#\/) #\_)
        (else c)))
    str)))

(define (base64-url->base64 str)
  ;; Replace '-' with '+', '_' with '/', and add padding characters.
  (string-append
   (string-map
    (lambda (c)
      (case c
        ((#\-) #\+)
        ((#\_) #\/)
        (else c)))
    str)
   (case (remainder (string-length str) 4)
     ((2) "==")
     ((3) "=")
     (else ""))))

(define data
  (string->utf8 "~~ Hello, world! ~~"))

(define encoded
  (base64->base64-url (base64-encode data)))

(define decoded
  (base64-decode (base64-url->base64 encoded)))

(format #t "The base64-url encoding is: ~s\n"
        encoded)

(format #t "Decoding it back gives: ~s\n"
        (utf8->string decoded))
