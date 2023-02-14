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

(use-modules (ice-9 rdelim)
             (ice-9 binary-ports)
             (ice-9 match)
             (rnrs bytevectors)
             (gnutls))

(define (read-curve)
  (format #t "curve:\n")
  (string->ecc-curve (read-line)))

(define (read-parameter name)
  (format #t "~a:\n" name)
  (base64-decode (read-line)))

(define (read-parameters)
  (let* ((curve (read-curve))
         (x (read-parameter 'x))
         (y (read-parameter 'y))
         (k (read-parameter 'k)))
    (values curve x y k)))

(define private-key
  (receive (curve x y k) (read-parameters)
    (let ((key (import-raw-ecc-private-key curve x y k)))
      key)))

(define message (string->utf8 "Hello, world!"))

(define signature
  (private-key-sign-data private-key
                         sign-algorithm/ecdsa-secp521r1-sha512
                         message
                         '()))

(define public-key
  (let ((key (private-key->public-key private-key
                                      (list key-usage/digital-signature))))
    key))

(public-key-verify-data public-key sign-algorithm/ecdsa-secp521r1-sha512
                        message signature)

(format #t "I could sign a message with that key.\n")
