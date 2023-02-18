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

(define random-data
  (gnutls-random
   ;; Choose a security level: /nonce, /random or /key.
   random-level/nonce
   ;; Choose the number of bytes:
   4))

(let ((dice-roll
       (remainder
        (car (bytevector->uint-list random-data (endianness little) 4))
        6)))
  (format #t "You roll a ~a.\n" (+ dice-roll 1)))
