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
             (rnrs bytevectors)
             (gnutls))

;; To define a symmetric encryption cipher context, you need an algorithm, a
;; key, and an initialization vector.

(define algorithm cipher/aes-128-cbc)

(define cipher
  (let ((initialisation-vector
         (string->utf8 "Initialisation.."))
        (key
         (string->utf8 "The 16-byte key.")))
    (unless (eqv? (bytevector-length initialisation-vector)
                  (cipher-iv-size algorithm))
      (error "Incorrect initialization vector size."))
    (unless (eqv? (bytevector-length key)
                  (cipher-key-size algorithm))
      (error "Incorrect key size."))
    (make-cipher algorithm key initialisation-vector)))

;; The context may be used to encrypt and decrypt data, if the data spans an
;; integer number of blocks.

(define block-size
  (cipher-block-size (cipher-algorithm cipher)))

(define data
  (string->utf8 "The data to encrypt must be a bytevector \
whose length must be a multiple of the block size. If you \
want to use the low-level cipher API, you must manage the \
data padding yourself, and know the message length."))

(define encrypted
  (cipher-encrypt cipher data))

(define decrypted
  (cipher-decrypt cipher encrypted))

(unless (equal? data decrypted)
  (error "data decryption failed."))
