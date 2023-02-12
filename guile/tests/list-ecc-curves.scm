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

;; Check the ECC curve inventory

(use-modules (gnutls)
             (gnutls build tests))

(run-test
 (lambda ()
   (unless (eqv? (ecc-curve-size ecc-curve/secp256r1) 32)
     (error "Wrong ECC curve size"))
   (unless (equal? (pk-algorithm->string (ecc-curve->pk-algorithm ecc-curve/secp256r1))
                   (pk-algorithm->string pk-algorithm/ecdsa))
     (error "Wrong PK algorithm for curve"))
   (let ((all-curves (ecc-curve-list)))
     (unless (member (ecc-curve->string ecc-curve/secp256r1)
                     (map ecc-curve->string all-curves))
       (error "Not enough listed ECC curves"))
     (for-each
      (lambda (curve)
        (let* ((name (ecc-curve->string curve))
               (same-curve (string->ecc-curve name))
               (same-name (ecc-curve->string same-curve)))
          (unless (and (equal? curve same-curve)
                       (equal? name same-name))
            (error "Inconsistent sign algorithm name")))
        (let* ((oid (ecc-curve->oid curve))
               (same-curve (and oid (oid->ecc-curve oid)))
               (same-oid (and same-curve (ecc-curve->oid same-curve))))
          (when oid
            (unless (and (equal? curve same-curve)
                         (equal? oid same-oid))
              (error "Inconsistent sign algorithm oid")))))
      all-curves))))
