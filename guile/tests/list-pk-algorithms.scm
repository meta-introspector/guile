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

;; Check the public key algorithm inventory

(use-modules (gnutls)
             (gnutls build tests))

(run-test
 (lambda ()
   (let ((all-algorithms (pk-algorithm-list)))
     (unless (member (pk-algorithm->string pk-algorithm/ecdsa)
                     (map pk-algorithm->string all-algorithms))
       (error "Not enough listed pk algorithms"))
     (for-each
      (lambda (algo)
        (let* ((name (pk-algorithm->string algo))
               (same-algo (string->pk-algorithm name))
               (same-name (pk-algorithm->string same-algo)))
          (unless (and (equal? algo same-algo)
                       (equal? name same-name))
            (error "Inconsistent PK algorithm name")))
        (let* ((oid (pk-algorithm->oid algo))
               (same-algo (and oid (oid->pk-algorithm oid)))
               (same-oid (and same-algo (pk-algorithm->oid same-algo))))
          (when oid
            (unless (and (equal? algo same-algo)
                         (equal? oid same-oid))
              (error "Inconsistent PK oid"))))
        (let ((sign-algorithm (pk-algorithm->sign-algorithm algo digest/sha256)))
          (unless (or (not sign-algorithm)
                      (equal? sign-algorithm sign-algorithm/unknown))
            (unless (sign-algorithm-supports? sign-algorithm algo)
              (error "Inconsistent PK supported algorithms"))
            (let ((any-pk (sign-algorithm->pk-algorithm sign-algorithm)))
              (unless (sign-algorithm-supports? sign-algorithm any-pk)
                (error "Inconsistent PK supported algorithms"))))))
      all-algorithms))))
