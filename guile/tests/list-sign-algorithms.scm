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

;; Check the signature algorithm inventory

(use-modules (gnutls)
             (gnutls build tests))

(run-test
 (lambda ()
   (when (sign-algorithm-is-secure? sign-algorithm/rsa-md2 #t)
     (error "Jump ship!"))
   (unless (sign-algorithm-is-secure? sign-algorithm/ecdsa-secp256r1-sha256 #f)
     (error "Are quantum computers a thing now?"))
   (unless (equal? (sign-algorithm->digest-algorithm sign-algorithm/ecdsa-secp256r1-sha256)
                   digest/sha256)
     (error "The ecdsa-sha256 signature algorithm should use \
the sha256 hash algorithm"))
   (let ((all-algorithms (sign-algorithm-list)))
     (unless (member (sign-algorithm->string
                      sign-algorithm/ecdsa-secp256r1-sha256)
                     (map (lambda (alg)
                            (and alg
                                 (sign-algorithm->string alg)))
                          all-algorithms))
       (error "Not enough listed sign algorithms"))
     (for-each
      (lambda (algo)
        (let* ((name (and algo (sign-algorithm->string algo)))
               (same-algo (and name (string->sign-algorithm name)))
               (same-name (and same-algo (sign-algorithm->string same-algo))))
          (unless (and (equal? algo same-algo)
                       (equal? name same-name))
            (error "Inconsistent sign algorithm name")))
        (let* ((oid (and algo (sign-algorithm->oid algo)))
               (same-algo (and oid (oid->sign-algorithm oid)))
               (same-oid (and same-algo (sign-algorithm->oid same-algo))))
          (when (and oid
                     (not (equal? algo sign-algorithm/rsa-pss-rsae-sha256))
                     (not (equal? algo sign-algorithm/rsa-pss-sha384))
                     (not (equal? algo sign-algorithm/rsa-pss-rsae-sha384))
                     (not (equal? algo sign-algorithm/rsa-pss-sha512))
                     (not (equal? algo sign-algorithm/rsa-pss-rsae-sha512)))
            (unless (and (equal? algo same-algo)
                         (equal? oid same-oid))
              (error "Inconsistent sign algorithm oid")))))
      all-algorithms))))
