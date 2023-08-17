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

;;;
;;; Exercise the X.509 key and certificate generation API.
;;;

(use-modules (gnutls)
             (gnutls build tests)
             (srfi srfi-1)
             (srfi srfi-4)
             (srfi srfi-11)
             (ice-9 format))

(run-test
 (lambda ()
   (let ((key (generate-x509-private-key pk-algorithm/rsa 2048 '()))
         (cert (make-x509-certificate))
         (cert-format x509-certificate-format/pem)
         (serial #vu8(1 2 3 4))
         (activation 946702800) ; 2000-01-01
         (expiration 1893474000)) ; 2030-01-01
     (set-x509-certificate-ca-status! cert #f)
     (set-x509-certificate-dn-by-oid! cert oid/x520-country-name "US")
     (set-x509-certificate-dn-by-oid! cert oid/x520-state-or-province-name "MA")
     (set-x509-certificate-dn-by-oid! cert oid/x520-locality-name "Boston")
     (set-x509-certificate-dn-by-oid! cert oid/x520-common-name "Test")
     (set-x509-certificate-dn-by-oid! cert oid/x520-organization-name "Guile")
     (set-x509-certificate-dn-by-oid! cert oid/x520-organizational-unit-name "GnuTLS")
     (set-x509-certificate-version! cert 3)
     (set-x509-certificate-serial! cert serial)
     (set-x509-certificate-activation-time! cert activation)
     (set-x509-certificate-expiration-time! cert expiration)
     (set-x509-certificate-key! cert key)
     (set-x509-certificate-key-usage! cert (list key-usage/digital-signature
                                                 key-usage/key-encipherment))
     (set-x509-certificate-subject-key-id! cert (x509-certificate-key-id cert))
     (sign-x509-certificate! cert cert key) ; self-sign
     (and (x509-private-key? key)
          (x509-certificate? cert)
          ;; Validate that setter bindings set the right values and getter
          ;; bindings return the right values.
          (not (x509-certificate-ca-status cert))
          (equal? (x509-certificate-dn cert)
                  "C=US,ST=MA,L=Boston,CN=Test,O=Guile,OU=GnuTLS")
          (= (x509-certificate-version cert) 3)
          (equal? (x509-certificate-serial cert) serial)
          (= (x509-certificate-activation-time cert) activation)
          (= (x509-certificate-expiration-time cert) expiration)
          (lset= eq?
                 (x509-certificate-key-usage cert)
                 (list key-usage/digital-signature
                       key-usage/key-encipherment))
          (equal? (x509-certificate-subject-key-id cert)
                  (x509-certificate-key-id cert))
          ;; Round trip export/import.
          (let ((bv (export-x509-private-key key cert-format)))
            (equal? (export-x509-private-key
                     (import-x509-private-key bv cert-format)
                     cert-format)
                    bv))
          (let ((bv (export-x509-certificate cert cert-format)))
            (equal? (export-x509-certificate
                     (import-x509-certificate bv cert-format)
                     cert-format)
                    bv))))))
