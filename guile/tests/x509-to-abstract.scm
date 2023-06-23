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

;; Check that X509 certificates and private keys can be converted to abstract
;; keys.

(use-modules (gnutls)
             (gnutls build tests)
             (rnrs bytevectors)
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-26))

(run-test
 (lambda ()
   ;; Generate a X509 private key
   (receive (crt-private-key certificate)
       ;; Stolen from x509-auth.scm
       (let ((import-key
              (lambda (import-proc file)
                (let* ((path (search-path %load-path file))
                       (size (stat:size (stat path)))
                       (raw  (make-u8vector size)))
                  (uniform-vector-read! raw (open-input-file path))
                  (import-proc raw x509-certificate-format/pem)))))
         (values
          (import-key import-x509-private-key "x509-key.pem")
          (import-key import-x509-certificate "x509-certificate.pem")))
     (receive (key pub)
         (values
          (x509-private-key->private-key crt-private-key '())
          (x509-certificate->public-key certificate))
       (unless (and key pub)
         (error "conversion failed"))))))
