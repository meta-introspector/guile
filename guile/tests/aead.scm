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

;; Check the aead implementation.

(use-modules (gnutls)
             (gnutls build tests)
             (rnrs bytevectors)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

(define confidential-data
  "Confidential data.")

(run-test
 (lambda ()
   ;; This is the example in the manual.
   (let ((encrypt-example
          (lambda ()
            (load-from-path "aead-encrypt-example.scm")))
         (decrypt-example
          (lambda ()
            (load-from-path "aead-decrypt-example.scm"))))
     (call-with-output-file "aead-manual-example-data"
       (lambda (port)
         (format port "~a" confidential-data))
       #:encoding "UTF-8")
     (let ((encrypt-input
            (call-with-output-string
              (lambda (port)
                ;; What is the secret?
                (format port "the secret key is 32 bytes long.\n")
                ;; Which file do you want to encrypt?
                (format port "./aead-manual-example-data"))))
           (expected-encrypt-output
            (call-with-output-string
              (lambda (port)
                (format port "What is the secret?\n")
                ;; the secret key is 32 bytes long.
                (format port "Which file do you want to encrypt?\n")
                ;; ./aead-manual-example-data
                ))))
       (let ((true-encrypt-output
              (call-with-output-string
                (lambda (output-port)
                  (with-input-from-port (open-input-string encrypt-input)
                    (lambda ()
                      (with-output-to-port output-port
                        encrypt-example)))))))
         (unless (equal? expected-encrypt-output true-encrypt-output)
           (error "Encryption failed."))))
     (delete-file "./aead-manual-example-data")
     (let ((decrypt-input
            (call-with-output-string
              (lambda (port)
                ;; What is the secret?
                (format port "the secret key is 32 bytes long.\n")
                ;; Which file do you want to decrypt?
                (format port "./aead-manual-example-data.encrypted"))))
           (expected-decrypt-output
            (call-with-output-string
              (lambda (port)
                (format port "What is the secret?\n")
                ;; the secret key is 32 bytes long.
                (format port "Which file do you want to decrypt?\n")
                ;; ./aead-manual-example-data.encrypted
                ))))
       (let ((true-decrypt-output
              (call-with-output-string
                (lambda (output-port)
                  (with-input-from-port (open-input-string decrypt-input)
                    (lambda ()
                      (with-output-to-port output-port
                        decrypt-example)))))))
         (unless (equal? expected-decrypt-output true-decrypt-output)
           (error "Decryption failed."))))
     (delete-file "./aead-manual-example-data.encrypted")
     (call-with-input-file "./aead-manual-example-data.encrypted.decrypted"
       (lambda (decrypted-file)
         (let ((content (read-line decrypted-file)))
           (unless (equal? content confidential-data)
             (error "Inconsistent encryption and decryption."))))
       #:encoding "UTF-8")
     (delete-file "./aead-manual-example-data.encrypted.decrypted"))))
