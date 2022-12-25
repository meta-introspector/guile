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

;; Check the hash algorithm implementation.

(use-modules (gnutls)
             (gnutls build tests)
             (rnrs bytevectors)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

(run-test
 (lambda ()
   ;; This checks that the direct and indirect hash methods give the same
   ;; results.
   (let ((payload (string->utf8 "Example data to hash.")))
     (let ((direct-method (hash-direct digest/sha256 payload))
           (indirect-method
            (let ((state (make-hash digest/sha256)))
              (hash! state payload)
              (when (defined? 'hash-copy)
                (let ((copy (hash-copy state)))
                  (hash! copy (string->utf8 " More data."))
                  (unless
                      (equal?
                       (hash-output copy)
                       #vu8(163 1 149 172 97 42 249 15 132 114 243 203 145
                                67 18 186 127 135 127 158 131 43 244 38 23
                                46 156 254 193 225 201 171))
                    (error "The hash state copy failed."))))
              (hash-output state))))
       (unless (equal? direct-method indirect-method)
         (error "The direct and indirect hash methods do not give the same result."))))
   ;; This is the example in the manual.
   (let ((manual-example
          (lambda ()
            (load-from-path "hash-example.scm")))
         ;; This is how we run the example for the purpose of testing the
         ;; code.
         (manual-example-environment
          (lambda (f)
            (call-with-output-file "hash-manual-example-data"
              (lambda (port)
                (format port "Example data to hash."))
              #:encoding "UTF-8")
            (let* ((input (call-with-output-string
                            (lambda (port)
                              ;; Which file do you want to hash?
                              (format port "./hash-manual-example-data\n"))))
                   (expected-output
                    (call-with-output-string
                      (lambda (port)
                        (format port "Which file do you want to hash?\n")
                        ;; we tell it the file
                        (format port "The digest is: ~s\n"
                                #vu8(87 122 203 105 209 216 209 172 80 105
                                        249 68 57 176 185 62 230 184 16 225
                                        123 155 151 181 137 113 249 49 37
                                        135 34 234)))))
                   (true-output
                    (call-with-output-string
                      (lambda (output-port)
                        (with-input-from-port
                            (open-input-string input)
                          (lambda ()
                            (with-output-to-port output-port
                              f)))))))
              (delete-file "hash-manual-example-data")
              (unless (equal? expected-output true-output)
                (error "Manual example for hash failed."))))))
     (manual-example-environment manual-example))))
