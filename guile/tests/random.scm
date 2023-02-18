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

;; Check the random number generation API.

(use-modules (gnutls)
             (ice-9 match)
             (gnutls build tests))

(run-test
 (lambda ()
   (let ((output
          (call-with-output-string
            (lambda (port)
              (with-output-to-port port
                (lambda ()
                  (load-from-path "random-example.scm")))))))
     (match output
       ((or "You roll a 1.\n"
            "You roll a 2.\n"
            "You roll a 3.\n"
            "You roll a 4.\n"
            "You roll a 5.\n"
            "You roll a 6.\n")
        #t)
       (otherwise
        (error "Invalid roll."))))))
