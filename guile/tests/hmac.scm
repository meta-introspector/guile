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

;; Check the HMAC algorithms.

(use-modules (gnutls)
             (gnutls build tests)
             (rnrs bytevectors)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

(define (hex->bytevector hex)
  (let* ((digits (string->list hex))
         (digits-as-numbers
          (map
           (cute string-index "0123456789abcdef" <>)
           digits))
         (digit-pairs
          (let get ((digits digits-as-numbers))
            (match digits
              (() '())
              ((big little other-digits ...)
               `(,(+ (* big 16) little)
                 ,@(get other-digits)))))))
    (u8-list->bytevector digit-pairs)))

(run-test
 (lambda ()
   (let ((secret (string->utf8 "secret!"))
         (payload (string->utf8 "Example data to hash."))
         (sha256-output
          (hex->bytevector
           "9d40ad76c2b12c5057e9f31bb35a7a382ade69d870e94510eb93dc35b14d262e")))
     (unless (equal? sha256-output (hmac-direct mac/sha256 secret payload))
       (error "The sha256 direct method failed."))
     (let ((state (make-hmac mac/sha256 secret)))
       (hmac! state payload)
       (let ((copy (hmac-copy state)))
         (hmac! copy (string->utf8 " More data."))
         (unless
             (equal?
              (hmac-output copy)
              (hex->bytevector
               "288a7d4b6fddac45a99fbefbfed287ca1c58c6ab011a579f47c6f3313422b39b"))
           (error "The hmac state copy failed.")))
       (unless (equal? (hmac-output state) sha256-output)
         (error "The indirect method failed."))))))
