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

(use-modules (rnrs bytevectors) (gnutls))

(when (defined? 'hmac-copy)

  (let ((hash-with-prefix
         (lambda (secret prefix)
           ;; Return a hasher of a string as a 1-argument function,
           ;; by first adding a prefix to it.
           (let ((tag (make-prompt-tag)))
             (call-with-prompt tag
               (lambda ()
                 (let ((state (make-hmac mac/sha256 secret)))
                   (hmac! state prefix)
                   (let ((line (abort-to-prompt tag)))
                     ;; The flow may reenter multiple times here, so
                     ;; we have to copy the hmac state.
                     (let ((copy (hmac-copy state)))
                       (hmac! copy line)
                       (hmac-output copy)))))
               (lambda (k) k))))))

    ;; So if "Prefix " is the prefix, it will be hashed only once.
    (let ((expected-output-1
           (hmac-direct mac/sha256
                        (string->utf8 "secret!")
                        (string->utf8 "Prefix and then some")))
          (expected-output-2
           (hmac-direct mac/sha256
                        (string->utf8 "secret!")
                        (string->utf8 "Prefix and other data"))))
      ;; hasher is a 1-argument function that computes the hash of
      ;; "Prefix " + its argument (as bytevectors), but re-uses the
      ;; state it has after hashing "Prefix ".
      (let ((hasher (hash-with-prefix (string->utf8 "secret!")
                                      (string->utf8 "Prefix "))))
        (let ((output-1 (hasher (string->utf8 "and then some")))
              (output-2 (hasher (string->utf8 "and other data"))))
          (unless (and (equal? output-1 expected-output-1)
                       (equal? output-2 expected-output-2))
            (error "This cannot happen.")))))))
