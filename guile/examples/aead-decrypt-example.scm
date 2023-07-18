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

(use-modules (ice-9 rdelim)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (gnutls))

(format #t "What is the secret?\n")

(let ((secret (read-line)))
  (set! secret
        (string->utf8 secret))
  (unless (equal? (bytevector-length secret)
                  (cipher-key-size cipher/aes-256-gcm))
    (error "incorrect key length"))
  (format #t "Which file do you want to decrypt?\n")
  (let ((file-name (read-line)))
    ;; Create a new state that will be reused when new bytes are
    ;; available.
    (let ((cipher (make-aead-cipher cipher/aes-256-gcm secret)))
      (call-with-output-file (string-append file-name ".decrypted~")
        (lambda (out)
          (call-with-input-file file-name
            (lambda (in)
              ;; Read raw bytes from the file.
              (let do-decrypt ()
                (let ((next (get-bytevector-some in)))
                  (unless (eof-object? next)
                    (let ((decrypted
                           (aead-cipher-decrypt
                            cipher
                            ;; The same value as used at encryption time:
                            (string->utf8 "12 randbytes")
                            (string->utf8 "Additional secret data")
                            0
                            next)))
                      (put-bytevector out decrypted)
                      (do-decrypt))))))
            #:binary #t))
        #:binary #t))
    (rename-file (string-append file-name ".decrypted~")
                 (string-append file-name ".decrypted"))))
