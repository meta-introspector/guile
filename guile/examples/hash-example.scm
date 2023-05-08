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
(format #t "Which file do you want to hash?\n")

(let ((file-name (read-line)))
  ;; Create a new state that will be reused when new bytes are
  ;; available.
  (let ((state (make-hash digest/sha256)))
    (call-with-input-file file-name
      (lambda (port)
        (let hash-all ()
          ;; Read raw bytes from the file.
          (let ((next (get-bytevector-some port)))
            (if (eof-object? next)
                ;; No more data in the file
                (format #t "The digest is: ~s\n"
                        (hash-output state))
                (begin
                  ;; Hash the bytes we got, and continue.
                  (hash! state next)
                  (hash-all))))))
      #:binary #t)))
