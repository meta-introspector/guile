(use-modules (ice-9 rdelim)
             (ice-9 binary-ports)
             (rnrs bytevectors)
             (gnutls))

(format #t "What is the secret?\n")

(let ((secret (read-line)))
  (format #t "Which file do you want to hash?\n")
  (let ((file-name (read-line)))
    ;; Create a new state that will be reused when new bytes are
    ;; available.
    (let ((state (make-hmac mac/sha256 (string->utf8 secret))))
      (call-with-input-file file-name
        (lambda (port)
          (let hash-all ()
            ;; Read raw bytes from the file.
            (let ((next (get-bytevector-some port)))
              (if (eof-object? next)
                  ;; No more data in the file
                  (format #t "The digest is: ~s\n"
                          (hmac-output state))
                  (begin
                    ;; Hash the bytes we got, and continue.
                    (hmac! state next)
                    (hash-all))))))
        #:binary #t))))
