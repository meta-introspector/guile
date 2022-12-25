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
