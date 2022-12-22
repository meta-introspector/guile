(use-modules (ice-9 rdelim) (rnrs bytevectors) (gnutls))

(format #t "What is the secret?\n")

(let ((secret (read-line)))
  (format #t "What message do you want to hash?\n")
  (let ((message (read-line)))
    (format #t "The digest is: ~s\n"
            (hmac-direct mac/sha256
                         (string->utf8 secret)
                         (string->utf8 message)))))
