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

;; Check the asymmetric cryptography implementation.

(use-modules (gnutls)
             (gnutls build tests)
             (rnrs bytevectors)
             (ice-9 match)
             (ice-9 receive)
             (srfi srfi-1)
             (srfi srfi-26))

(define (version>? a b)
  (match `(,a ,b)
         ((() ()) #f)
         ((_ ()) #t)
         ((() _) #f)
         (((leading-a a ...)
           (leading-b b ...))
          (cond
           ((> leading-a leading-b)
            #t)
           ((< leading-a leading-b)
            #f)
           (else
            (version>? a b))))))

(define (version<? a b)
  (version>? b a))

(define (version>=? a b)
  (not (version<? a b)))

(define (check-import-rsa-private-key-opt m e d p q u e1 e2)
  ;; The RSA private key import accepts #f for d only after 3.6.14.
  (receive (at-least-3.7.0? before-3.6.14?)
      (let ((version-digits
             (let ((canonical
                    (match (string-split (gnutls-version) #\-)
                           ((before-dash . _) before-dash))))
               (map string->number (string-split canonical #\.)))))
        (values
         (version>=? version-digits '(3 7 0))
         (version<? version-digits '(3 6 14))))
    ;; In some old versions of gnutls, having exactly u, e1 and e2 missing
    ;; leads to a gnutls internal error exception. So I avoid the situation
    ;; here and either have all optional parameters absent, or all optional
    ;; parameters present.
    (cond
     (at-least-3.7.0?
      (import-raw-rsa-private-key m e #f p q #f #f #f))
     (before-3.6.14?
      (begin
        ;; Importing without d should fail
        (when (false-if-exception
               (import-raw-rsa-private-key m e #f p q #f #f #f))
          (error "this should not work"))
        (import-raw-rsa-private-key m e d p q u e1 e2)))
     (else
      ;; This is a grey zone, maybe we’re before the commit that made d
      ;; optional, maybe we’re after.
      (import-raw-rsa-private-key m e d p q u e1 e2)))))

(run-test
 (lambda ()
   ;; Check that I can export an RSA key and that I can sign with PSS
   (receive (m e d p q u e1 e2)
       (private-key-export-raw-rsa
        (generate-private-key pk-algorithm/rsa 2048))
     (let ((private-key (check-import-rsa-private-key-opt m e d p q u e1 e2))
           (public-key (import-raw-rsa-public-key m e))
           (message (string->utf8 "Hello, world!")))
       (receive (signature pss-signature algo)
           (let ((sign-algo
                  (pk-algorithm->sign-algorithm
                   (private-key-pk-algorithm private-key)
                   digest/sha256)))
             (values
              (private-key-sign-data private-key sign-algo message '())
              (private-key-sign-data private-key sign-algo message
                                     (list privkey/sign-flag-rsa-pss))
              sign-algo))
         (public-key-verify-data public-key algo message signature)
         (public-key-verify-data public-key algo message pss-signature)
         (receive (incorrect-signature incorrect-pss-signature)
             (let ((corrupt
                    (let ((+1/8bits
                           (lambda (number)
                             (remainder (+ 1 number) 256))))
                      (lambda (bv)
                        (u8-list->bytevector
                         (map (lambda (byte) (+1/8bits byte))
                              (bytevector->u8-list bv)))))))
               (values (corrupt signature) (corrupt pss-signature)))
           (when (false-if-exception
                  (public-key-verify-data
                   public-key algo message incorrect-signature))
             (error "This should have failed (I)."))
           (when (false-if-exception
                  (public-key-verify-data
                   public-key algo message incorrect-pss-signature))
             (error "This should have failed (II)."))))))
   ;; This is the example in the manual.
   (receive (curve x y k)
       (let ((raw-output
              (call-with-output-string
               (lambda (port)
                 (with-input-from-port
                     (open-input-string "")
                   (lambda ()
                     (with-output-to-port port
                       (lambda ()
                         (load-from-path "generate-private-key.scm")))))))))
         (match (read (open-input-string raw-output))
                (`((curve . ,curve)
                   (x . ,x)
                   (y . ,y)
                   (k . ,k))
                 (values curve x y k))))
     (let ((input
            (call-with-output-string
             (lambda (port)
               ;; curve:
               (format port "~a\n" curve)
               ;; x:
               (format port "~a\n" x)
               ;; y:
               (format port "~a\n" y)
               ;; k:
               (format port "~a\n" k))))
           (expected-output
            (call-with-output-string
             (lambda (port)
               (format port "curve:\n")
               (format port "x:\n")
               (format port "y:\n")
               (format port "k:\n")
               (format port "I could sign a message with that key.\n")))))
       (let ((true-output
              (call-with-output-string
               (lambda (output-port)
                 (with-input-from-port
                     (open-input-string input)
                   (lambda ()
                     (with-output-to-port output-port
                       (lambda ()
                         (load-from-path "use-private-key.scm")))))))))
         (unless (equal? expected-output true-output)
           (error "The manual example failed.")))))))
