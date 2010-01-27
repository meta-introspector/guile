;;; Help produce Guile wrappers for GnuTLS types.
;;;
;;; GNUTLS --- Guile bindings for GnuTLS.
;;; Copyright (C) 2007  Free Software Foundation, Inc.
;;;
;;; GNUTLS is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; GNUTLS is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with GNUTLS; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

;;; Written by Ludovic Court�s <ludo@chbouib.org>.


(use-modules (gnutls build smobs))


;;;
;;; The program.
;;;

(define (main . args)
  (define extra? (not (null? args)))

  (let ((port (current-output-port))
        (enums (if (not extra?)
                   %gnutls-smobs
                   %gnutls-extra-smobs)))
    (format port "/* Automatically generated, do not edit.  */~%~%")
    (format port "#ifndef GUILE_GNUTLS_~aSMOBS_H~%"
            (if extra? "EXTRA_" ""))
    (format port "#define GUILE_GNUTLS_~aSMOBS_H~%"
            (if extra? "EXTRA_" ""))
    (for-each (lambda (type)
                (output-smob-type-declaration type port)
                (output-c->smob-converter type port)
                (output-smob->c-converter type port))
              enums)
    (format port "#endif~%")))

(apply main (cdr (command-line)))

;;; Local Variables:
;;; mode: scheme
;;; coding: latin-1
;;; End:

;;; arch-tag: 7ae9c82f-a423-4251-9a58-6e2581267567
