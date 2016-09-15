#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 rdelim))
(load "parser.scm")

;; TODO reenable access to debug output
(define (main args)
  (let ((file (open-input-file (cadr args))))
    (parse-string (read-delimited (string #\eot) file))))
