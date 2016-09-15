#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 rdelim))
(load "parser.scm")

(define (parse-args args)
  (define (inner rem parsed)
    (cond
      ((null? rem)
       parsed)
      ((= (length rem) 1)
       (cons `(file . ,(car rem)) parsed))
      ((equal? (car rem) "--debug")
       (inner (cdr rem) (cons '(debug . #t) parsed)))))
  (inner args '()))


(define (main args)
  (let ((parsed-args (parse-args (cdr args))))
    (let ((filename (assoc-ref parsed-args 'file))
          (debug (assoc-ref parsed-args 'debug)))
      (let ((file (open-input-file filename)))
        (parse-string (read-delimited (string #\eot)
                                      file)
                      debug)))))
