#!/usr/bin/guile \
-e main -s
!#


(use-modules (ice-9 rdelim))
(load "parser.scm")

; brainfuck program to be read through stdin
; optional cli argument if debug should be shown (anything input)
(define (main args)
  (define prgr (read-line))
  (parse-string prgr (if (null? (cdr args)) #f #t)))
