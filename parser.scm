;(define-module (brainfuck parser)
;               #:use-module (ice-9 format)
;               #:export (parse-string))
(use-modules (ice-9 format))

; car with fallback 0
(define (stack-take stack)
  (if (null? stack)
    0
    (car stack)))

; cdr with fallback '()
(define (stack-rest stack)
  (if (null? stack)
    '()
    (cdr stack)))


; code-list is the brainfuck code as a list of characters
; an optional secound argument is a boolean if debug output should be on
(define (parse code-list . args)
  (define (inner rem jumpbacks left-stack value right-stack)
    (unless (null? rem)
      (unless (null? args)
        (when (car args)
          (format #t "~2a ~60a ~5a ~60a\n"
                  (car rem)
                  (reverse left-stack)
                  value
                  right-stack)))
      (let ((object (car rem)))
        (let ((rem (cdr rem))
              (jumpbacks jumpbacks)
              (left-stack left-stack)
              (value value)
              (right-stack right-stack))
          (case object
            ((#\+) (set! value (1+ value)))
            ((#\-) (set! value (1- value)))
            ((#\<) (set! right-stack (cons value right-stack))
                   (set! value (stack-take left-stack))
                   (set! left-stack (stack-rest left-stack)))
            ((#\>) (set! left-stack (cons value left-stack))
                   (set! value (stack-take right-stack))
                   (set! right-stack (stack-rest right-stack)))
            ((#\.) (display (integer->char value)))
            ((#\,) (set! value (read-char)))
            ((#\[) (let ((items (call/cc
                                  (lambda (cont)
                                    (set! jumpbacks (cons cont jumpbacks))
                                    (cont (list value
                                                left-stack
                                                right-stack))))))
                     (set! value (car items))
                     (set! left-stack (cadr items))
                     (set! right-stack (caddr items))))
            ((#\]) (if (not (zero? value))
                     ((car jumpbacks) (list value
                                            left-stack
                                            right-stack))
                     (set! jumpbacks (cdr jumpbacks)))))
          (inner rem jumpbacks left-stack value right-stack)))))
  (inner code-list '() '() 0 '()))

(define (parse-string str . args)
  (parse (string->list str) (if (null? args) #f (car args))))
