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

(define (eat-rem rem)
  (let inner ((rem rem)
              (depth 0))
    (if (eqv? (car rem) #\])
      (if (zero? depth)
        (cdr rem)
        (inner (cdr rem) (1- depth)))
      (inner (cdr rem)
             ((if (eqv? (car rem) #\[)
                1+
                +)
              depth)))))

; returns a function which takes the same number of argumentns
;   as the number of variables sent to this function
; Then uses the input to that function to set the
;   variables that was input to the macro
(define-macro (set-multiple! . variables)
  (let ((inputs (map (lambda (fun-name)
                       (symbol-append 'in- fun-name))
                     variables)))
    (let ((body (map (lambda (fun-name in-name)
                       `(set! ,fun-name ,in-name))
                     variables
                     inputs)))
      (display (cons* 'lambda inputs body))
      (cons* 'lambda inputs body))))

; code-list is the brainfuck code as a list of characters
; an optional secound argument is a boolean if debug output should be on
(define (parse code-list . args)
  (define (inner rem jumpbacks left-stack value right-stack)
    (unless (null? rem)
      (unless (null? args)
        (when (car args)
          (format #t "~2a ~37a ~4a ~37a\n"
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
            ((#\[) (if (zero? value)
                     ;; Remove items until matching end bracket is found
                     (set! rem (eat-rem rem))
                     (call-with-values
                       (lambda ()
                         (call/cc
                           (lambda (cont)
                             (set! jumpbacks (cons cont jumpbacks))
                             (cont value
                                   left-stack
                                   right-stack))))
                       (set-multiple! value left-stack right-stack))))
            ((#\]) (if (not (zero? value))
                     ((car jumpbacks) value
                                      left-stack
                                      right-stack)
                     (set! jumpbacks (cdr jumpbacks)))))
          (inner rem jumpbacks left-stack value right-stack)))))
  (inner code-list '() '() 0 '()))

(define (parse-string str . args)
  (parse (string->list str) (if (null? args) #f (car args))))
