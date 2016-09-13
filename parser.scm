; code-list is the brainfuck code as a list of characters
; put anything in args to enable debug output
(define (parse code-list . args)
  (define (inner rem jumpbacks left-stack value right-stack)
    (unless (null? rem)
      (when (not (null? args))
        (format #t "~2a ~60a ~5a ~60a\n"
                (car rem)
                (reverse left-stack)
                value
                right-stack))
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
                   (set! value (car left-stack))
                   (set! left-stack (cdr left-stack)))
            ((#\>) (set! left-stack (cons value left-stack))
                   (set! value (car right-stack))
                   (set! right-stack (cdr right-stack)))
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
  (inner code-list '() (make-list 20 0) 0 (make-list 20 0)))


