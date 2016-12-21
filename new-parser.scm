(use-modules (ice-9 streams)
             (ice-9 match))

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

;; Evaluate body with the items from state bound
(define-macro (change state body)
              `(match ,state
                     ((jumps value left right)
                      ,body)))

(define (parse-brainfuck source-stream)
  (stream-fold (lambda (object state)
                 ;(display state) (newline)
                 (if (number? (car state))
                   (case object
                     ((#\[) (cons (1+ (car state)) (cdr state)))
                     ((#\]) (if (zero? (car state))
                              (cdr state)
                              (cons (1- (car state)) (cdr state))))
                     (else state))
                   (change state
                           (case object
                             ((#\+) (list jumps (1+ value) left right))
                             ((#\-) (list jumps (1- value) left right))
                             ((#\<) (list jumps
                                          (stack-take left)
                                          (stack-rest left)
                                          (cons value right)))
                             ((#\>) (list jumps
                                          (stack-take left)
                                          (cons value left)
                                          (stack-rest right)))
                             ((#\.) (begin (display (integer->char (modulo value #xFF)))
                                           state))
                             ((#\,) (list jumps
                                          (char->integer (read-char))
                                          left
                                          right))
                             ((#\[) (if (zero? value)
                                      (cons 0 state)
                                      (let* ((kont #f)
                                             (rest (call/cc (lambda (cont)
                                                              (set! kont cont)
                                                              (list value left right)))))
                                        (cons (cons kont jumps) rest))))
                             ((#\]) (if (not (zero? value))
                                      ((car jumps)
                                       (list value left right))
                                      (list (cdr jumps) value left right)))
                             (else state)))))
               '(() 0 () ())
               source-stream))

(define (parse-brainfuck-file file-name)
  (parse-brainfuck (port->stream (open-input-file file-name)
                                 read-char)))

(define (parse-string string)
  (parse-brainfuck (list->stream (string->list string))))
