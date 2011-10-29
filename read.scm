
(define (read-while pred)
  (let more ((c ""))
    (if (pred (peek-char))
      (more (conc c (read-char)))
      c)))

(define (read-number)
  (string->number (read-while char-numeric?)))

(define (char-symbol? c)
  (or (char-alphabetic? c)
      (not (eq? #f (memq c '(#\+ #\/ #\- #\= #\> #\< #\* #\! #\?))))))

(define (read-symbol)
  (string->symbol (read-while char-symbol?)))

(define (open-paren? c)
  (eq? c #\())

(define (close-paren? c)
  (eq? c #\)))

(define (next-token)
    (let ((next (peek-char)))
      (cond ((eof-object? next) (begin (read-char) '(eof)))
            ((char-whitespace? next) (begin (read-char) (next-token)))
            ((open-paren? next) (begin (read-char) '(open-paren)))
            ((close-paren? next) (begin (read-char) '(close-paren)))
            ((char-numeric? next) (list 'number (read-number)))
            ((char-symbol? next) (list 'symbol (read-symbol)))
            (else (print "Malformed expression")))))

(define (parse-s)
  (let ((token (next-token)))
    (if (eq? (car token) 'close-paren)
      '()
      (cons
        (case (car token)
          ((eof) (error "Unexpected EOF"))
          ((open-paren) (parse-s))
          ((symbol) (cadr token))
          ((number) (cadr token))
          (else (print "Unrecognized token")))
        (parse-s)))))

(define (read- . port)
  (with-input-from-port (optional port (current-input-port))
    (lambda ()
      (if (eq? 'open-paren (car (next-token)))
        (parse-s)
        (print "Not an s-expression.")))))
