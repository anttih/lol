
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

(define (char-colon? c)
  (eq? c #\:))

(define (read-symbol)
  (string->symbol (read-while char-symbol?)))

(define (read-keyword)
  (string->keyword (read-while char-symbol?)))

(define (open-paren? c) (eq? c #\())
(define (close-paren? c) (eq? c #\)))

(define (open-curly? c) (eq? c #\{))
(define (close-curly? c) (eq? c #\}))

(define (open-bracket? c) (eq? c #\[))
(define (close-bracket? c) (eq? c #\]))

(define (next-token)
    (let ((next (peek-char)))
      (cond ((eof-object? next) (begin (read-char) '(eof)))
            ((char-whitespace? next) (begin (read-char) (next-token)))
            ((open-paren? next) (begin (read-char) '(open-paren)))
            ((close-paren? next) (begin (read-char) '(close-paren)))
            ((open-curly? next) (begin (read-char) '(open-curly)))
            ((close-curly? next) (begin (read-char) '(close-curly)))
            ((open-bracket? next) (begin (read-char) '(open-bracket)))
            ((close-bracket? next) (begin (read-char) '(close-bracket)))
            ((char-colon? next) (begin (read-char) (list 'keyword (read-keyword))))
            ((char-numeric? next) (list 'number (read-number)))
            ((char-symbol? next) (list 'symbol (read-symbol)))
            (else (print "Malformed expression")))))

(define (parse-s close)
  (let ((token (next-token)))
    (if (eq? (car token) close)
      '()
      (cons
        (case (car token)
          ((eof) (error "Unexpected EOF"))
          ((open-paren) (parse-s 'close-paren))
		  ((open-curly) (cons 'hash-table (parse-s 'close-curly)))
		  ((open-bracket) (cons 'vector (parse-s 'close-bracket)))
          ((keyword) (cadr token))
          ((symbol) (cadr token))
          ((number) (cadr token))
          (else (print "Unrecognized token")))
        (parse-s close)))))

(define (read- . port)
  (with-input-from-port (optional port (current-input-port))
    (lambda ()
	  (let ((token (car (next-token))))
		(case token
		  ((open-paren) (parse-s 'close-paren))
		  ((eof) #!eof)
		  (else (error "Not an s-expression")))))))
