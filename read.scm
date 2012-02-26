(declare
  (unit reader)
  (uses ports data-structures srfi-69))

(define (read-while pred)
  (let more ((a ""))
    (let ((c (peek-char)))
      (if (and (not (eof-object? c))
               (pred c))
        (more (conc a (read-char)))
        a))))

(define (read-number)
  (string->number (read-while char-numeric?)))

(define (char-doublequote? c) (eq? c #\"))

(define (read-str)
  (let ((str (read-while (compose not char-doublequote?))))
    (read-char)
    str))

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
            ((char-doublequote? next) (begin (read-char) (list 'string (read-str))))
            ((char-symbol? next) (list 'symbol (read-symbol)))
            (else (print "Malformed expression")))))

(define (read-s close)
  (let ((token (read-next)))
    (cond ((eq? token 'eof) (error "Unexpected EOF"))
          ((eq? token close) '())
          (else (cons token
                      (read-s close))))))

(define (read-alist close)
  (let next ((s (read-s close)))
	(if (null? s)
	  '()
	  (cons (cons (car s) (cadr s))
			(next (cddr s))))))

(define (atomic? s)
  (or (eq? s 'keyword)
      (eq? s 'symbol)
      (eq? s 'number)
      (eq? s 'string)))

(define (close-s? s)
    (or (eq? s 'close-paren)
        (eq? s 'close-curly)
        (eq? s 'close-bracket)))

(define (read-next)
  (let* ((token (next-token))
         (tag (car token)))
    (cond ((eq? tag 'eof) #!eof)
          ((atomic? tag) (cadr token))
          ((eq? tag 'open-paren) (read-s 'close-paren))
          ((eq? tag 'open-curly) (alist->hash-table (read-alist 'close-curly)))
          ((eq? tag 'open-bracket) (apply vector (read-s 'close-bracket)))
          ((close-s? tag) tag)
          (else (print "Unrecognized token")))))

(define (read- . port)
  (with-input-from-port (optional port (current-input-port))
    (lambda () (read-next))))
