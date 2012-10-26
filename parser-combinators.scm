(declare
  (unit parser-combinators)
  (uses srfi-1 srfi-69))

(use srfi-41)

(define (any-char s)
  (if (and (stream-occupied? s)
            (char? (stream-car s)))
    (values (stream-car s) (stream-cdr s))
    (values #f #f)))

(define (all-of . tests)
  (lambda (s)
    (let loop [(c #f)
               (cs s)
               (tests tests)]
      (if (null? tests)
        (values c cs)
        (let-values [((c cs) ((car tests) s))]
            (if (not cs)
              (values #f #f)
              (loop c cs (cdr tests))))))))

(define (one-of . tests)
  (lambda (s)
    (let loop [(tests tests)]
      (if (null? tests)
        (values #f #f)
        (let-values [((c cs) ((car tests) s))]
            (if (not cs)
              (loop (cdr tests))
              (values c cs)))))))

(define (char-test t?)
  (all-of
    any-char
    (lambda (s)
      (let ((c (stream-car s)))
        (if (t? c)
          (values c (stream-cdr s))
          (values #f #f))))))

(define numeric (char-test char-numeric?))
(define alpha (char-test char-alphabetic?))
(define alpha-numeric (one-of alpha numeric))

(define (seq . ps)
  (lambda (s)
    (let loop [(acc '())
               (cs s)
               (parsers ps)]
      (if (null? parsers)
        (values (reverse acc) cs)
        (let-values [((c cs) ((car parsers) cs))]
            (if (not cs)
              (values #f #f)
              (loop (cons c acc) cs (cdr parsers))))))))

(define (zero-many test)
  (lambda (s)
    (let loop [(acc '())
               (s s)]
      (let-values [((i cs) (test s))]
          (if (not cs)
            (values (reverse acc) s)
            (loop (cons i acc) cs))))))

(define (map* f parser)
  (lambda (s)
    (let-values [((v cs) (parser s))]
        (if (not cs)
          (values v cs)
          (values (f v) cs)))))

(define (one-many test)
    (map* (lambda (xs) (cons (car xs) (cadr xs)))
          (seq test (zero-many test))))

(define whitespace (char-test (lambda (c) (and (char? c) (char-whitespace? c)))))

(define (ltrim test)
  (map* cadr (seq (zero-many whitespace) test))) 

(define to-integer (compose string->number list->string))

(define (char= c) (char-test (lambda (v) (char=? v c))))
(define integer (map* to-integer (ltrim (one-many numeric))))

(define (char-in-list lst) (char-test (lambda (c) (memq c lst))))

(define open-paren (char= #\())
(define close-paren (char= #\)))

(define open-bracket (char= #\[))
(define close-bracket (char= #\]))

(define open-curly (char= #\{))
(define close-curly (char= #\}))

(define double-quote (char= #\"))
(define single-quote (char= #\'))

(define (no test)
  (lambda (s)
    (let-values [((v cs) (test s))]
        (if (not cs)
          (values #t s)
          (values #f #f)))))

(define symbol-special (char-in-list '(#\+ #\/ #\- #\= #\> #\< #\* #\! #\?)))

(define symbol
  (map* (compose string->symbol list->string flatten)
        (ltrim (seq (one-of alpha symbol-special)
                    (zero-many (one-of alpha-numeric symbol-special))))))

(define string-chars (all-of (no double-quote) any-char))

(define str
  (map* (compose list->string cadr)
        (ltrim (seq double-quote (zero-many string-chars) double-quote))))

(define-syntax delayed
  (syntax-rules ()
      ((_ test)
       (lambda (s) (test s)))))

(define expr (one-of symbol integer str (delayed list*) (delayed vector*) (delayed hash-map*)))

(define list*
  (map* cadr
        (seq (ltrim open-paren)
             (one-many expr)
             (ltrim close-paren))))

(define vector*
  (map* (lambda (xs) (apply vector (cadr xs)))
        (seq (ltrim open-bracket)
             (zero-many expr)
             (ltrim close-bracket))))

(define hash-map*
  (map* (compose alist->hash-table cadr)
        (seq (ltrim open-curly)
             (zero-many (seq expr expr))
             (ltrim close-curly))))

