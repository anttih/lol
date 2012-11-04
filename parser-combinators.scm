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

(define (char-in-list lst) (char-test (lambda (c) (memq c lst))))
(define (char= c) (char-test (lambda (v) (char=? v c))))
(define (not-char c) (char-test (lambda (v) (not (char=? v c)))))
(define whitespace (char-test (lambda (c) (and (char? c) (char-whitespace? c)))))

(define semicolon (char= #\;))

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


(define comment (seq semicolon (zero-many (not-char #\newline)) (char= #\newline)))

(define (ltrim test)
  (map* cadr (seq (zero-many whitespace) test))) 

(define symbol-special (char-in-list '(#\+ #\/ #\- #\= #\> #\< #\* #\! #\?)))

(define symbol-seq
  (map* flatten
        (seq (one-of alpha symbol-special)
             (zero-many (one-of alpha-numeric symbol-special)))))

(define (tokenify type)
  (lambda (v) (cons type v)))

(define t-symbol
  (map* (compose (tokenify 'symbol) string->symbol list->string)
        symbol-seq))

(define t-integer
  (map* (compose (tokenify 'integer) string->number list->string)
        (one-many numeric)))

(define string-chars (not-char #\"))

(define t-string
  (map* (compose (tokenify 'string) list->string cadr)
        (seq (char= #\") (zero-many string-chars) (char= #\"))))

(define t-keyword
  (map* (compose (tokenify 'keyword) string->keyword list->string flatten cdr)
        (seq (char= #\:) symbol-seq)))

(define t-open-paren (map* (tokenify 'open-paren) (char= #\()))
(define t-close-paren (map* (tokenify 'close-paren) (char= #\))))

(define t-open-bracket (map* (tokenify 'open-bracket) (char= #\[)))
(define t-close-bracket (map* (tokenify 'close-bracket) (char= #\])))

(define t-open-curly (map* (tokenify 'open-curly) (char= #\{)))
(define t-close-curly (map* (tokenify 'close-curly) (char= #\})))

(define whitespace-or-comment
  (zero-many (one-of (one-many whitespace) comment)))

(define (token . parsers)
  (map* cadr
        (seq whitespace-or-comment
             (apply one-of parsers))))

(define next-token
    (token t-symbol
           t-integer
           t-string
           t-keyword
           t-open-paren
           t-close-paren
           t-open-bracket
           t-close-bracket
           t-open-curly
           t-close-curly))

(define stream->token-stream
  (stream-lambda (s)
    (let-values [((v cs) (next-token s))]
        (if (not cs)
          stream-null
          (stream-cons v (stream->token-stream cs))))))

;;
;; higher order parsers
;;

(define (any-token s)
  (if (and (stream-occupied? s)
           (pair? (stream-car s)))
    (values (stream-car s) (stream-cdr s))
    (values #f #f)))

(define (token-type type)
  (all-of any-token
          (lambda (s)
            (let [(t (stream-car s))]
              (if (eq? (car t) type)
                (values t (stream-cdr s))
                (values #f #f))))))

(define (match-token type)
  (map* cdr (token-type type)))

(define symbol (match-token 'symbol))
(define keyword (match-token 'keyword))
(define str (match-token 'string))
(define integer (match-token 'integer))

(define atomic (one-of symbol keyword str integer))

(define-syntax delayed
  (syntax-rules ()
      ((_ test)
       (lambda (s) (test s)))))

(define (set-error parser msg)
  (lambda (s)
    (if (stream-null? s)
      (values #f #f)
      (let-values [((v cs) (parser s))]
          (if (not cs)
            (values (format msg (cdr (stream-car s))) #f)
            (values v cs))))))

(define expr
  (set-error (one-of atomic
                     (delayed list*)
                     (delayed vector*)
                     (delayed hash-map*))
             "Error: unexpected token: ~A"))

(define list*
  (map* cadr
        (seq (match-token 'open-paren)
             (zero-many expr)
             (match-token 'close-paren))))

(define vector*
  (map* (lambda (xs) (apply vector (cadr xs)))
        (seq (match-token 'open-bracket)
             (zero-many expr)
             (match-token 'close-bracket))))

(define (pair->dotted l)
	(map (lambda (x) `(,(car x) . ,(cadr x))) l))

(define hash-map*
  (map* (compose alist->hash-table cadr)
        (seq (match-token 'open-curly)
             (map* pair->dotted (zero-many (seq expr expr)))
             (match-token 'close-curly))))

