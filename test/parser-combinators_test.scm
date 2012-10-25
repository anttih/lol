(require-extension test)
(include "parser-combinators")

(define (string->stream s) (list->stream (string->list s)))

(define-syntax test-parser
  (syntax-rules ()
    ((_ name expected expr)
     (test name expected (let-values [((v s) expr)]
                                     v)))))

(test-parser "one numeric char" #\1 (numeric (string->stream "1")))
(test-parser "not a numeric" #f (numeric (string->stream "a")))

(test-parser "numeric and 5" #\5 ((all-of numeric (char-test (lambda (v) (eq? v #\5))))
                                  (string->stream "5")))

(test-parser "one-many: just one" '(#\a) ((one-many alpha) (string->stream "a1")))
(test-parser "one-many: two" '(#\a #\b) ((one-many alpha) (string->stream "ab")))

(test-parser "alpha when alpha or numeric"
             #\a
             ((one-of alpha numeric) (string->stream "a")))

(test-parser "numeric when alpha or numeric"
             #\5
             ((one-of alpha numeric) (string->stream "5a")))

(test-parser "zero chars"
             '()
             ((zero-many alpha) (string->stream "")))

(test-parser "zero or many numeric"
             '(#\1 #\2)
             ((zero-many numeric) (string->stream "12")))

(test-parser "no returns true for all matching"
             #t
             ((no numeric) (string->stream "a")))

(test-parser "integer" 42 (integer (string->stream "42")))
(test-parser "integer ignores following" 42 (integer (string->stream "42 asdf")))

(test-parser "sequence of two numbers"
             '(#\a #\b)
             ((seq alpha alpha) (string->stream "ab")))

(test-parser "integer terminated by space"
             12
             ((terminated-with (char= #\space) integer) (string->stream "12 ")))

(test-parser "one alpha is a symbol" 'a (symbol (string->stream "a")))
(test-parser "symbols: special chars" '+ (symbol (string->stream "+")))

(test-parser "string with alphas" "hello" (str (string->stream "\"hello\"")))
(test-parser "string with numeric" "42" (str (string->stream "\"42\"")))
(test-parser "string with newline" "hello\nworld" (str (string->stream "\"hello\nworld\"")))

(test-parser "empty list is illegal" #f (list* (string->stream "()")))
(test-parser "list with one symbol" '(hello) (list* (string->stream "(hello)")))
(test-parser "list ignores whitespace" '(hello) (list* (string->stream "(  hello  )")))
(test-parser "list with a string" '("string") (list* (string->stream "(\"string\")")))
(test-parser "list with an integer" '(42) (list* (string->stream "(42)")))
(test-parser "list with all datatypes"
             '(hello "string" 567)
             (list* (string->stream "(hello \"string\" 567)")))

(test-parser "nested list"
             '(hello (world))
             (list* (string->stream "(hello (world))")))

(test-parser "complex structure"
             '((hello world) (+ 12 34) (something "else"))
             (list* (string->stream "((hello world) (+ 12 34) (something \"else\"))")))
