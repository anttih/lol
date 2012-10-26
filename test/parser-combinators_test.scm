(require-extension test)
(include "parser-combinators")

(define (string->stream s) (list->stream (string->list s)))

(define-syntax test-parser
  (syntax-rules ()
    ((_ name expected expr)
     (test name expected (let-values [((v s) expr)]
                                     v)))))

(define (test-match name expected parser input)
  (let-values [((v s) (parser (string->stream input)))]
      (test name expected v)))

(define (test-nomatch name parser input)
  (let-values [((v s) (parser (string->stream input)))]
      (test-assert name (not s))))

(test-group "numeric"
  (test-match "one numeric char" #\1 numeric "1")
  (test-nomatch "character is not numeric" numeric "a"))

(test-group "alpha"
  (test-match "matches alpha" #\a alpha "a")
  (test-nomatch "number is not alpha" alpha "5")
  (test-nomatch "whitespace is not alpha" alpha "   "))

(test-group "one-of (or)"
  (test-match "first matches" #\a (one-of alpha numeric) "a")
  (test-match "second matches" #\5 (one-of alpha numeric) "5"))

(test-group "all-of (and)"
  (test-nomatch "all need to match" (all-of numeric (char= #\5)) "6"))

(test-group "one-many"
  (test-match "just one" '(#\a) (one-many alpha) "a1")
  (test-match "two" '(#\a #\b) (one-many alpha) "ab"))


(test-group "zero-many"
  (test-match "matches empty input" '() (zero-many alpha) "")
  (test-match "matches many" '(#\1 #\2 #\3) (zero-many numeric) "123"))

(test-parser "no returns true for all matching"
             #t
             ((no numeric) (string->stream "a")))

(test-parser "keywords: colon followed by symbol chars"
             symbol-+:
             (keyword (string->stream ":symbol-+")))

(test-parser "integer" 42 (integer (string->stream "42")))
(test-parser "integer ignores following" 42 (integer (string->stream "42 asdf")))

(test-parser "sequence of two numbers"
             '(#\a #\b)
             ((seq alpha alpha) (string->stream "ab")))

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
             '((hello world:) (+ 12 34) (something "else"))
             (list* (string->stream "((hello :world) (+ 12 34) (something \"else\"))")))

(test-exit)
