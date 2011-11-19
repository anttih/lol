(require-extension test)
(use srfi-1 srfi-69)
(include "eval")
(include "pretty")

; lambda
(test "lambda with empty param list" #t (lambda? '(fn () 1)))
(test "lambda with a thunk" #t (lambda? '(fn () (a 1) (b 2))))
(test "lambda has no params"
      '()
      (lambda-params '(fn () (hello))))
(test "lambda has one param"
      '(name)
      (lambda-params '(fn (name) (hello))))
(test "lambda has a body sequence"
      '(do (hello) (world))
      (lambda-sequence '(fn () (hello) (world))))


(test "special form def"
      #t
      (definition? '(def a 1)))

(test "define in frame"
      '((b a) . (2 1))
      (frame-bind '((a) . (1)) 'b 2))

(test "define variable"
      '(((new) 2))
      (let ((env (make-environment '() '())))
          (define-variable! env 'new 2)
          env))

(test "lookup variable value in frame"
      2
      (lookup-variable-value '((a b) . (2 3)) 'a))

(test "lookup variable value in frame not found"
        #f
        (lookup-variable-value '((a b) . (2 3)) 'c))

(test "lookup compound procedure value in a frame"
      '(procedure '())
      (lookup-variable-value '((name) (procedure '())) 'name))

(define test-env '(((a b) . (2 3)) ((b c) . (4 5))))

(test "extend environment"
      '(((name) value) ((name1) value1))
      (extend-env (make-environment '(name1) '(value1)) '(name) '(value)))

(test "lookup variable in environment" 2 (lookup-variable test-env 'a))
(test "shadow value" 3 (lookup-variable test-env 'b))

(define empty-env (make-environment '() '()))
(define (evaluate* e)
  (evaluate e (make-environment '(+ - / *) (list + - / *))))

(test "simple arithmetic" 2 (evaluate* '(+ 1 1)))
(test "nested arithmetic" 5 (evaluate* '(+ (* 2 2) 1)))
(test "variables"
      4
      (begin
          (evaluate* '(do (def a 3)
                            (+ a 1)))))

(test "definition-name"
      'name
      (definition-name '(def name 1)))

(test "definition-name returns car when param list present"
      'name
      (definition-name '(def (name) 1)))

(test "definition-value returns proc with no args when spec is list with just car"
      '(procedure (env) () (do something awesome))
      (definition-value '(def (name) something awesome) '(env)))

(test "definition-value returns proc with params when spec is list with cdr"
      '(procedure (env) (arg) (do hello world))
      (definition-value '(def (name arg) hello world) '(env)))

(test "define and apply lambda with no params"
      42
      (evaluate* '(do
                   (def test (fn () 42))
                   (test))))

(test "define lambda"
      16
      (evaluate* '(do
                   (def square (fn (x) (* x x)))
                   (square 4))))
                    
(test "apply lambda immediately"
      1
      (evaluate* '((fn (x) x) 1)))

;; Pretty printing
(test "strings are quoted" "\"hello\"" (pretty "hello"))
(test "numbers are just numbers" "42" (pretty 42))
(test "symbols are not quoted" "symbol" (pretty 'symbol))
(test "quoted form prints a list of symbols" "(name 1)" (pretty '(quote name 1)))
(test "compound procedures print a special string"
      "(#<compound-procedure>)"
      (let ((env (make-environment '() '())))
        (evaluate '(def my (fn (x) x)) env)
        (pretty (frame-values (car env)))))

;; if
(test "evaluates consequence when true" 1 (evaluate* '(if true 1)))
(test "evaluates alternate when false" 2 (evaluate* '(if false 1 2)))

;; cond
(test "expands one cond"
      '(if a b false)
      (expand-cond '(cond a b)))

(test "expands two conditions"
      '(if a b (if c d false))
      (expand-cond '(cond a b c d)))

(test "expands cond with else"
      '(if a b other)
      (expand-cond '(cond a b else other)))

;; let
(test "let with one param" 1 (evaluate* '(let (x 1) x)))
(test "let with two params" 2 (evaluate* '(let (x 1 y 1) (+ x y))))
(test "let with longer sequence" 3 (evaluate* '(let () (+ 1 1) (+ 1 2))))
(test "let with bind expression" 2 (evaluate* '(let (x (+ 1 1)) x)))

(test "vector with one value" 2 (vector-ref (evaluate* (vector 2)) 0))
(test "vector with two values" 2 (vector-ref (evaluate* (vector 1 2)) 1))

(test "hash table"
      1
      (hash-table-ref (evaluate* (alist->hash-table '((key: . 1)))) key:))

