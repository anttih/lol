(require-extension test)
(use srfi-1 srfi-69)
(include "eval")
(include "std")

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

(define (evaluate* e)
  (call/cc (lambda (return)
    (evaluate e return initial-env))))

; primitive application
(test "simple arithmetic" 2 (evaluate* '(+ 1 1)))
(test "nested arithmetic" 5 (evaluate* '(+ (* 2 2) 1)))

(test "n-ary primitive, one param"
      (list 1)
      (evaluate* '(list 1)))

; sequence
(test "do block with empty body returns nil" '() (evaluate* '(do)))
(test "do block with one expression returns" 2 (evaluate* '(do 2)))
(test "do block with pair expression" 2 (evaluate* '(do (+ 1 1))))

(test "variables" 4 (evaluate* '(do (def a 3) (+ a 1))))

(test "define and apply lambda with no params"
      42
      (evaluate* '(do
                   (def test (fn () 42))
                   (test))))

(test "define function and apply"
      16
      (evaluate* '(do
                   (defn square (x) (* x x))
                   (square 4))))
                    
(test "apply lambda at function position"
      1
      (evaluate* '((fn (x) x) 1)))

(test "arity error"
      "ptrees do not match"
      (error-msg (evaluate* '(do
                    (defn proc (x) x)
                    (proc)))))

(test "error when calling non-existent function"
      "Variable not bound: fail"
      (error-msg (evaluate* '(fail 1))))

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
(test "empty let expands to lambda with no args"
      '((fn () 1))
      (expand-let '(let () 1)))

(test "let with body expression expands to lambda"
      '((fn () (* 2 2)))
      (expand-let '(let () (* 2 2))))

(test "let with body expressions expands to lambda"
      '((fn () (* 2 2) (+ 1 1)))
      (expand-let '(let () (* 2 2) (+ 1 1))))

(test "expand let with one binding"
      '((fn (x) x) 2)
      (expand-let '(let (x 2) x)))

(test "expand let with two bindings"
      '((fn (x y) x) 1 2)
      (expand-let '(let (x 1 y 2) x)))

(test "expand let when destructuring"
      '((fn ((first . rest)) rest) (list 1 2))
      (expand-let '(let ((first . rest) (list 1 2)) rest)))

(test "let with one param" 1 (evaluate* '(let (x 1) x)))
(test "let with two params" 2 (evaluate* '(let (x 1 y 1) (+ x y))))
(test "let with longer sequence" 3 (evaluate* '(let () (+ 1 1) (+ 1 2))))
(test "let with bind expression" 2 (evaluate* '(let (x (+ 1 1)) x)))

(test "vector with one value" 2 (vector-ref (evaluate* (vector 2)) 0))
(test "vector with two values" 2 (vector-ref (evaluate* (vector 1 2)) 1))

(test "hash table"
      1
      (hash-table-ref (evaluate* (alist->hash-table '((key: . 1)))) key:))

;; call/cc
(test "returns value normally"
      1
      (evaluate* '(call/cc (fn (ret) 1))))

(test "call/cc can be returned from"
      3
      (evaluate*
        '(+ 1
           (call/cc
             (fn (return)
               (return 2))))))

(test "set! a binding" 4 (evaluate* '(let (x 1) (set! x 4) x)))

(test "set! returns new value when existing binding not found" 2 (evaluate* '(set! y (+ 1 1))))
(test "set! returns new value when existing binding found" 3 (evaluate* '(let (z 1) (set! z 3))))
(test "set! value can be an expression" 2 (evaluate* '(set! x (- 3 1))))

(test "setting a var sets the closest binding"
      1
      (evaluate* '(let (x 1)
                    (let (x 2) 
                      (set! x (+ x 2)))
                    x)))

(test "error has a message" "Variable not bound: fail" (error-msg (evaluate* (vector 'fail 2))))
(test "error in if test adds call history"
      '((if fail 1))
      (error-calls (evaluate* '(if fail 1))))

(test "error in if consequent adds call history"
      '((if 1 (+ fail 1)) (+ fail 1))
      (error-calls (evaluate* '(if 1 (+ fail 1)))))
