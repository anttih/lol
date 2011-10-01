
(use test)
(load "eval.scm")

(test "simple arithmetic" 2 (eval- '(+ 1 1)))
(test "nested arithmetic" 5 (eval- '(+ (* 2 2) 1)))

(test "variables"
      4
      (begin
          (eval- '(def a 3))
          (eval- '(+ a 1))))

(test "lambda with empty param list" #t (lambda? '(fn () 1)))
(test "lambda with a thunk" #t (lambda? '(fn () (a 1) (b 2))))

(test "define in frame"
      '((b a) . (2 1))
      (frame-bind '((a) . (1)) 'b 2))

(test "lookup variable value in frame"
      2
      (lookup-variable-value '((a b) . (2 3)) 'a))

(test "lookup variable value in frame not found"
        #f
        (lookup-variable-value '((a b) . (2 3)) 'c))

(define test-env '(((a b) . (2 3)) ((b c) . (4 5))))

(test "lookup variable in environment" 2 (lookup-variable test-env 'a))
(test "shadow value" 3 (lookup-variable test-env 'b))
