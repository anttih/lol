
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
