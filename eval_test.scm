(use test)
(load "eval.scm")


(test "lambda with empty param list" #t (lambda? '(fn () 1)))
(test "lambda with a thunk" #t (lambda? '(fn () (a 1) (b 2))))

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

(define test-env '(((a b) . (2 3)) ((b c) . (4 5))))

(test "lookup variable in environment" 2 (lookup-variable test-env 'a))
(test "shadow value" 3 (lookup-variable test-env 'b))

; procedures
;(test "not a primitive procedure" #f (primitive-procedure? 

(define empty-env (make-environment '() '()))
(define (evaluate e)
  (eval- e (make-environment '(+ - / *) (list + - / *))))

(test "simple arithmetic" 2 (evaluate '(+ 1 1)))
(test "nested arithmetic" 5 (evaluate '(+ (* 2 2) 1)))
(test "variables"
      4
      (begin
          (evaluate '(do (def a 3)
                            (+ a 1)))))
