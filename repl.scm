(include "eval")
(include "pretty")

(define primitives
  `((+     . ,+)
    (-     . ,-)
    (*     . ,*)
    (/     . ,/)
    (>     . ,>)
    (<     . ,<)
    (=     . ,=)
    (first . ,car)
    (rest  . ,cdr)
    (eq?   . ,eq?)))

(define (initial-env)
  (make-environment (map car primitives)
                    (map cdr primitives)))

(define (repl-)
  (call/cc
    (lambda (quit)
      (let ((env (extend-env (initial-env) '(quit) (list quit))))
        (print "Welcome to LOL! Type (quit) to quit.")
        (define (loop)
          (display ";lol> ")
          (pretty-print (eval- (read) env))
          (loop))
        (loop)))))
