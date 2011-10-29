(include "eval")
(include "pretty")
(include "read")

(define primitives
  `((+     . ,+)
    (-     . ,-)
    (*     . ,*)
    (/     . ,/)
    (>     . ,>)
    (<     . ,<)
    (=     . ,=)
    (list  . ,list)
    (null? . ,null?)
    (first . ,car)
    (rest  . ,cdr)
    (cons  . ,cons)
    (eq?   . ,eq?)
    (print . ,print)
    (display . ,display)))

(define initial-env
  (make-environment (map car primitives)
                    (map cdr primitives)))

(define (repl-)
  (call/cc
    (lambda (quit)
      (let ((env (extend-env (initial-env) '(quit) (list quit))))
        (print "Welcome to LOL! Type (quit) to quit.")
        (define (loop)
          (display ";lol> ")
          (pretty-print (evaluate (read-) env))
          (loop))
        (loop)))))

(define (evaluate-from-file path)
  (for-each (lambda (s) (evaluate s initial-env))
	   (read-file path read-)))
