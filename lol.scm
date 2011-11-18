(declare
  (uses repl))

(define (run args)
  (if (null? args)
    (repl-)
    (evaluate-from-file (car args))))

(run (command-line-arguments))
