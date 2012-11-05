(declare
  (uses repl))

(use srfi-41)

(define (evaluate-from-file path)
  (for-each (lambda (s) (evaluate s no-op initial-env))
	        (read-file path read-)))

(define (run args)
  (if (null? args)
    (repl-)
    (evaluate-from-file (car args))))

(run (command-line-arguments))
