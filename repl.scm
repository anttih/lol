(declare
  (unit repl)
  (uses evaler pretty std reader extras srfi-69))

(define (repl-)
  (call/cc
    (lambda (quit)
      (let* ((q (make-primitive-procedure 0 #f (lambda () (quit #t))))
             (env (extend-env initial-env '(quit) (list q))))
        (print "Welcome to LOL! Type (quit) to quit.")
        (let loop ()
          (display ";lol> ")
          (evaluate (read-) prettyprint env)
          (loop))))))

(define (no-op v) 'void)
(define (evaluate-from-file path)
  (for-each (lambda (s) (evaluate s no-op initial-env))
	        (read-file path read-)))
