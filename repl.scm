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
          (handle-exceptions exn
            (display ((condition-property-accessor 'exn 'message) exn))
            (evaluate (read-) prettyprint env))
          (newline)
          (loop))))))

(define (read-cb success)
  (let-values [((v cs) (read-))]
    (if (not cs)
      (print v)
      (success v))))

(define (no-op v) 'void)
(define (evaluate-from-file path)
  (for-each (lambda (s) (evaluate s no-op initial-env))
            (read-file path read-)))
