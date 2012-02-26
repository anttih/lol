(declare
  (unit repl)
  (uses evaler pretty reader extras srfi-69))

(define builtins '())

(define-syntax defprimitive
  (syntax-rules ()
    ((_ name value) (set! builtins (cons (cons 'name value) builtins)))))

(defprimitive + +)
(defprimitive - -)
(defprimitive * *)
(defprimitive / /)
(defprimitive > >)
(defprimitive < <)
(defprimitive = =)
(defprimitive eq? eq?)
(defprimitive list list)
(defprimitive null? null?)
(defprimitive first car)
(defprimitive rest cdr)
(defprimitive cons cons)
(defprimitive nth vector-ref)
(defprimitive hash-ref hash-table-ref)
(defprimitive print print)
(defprimitive display display)
(defprimitive current-milliseconds current-milliseconds)

(define initial-env
  (make-environment (map car builtins)
					(map cdr builtins)))

(define (repl-)
  (call/cc
    (lambda (quit)
      (let ((env (extend-env initial-env '(quit) (list quit))))
        (print "Welcome to LOL! Type (quit) to quit.")
        (define (loop)
          (display ";lol> ")
          (evaluate (read-) pretty-print env)
          (loop))
        (loop)))))

(define (no-op v) 'void)
(define (evaluate-from-file path)
  (for-each (lambda (s) (evaluate s no-op initial-env))
	        (read-file path read-)))
