(declare
  (unit std)
  (uses evaler))

(define builtins '())

(define-syntax defprimitive
  (syntax-rules ()
    ((_ name value arity n-ary-flag)
     (set! builtins (cons (cons 'name
                                (make-primitive-procedure arity n-ary-flag value))
                          builtins)))))

(define n-ary #t)

(defprimitive + + 2 n-ary)
(defprimitive - - 2 n-ary)
(defprimitive * * 2 n-ary)
(defprimitive / / 2 n-ary)
(defprimitive > > 2 n-ary)
(defprimitive < < 2 n-ary)
(defprimitive = = 2 n-ary)
(defprimitive eq? eq? 2 n-ary)
(defprimitive equal? equal? 2 #f)
(defprimitive not not 1 #f)
(defprimitive list list 1 n-ary)
(defprimitive null? null? 1 #f)
(defprimitive first car 1 #f)
(defprimitive rest cdr 1 #f)
(defprimitive cons cons 2 #f)
(defprimitive nth vector-ref 2 #f)
(defprimitive hash-ref hash-table-ref 2 #f)
(defprimitive print print 1 n-ary)
(defprimitive display display 1 #f)
(defprimitive newline newline 0 #f)
(defprimitive current-milliseconds current-milliseconds 0 #f)

(define initial-env
  (make-environment (map car builtins)
					(map cdr builtins)))
