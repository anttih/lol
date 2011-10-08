(use srfi-69 srfi-1)

(define (definition? sexpr)
  (and (list? sexpr)
       (= 3 (length sexpr))
       (equal? 'def (car sexpr))
       (symbol? (cadr sexpr))))
       
(define (definition-name sexpr)
  (cadr sexpr))

(define (definition-value sexpr env)
  (eval- (caddr sexpr) env))

(define (frame-bind frame name val)
    (cons (cons name (car frame))
          (cons val (cdr frame))))

(define (frame-names frame)
    (car frame))

(define (frame-values frame)
    (cdr frame))

(define (define-variable! env name val)
    (set-car! env (frame-bind (car env) name val)))

(define (lookup-variable-value frame name)
    (define (search-frame names values)
        (if (null? names)
          #f
          (if (eq? (car names) name)
            (car values)
            (search-frame (cdr names)
                          (cdr values)))))
    (search-frame (car frame)
                  (cdr frame)))

(define (empty-environment? env)
    (null? env))

(define (error msg)
    (print "Error: " msg))

; lookup a binding in the environment
(define (lookup-variable env name)
    (if (empty-environment? env)
      (error (conc "Variable not bound: "  name))
      (let ((found (lookup-variable-value (car env) name)))
        (if found
          found
          (lookup-variable (cdr env) name)))))

(define (application? l)
    (pair? l))

(define (parameter-list? s)
  (list? s))

(define (tagged-list? sexpr tag)
  (and (pair? sexpr)
       (eq? (car sexpr) tag)))

(define (thunk? s)
  (not (null? s)))

(define (lambda? sexpr)
  (and (equal? (car sexpr) 'fn)
       (parameter-list? (cadr sexpr))
       (thunk? (cddr sexpr))))

(define (lambda-params s)
  (cadr s))

(define (lambda-sequence s)
  (cons 'do (cddr s)))

;; procs
(define (make-compound-procedure env params seq)
  (list 'procedure env params seq))

(define (proc-env p)
  (cadr p))

(define (proc-params p)
  (caddr p))

(define (proc-sequence p)
  (cadddr p))

(define (sequence? sexpr)
  (tagged-list? sexpr 'do))

(define (last-exp? seq)
  (null? (cdr seq)))

(define (eval-sequence seq env)
  (cond ((last-exp? seq) (eval- (car seq) env))
		(else (eval- (car seq) env)
			  (eval-sequence (cdr seq) env))))

;; environment
(define (make-environment names values)
  (extend-env '() names values))

(define (make-frame names values)
  (cons names values))

(define (extend-env env names values)
  (cons (make-frame names values) env))

(define (primitive-procedure? s)
    (procedure? s))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (apply-compound-procedure p args env)
    (eval- (proc-sequence p)
		 (extend-env
		   (proc-env p)
		   (proc-params p)
		   args)))

(define (apply-primitive-procedure p args)
    (apply p args))

(define (apply- p args env)
  (cond ((primitive-procedure? p)
         (apply-primitive-procedure p args))
        ((compound-procedure? p)
         (apply-compound-procedure p args env))))

(define (eval- sexpr env)
    (cond ((number? sexpr) sexpr)
          ((symbol? sexpr) (lookup-variable env sexpr))
		  ((sequence? sexpr) (eval-sequence (cdr sexpr) env))
          ((definition? sexpr)
           (define-variable! env
                            (definition-name sexpr)
                            (definition-value sexpr env))
           (list 'unspecified))
          ((lambda? sexpr)
           (make-compound-procedure env
                             (lambda-params sexpr)
                             (lambda-sequence sexpr)))
          ((application? sexpr)
           (apply- (lookup-variable env (car sexpr))
                   (list-of-values (cdr sexpr) env)
                   env))
          ((list? sexpr) (list-of-values sexpr))
          (else (print "Unrecognized form"))))

(define (unspecified? v)
  (tagged-list? v 'unspecified))
