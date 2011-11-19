(declare
  (unit evaler)
  (uses srfi-1 srfi-69))

(define (definition? sexpr)
  (and (pair? sexpr)
       (equal? 'def (car sexpr))))
       
(define (definition-name e)
  (let ((spec (cadr e)))
    (cond ((atom? spec) spec)
          (else (car spec)))))

(define (definition-value e env)
  (let ((spec (cadr e)))
    (cond ((atom? spec)
           (evaluate (caddr e) env))
          (else
            (make-compound-procedure env
                                     (cdr spec)
                                     (cons 'do (cddr e)))))))

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
    (let search-frame ((names (car frame))
                       (values (cdr frame)))
        (if (null? names)
          #f
          (if (eq? (car names) name)
            (car values)
            (search-frame (cdr names)
                          (cdr values))))))

(define (empty-environment? env)
    (null? env))

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

(define (evaluate-sequence seq env)
  (cond ((last-exp? seq) (evaluate (car seq) env))
		(else (evaluate (car seq) env)
			  (evaluate-sequence (cdr seq) env))))

;; environment
(define (make-environment names values)
  (extend-env '() names values))

(define (make-frame names values)
  (cons names values))

(define (extend-env env names values)
  (cons (make-frame names values) env))

(define (self-evaluating? e)
  (or (number? e)
      (string? e)
	  (keyword? e)))

(define (boolean-value? e)
  (and (symbol? e)
	   (or (eq? e 'true)
		   (eq? e 'false))))

(define (boolean-eval s)
  (if (eq? s 'true) #t #f))

(define (quoted? e)
  (tagged-list? e 'quote))

(define (cond? s)
  (tagged-list? s 'cond))

(define (if? e)
  (tagged-list? e 'if))

(define (evaluate-if e env)
  (if (evaluate (cadr e) env)
	(evaluate (caddr e) env)
	(evaluate (cadddr e) env)))

(define (expand-cond s)
  (define (make-if test con alt)
	(list 'if test con alt))

  (define (else? s)
	(tagged-list? s 'else))

  (let process-cond ((conds (cdr s)))
	(if (null? conds)
		'false 
		(make-if (car conds)
				 (cadr conds)
				 (if (else? (cddr conds))
				   (cadddr conds)
				   (process-cond (cddr conds)))))))

(define (let? s)
  (tagged-list? s 'let))

(define (evaluate-let s env)

  (define (pairs s)
    (if (null? s)
      '()
      (cons `(,(car s) . ,(cadr s))
            (pairs (cddr s)))))

  (define (let-names spec)
    (if (not (= (modulo (length spec) 2) 0))
      (error "Unmatched number of let bindings")
      (map car (pairs spec))))

  (define (let-values spec)
    (map (compose (lambda (s) (evaluate s env)) cdr)
		 (pairs spec)))

  (apply-compound-procedure
	(make-compound-procedure env
							 (let-names (cadr s))
							 (cons 'do (cddr s)))
    (let-values (cadr s))))


(define (primitive-procedure? s)
    (procedure? s))

(define (compound-procedure? p)
    (tagged-list? p 'procedure))

(define (apply-compound-procedure p args)
    (evaluate (proc-sequence p)
		 (extend-env
		   (proc-env p)
		   (proc-params p)
		   args)))

(define (apply-primitive-procedure p args)
    (apply p args))

(define (evaluate-list sexpr env)
  (cond ((null? sexpr) '())
        (else (cons (evaluate (car sexpr) env)
                    (evaluate-list (cdr sexpr) env)))))

(define (evaluate-to-alist s env)
  (if (null? s)
	'()
	(cons `(,(evaluate (car s) env) . ,(evaluate (cadr s) env))
		  (evaluate-to-alist (cddr s) env))))

(define (evaluate-hash-table s env)
  (alist->hash-table
    (hash-table-map s
        (lambda (k v) `(,(evaluate k env) . ,(evaluate v env))))))

(define (evaluate-vector s env)
  (apply vector (map (lambda (v) (evaluate v env)) (vector->list s))))

(define (vector-expression? s)
  (tagged-list? s 'vector))

(define (invoke p args env)
  (cond ((primitive-procedure? p)
         (apply-primitive-procedure p args))
        ((compound-procedure? p)
         (apply-compound-procedure p args))))

(define (evaluate sexpr env)
    (cond ((self-evaluating? sexpr) sexpr)
		  ((boolean-value? sexpr) (boolean-eval sexpr))
          ((symbol? sexpr) (lookup-variable env sexpr))
          ((quoted? sexpr) sexpr)
		  ((sequence? sexpr) (evaluate-sequence (cdr sexpr) env))
		  ((if? sexpr) (evaluate-if sexpr env))
		  ((cond? sexpr) (evaluate-if (expand-cond sexpr) env))
          ((let? sexpr) (evaluate-let sexpr env))
		  ((hash-table? sexpr)
		   (evaluate-hash-table sexpr env))
		  ((vector? sexpr)
		   (evaluate-vector sexpr env))
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
           (invoke (if (pair? (car sexpr))
                     (evaluate (car sexpr) env)
                     (lookup-variable env (car sexpr)))
                   (evaluate-list (cdr sexpr) env)
                   env))
          (else (print "Unrecognized form"))))

(define (unspecified? v)
  (tagged-list? v 'unspecified))
