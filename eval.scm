(declare
  (unit evaler)
  (uses srfi-1 srfi-69))

(define (definition? sexpr)
  (tagged-list? sexpr 'def))
       
(define (definition-name e)
  (let ((spec (cadr e)))
    (cond ((atom? spec) spec)
          (else (car spec)))))

(define (analyze-definition-value e)
  (let ((spec (cadr e)))
    (cond ((atom? spec)
           (analyze (caddr e)))
          (else
            (analyze-anon (cdr spec) (cddr e))))))

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

(define (empty-environment? env) (null? env))

; lookup a binding in the environment
(define (lookup-variable env name)
    (if (empty-environment? env)
      (error (conc "Variable not bound: "  name))
      (let ((found (lookup-variable-value (car env) name)))
        (if found
          found
          (lookup-variable (cdr env) name)))))

(define (application? l) (pair? l))

(define (parameter-list? s) (list? s))

(define (tagged-list? sexpr tag)
  (and (pair? sexpr)
       (eq? (car sexpr) tag)))

(define (thunk? s)
  (not (null? s)))

(define (lambda? sexpr)
  (and (equal? (car sexpr) 'fn)
       (parameter-list? (cadr sexpr))
       (thunk? (cddr sexpr))))

(define (lambda-params s) (cadr s))

(define (lambda-sequence s)
  (cons 'do (cddr s)))

(define (make-compound-procedure names seq env)
  (list 'procedure names seq env))

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

(define (primitive-procedure? p) (procedure? p))

(define (analyze-list parts)
  (cond ((null? parts) '())
        (else (cons (analyze (car parts))
                    (analyze-list (cdr parts))))))

(define (vector-map f v)
  (list->vector
    (let loop ((l (vector->list v)))
      (if (null? l)
        '()
        (cons (f (car l))
              (loop (cdr l)))))))

(define (analyze-vector s)
  (let ((v (vector-map (lambda (val) (analyze val)) s)))
    (lambda (env) (vector-map (lambda (v) (v env)) v))))

(define (map-hash-table f t)
  (alist->hash-table
    (let loop ((h (hash-table->alist t)))
      (if (null? h)
        '()
        (cons `(,(caar h) . ,(f (cdar h)))
              (loop (cdr h)))))))

(define (analyze-hash-table s)
  (let ((h (map-hash-table (lambda (v) (analyze v)) s)))
    (lambda (env) (map-hash-table (lambda (v) (v env)) h))))

(define (analyze-self-evaluating s)
  (lambda (env) s))

(define (analyze-boolean s)
  (let ((bool (boolean-eval s)))
      (lambda (env) bool)))

(define (analyze-variable s)
  (lambda (env) (lookup-variable env s)))

(define (analyze-quoted s)
  (let ((quoted (cdr s)))
    (lambda (env) quoted)))

(define (analyze-if s)
  (let ((test (analyze (cadr s)))
        (then (analyze (caddr s)))
        (alt (if (> (length s) 3)
               (analyze (cadddr s))
               (analyze-boolean 'false))))
    (lambda (env) (if (test env) (then env) (alt env)))))

(define (analyze-seq s)
  (define (sequentally s1 s2)
    (lambda (env) (s1 env) (s2 env)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentally first-proc (car rest-procs))
            (cdr rest-procs))))

  (let ((procs (map analyze s)))
    (if (null? procs)
      (error "Empty sequece")
      (loop (car procs) (cdr procs)))) )

(define (analyze-anon names seq)
  (let ((s (analyze-seq seq)))
    (lambda (env) (make-compound-procedure names s env))))

(define (analyze-lambda s)
  (analyze-anon (cadr s) (cddr s)))

(define (analyze-let s)
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
    (map (compose (lambda (s) (analyze s)) cdr)
         (pairs spec)))

  (let ((l (analyze-anon (let-names (cadr s)) (cddr s)))
        (args (let-values (cadr s))))
    (lambda (env) (invoke (l env) (map (lambda (v) (v env)) args) env))))

(define (analyze-definition s)
  (let ((name (definition-name s))
        (value (analyze-definition-value s)))
    (lambda (env)
      (define-variable! env
                        name
                        (value env)))))

(define (analyze-application s)
  (let ((p (car s))
        (args (analyze-list (cdr s))))
    (if (pair? p)
      (let ((l (analyze-lambda p)))
        (lambda (env) (invoke (l env) (map (lambda (a) (a env)) args) env)))
      (lambda (env) (invoke (lookup-variable env p) (map (lambda (a) (a env)) args) env)))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (invoke p args env)
  (cond ((compound-procedure? p)
         ((caddr p) (extend-env (cadddr p) (cadr p) args)))
        ((primitive-procedure? p)
         (apply p args))
        (else (error "Cannot invoke"))))

(define (analyze s)
  (cond ((self-evaluating? s) (analyze-self-evaluating s))
        ((boolean-value? s) (analyze-boolean s))
        ((symbol? s) (analyze-variable s))
        ((quoted? s) (analyze-quoted s))
        ((if? s) (analyze-if s))
        ((cond? s) (analyze-if (expand-cond s)))
        ((let? s) (analyze-let s))
        ((vector? s) (analyze-vector s))
        ((hash-table? s) (analyze-hash-table s))
        ((lambda? s) (analyze-lambda s))
        ((sequence? s) (analyze-seq (cdr s)))
        ((definition? s) (analyze-definition s))
        ((application? s) (analyze-application s))
        (else (error "Unrecognized form"))))

(define (evaluate sexpr env)
  ((analyze sexpr) env))

(define (unspecified? v)
  (tagged-list? v 'unspecified))
