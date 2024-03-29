(declare
  (unit evaler)
  (uses srfi-1 srfi-69))

(define (definition? sexpr)
  (tagged-list? sexpr 'def))
       
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
      (throw (conc "Variable not bound: "  name))
      (let ((found (lookup-variable-value (car env) name)))
        (if found
          found
          (lookup-variable (cdr env) name)))))


(define (frame-set-variable frame name value)
    (let search-frame ((names (car frame))
                       (values (cdr frame)))
        (if (null? names)
          #f
          (if (eq? (car names) name)
            (begin (set-car! values value) #t)
            (search-frame (cdr names)
                          (cdr values))))))

(define (env-set-variable env name value)
    (if (null? env)
      (begin (define-variable! root-env name value) value)
      (let ((succ (frame-set-variable (car env) name value)))
        (if succ value (env-set-variable (cdr env) name value)))))

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

(define (make-compound-procedure ptree seq env)
  (list 'procedure ptree seq env))

(define (compound-procedure? p) (tagged-list? p 'procedure))

(define (make-primitive-procedure arity n-ary-flag p)
  (list 'primitive arity n-ary-flag p))

(define (primitive-procedure? p) (tagged-list? p 'primitive))

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

(define (quoted? e) (tagged-list? e 'quote))

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


(define (analyze-vector s)
  (define (make-vector-cont c)
    (lambda (xs) (c (list->vector xs))))

  (let ((xs (map (lambda (val) (analyze val)) (vector->list s))))
    (lambda (env c)
      (eval-list c env (make-vector-cont c) xs))))

(define (analyze-hash-table s)
  (define (interleave keys vals)
    (if (null? keys)
      '()
      (cons
        (cons (car keys) (car vals))
        (interleave (cdr keys) (cdr vals)))))
     
  (define (make-hash-cont c keys)
    (lambda (vals) (c (alist->hash-table (interleave keys vals)))))

  (let ((vals (map analyze (hash-table-values s))))
    (lambda (env c)
      (eval-list c env (make-hash-cont c (hash-table-keys s)) vals))))

(define (analyze-self-evaluating v)
  (lambda (env c) (c v)))

(define (analyze-boolean s)
  (let ((bool (boolean-eval s)))
      (lambda (env c) (c bool))))

(define (analyze-variable s)
  (lambda (env c) (c (lookup-variable env s))))

(define (analyze-quoted s)
  (let ((quoted (cadr s)))
    (lambda (env c) (c quoted))))

(define (analyze-if s)
  (let ((test (analyze (cadr s)))
        (then (analyze (caddr s)))
        (alt (if (> (length s) 3)
               (analyze (cadddr s))
               (analyze-boolean 'false))))
    (lambda (env c)
      (test env (make-if-cont s c then alt env)))))

(define-record error msg calls)

(define (throw msg)
  (make-error msg '()))

(define-syntax error-or
  (syntax-rules ()
    ((_ source value cont alt ...)
     (if (error? value)
       (begin
         (error-calls-set! value (cons source (error-calls value)))
         (cont value))
       (begin alt ...)))))

(define (make-if-cont source cont then alt env)
  (lambda (v)
    (error-or source v cont ((if (true? v) then alt) env (make-catch-cont source cont)))))

(define (error-add-history! error source)
  (error-calls-set! error (cons source (error-calls error))))

(define (make-catch-cont s c)
  (lambda (e)
    (if (error? e)
      (begin
        (error-add-history! e s)
        (c e))
      (c e))))

(define (true? v)
  (not (eq? v #f)))

(define (analyze-seq s)

  (define (eval-seq procs env c)
      (if (pair? procs)
        (if (pair? (cdr procs))
          ((car procs) env (make-do-cont procs env c))
          ((car procs) env c))
        (c '())))

  (define (make-do-cont procs env c)
    (lambda (v) (error-or s v c (eval-seq (cdr procs) env c))))

  (let ((procs (map analyze s)))
      (lambda (env c) (eval-seq procs env (make-catch-cont s c)))))

;; eval a list of values and return them to the continuation
(define (eval-list orig-cont env c args)
  (define (make-list-cont c env args)
    (lambda (evaled)
        (if (error? evaled)
          (orig-cont evaled)
          (eval-list orig-cont env (make-gather-cont c evaled) (cdr args)))))

  (define (make-gather-cont c evaled)
    (lambda (v) (c (cons evaled v))))

  (if (pair? args)
    ((car args) env (make-list-cont c env args))
    (c '())))

(define (analyze-args s)
  (let ((args (map analyze s)))
    (lambda (env c) (eval-list c env c args))))

(define (analyze-anon ptree seq)
  (let ((s (analyze-seq seq)))
    (lambda (env c) (c (make-compound-procedure ptree s env)))))

(define (analyze-lambda s)
  (analyze-anon (cadr s) (cddr s)))

(define (expand-let s)
  (define (pairs s)
    (if (null? s)
      '()
      (cons (cons (car s) (cadr s))
            (pairs (cddr s)))))

  (define (let-names spec)
    (if (not (= (modulo (length spec) 2) 0))
      (error "Unmatched number of let bindings")
      (map car (pairs spec))))

  (define (let-vals spec)
    (map cdr (pairs spec)))

  (cons
    (cons 'fn
          (cons (let-names (cadr s))
                (cddr s)))
    (let-vals (cadr s))))

(define (analyze-let s)
  (analyze-application (expand-let s)))

(define (analyze-set! s)
  (let ((var (cadr s))
        (val (analyze (caddr s))))
    (lambda (env c)
      (val env (make-set-cont c env var)))))

(define (make-set-cont c env name)
  (lambda (val) (c (env-set-variable env name val))))

(define (make-def-cont c name env)
  (lambda (v)
     (define-variable! env name v)
     (c '(inert))))

(define (analyze-definition s)
  (let ((name (cadr s))
        (value (analyze (caddr s))))
    (lambda (env c)
      (value env (make-def-cont c name env)))))

(define (analyze-function-definition s)
  (let ((name (cadr s))
        (fun (analyze-anon (caddr s) (cdddr s))))
    (lambda (env c)
      (fun env (make-def-cont c name env)))))

(define (make-apply-cont s c args env)
  (lambda (p) (args env (make-ptree-cont s (make-compound-invoke-cont s c p) p))))

(define (make-arity-check-cont c p)
  (lambda (args)
    (if (error? args)
      (c args)
      (if (or (= (length args) (cadr p))
              (and (caddr p) ; n-ary
                   (>= (length args) (cadr p))))
        (c args)
        (c (throw (conc "Expected " (cadr p) " arguments but got " (length args))))))))

(define (make-ptree-cont s c p)
  (define (ptree-match tree obj treem objm)
    (cond ((symbol? tree) (cons (cons tree treem)
                                (cons obj objm)))
          ((null? tree)
           (if (null? obj)
             (cons treem objm)
             (throw "ptree does not match")))
          (else (if (not (pair? obj))
                  (throw "ptrees do not match")
                  (let* ((left (ptree-match (car tree) (car obj) treem objm)))
                    (ptree-match (cdr tree) (cdr obj) (car left) (cdr left)))))))

  (lambda (args)
    (c (ptree-match (cadr p) args '() '()))))

(define (make-primitive-invoke-cont s c p)
  (lambda (args) (error-or s args c (c (apply (cadddr p) args)))))

(define (make-compound-invoke-cont s c p)
  (lambda (args) (error-or s args c ((caddr p) (extend-env (cadddr p) (car args) (cdr args)) c))))

(define (analyze-application s)
  (let ((p (car s))
        (args (analyze-args (cdr s))))
    (if (pair? p)
      ;; ((fn () ...) ...)
      (let ((f (analyze-lambda p)))
        (lambda (env c)
          (f env (make-apply-cont s c args env))))
      ;; (proc ...)
      (lambda (env c)
        (let ((proc (lookup-variable env p)))
          (error-or s proc c
            (cond ((compound-procedure? proc)
                   (args env (make-ptree-cont s (make-compound-invoke-cont s c proc) proc)))
                  ((primitive-procedure? proc)
                   (args env (make-arity-check-cont (make-primitive-invoke-cont s c proc) proc)))
                (else (error (conc "cannot invoke " proc))))))))))

(define (analyze-call/cc s)
  (define (c-args cc)
    (let ((p (make-primitive-procedure 1 #f cc)))
      (lambda (env c) (c (list p)))))

  (let ((f (analyze-lambda (cadr s))))
    (lambda (env c)
      (f env (make-apply-cont s c (c-args c) env)))))

(define (analyze s)
  (if (atom? s)
    (cond ((self-evaluating? s) (analyze-self-evaluating s))
          ((boolean-value? s) (analyze-boolean s))
          ((symbol? s) (analyze-variable s))
          ((vector? s) (analyze-vector s))
          ((hash-table? s) (analyze-hash-table s)))
    (case (car s)
          ((quote) (analyze-quoted s))
          ((if) (analyze-if s))
          ((cond) (analyze-if (expand-cond s)))
          ((let) (analyze-let s))
          ((set!) (analyze-set! s))
          ((fn) (analyze-lambda s))
          ((do) (analyze-seq (cdr s)))
          ((def) (analyze-definition s))
          ((defn) (analyze-function-definition s))
          ((call/cc) (analyze-call/cc s))
          (else (analyze-application s)))))

(define (evaluate sexpr c env)
  (set! root-env env)
  ((analyze sexpr) env c))

(define (inert? v) (tagged-list? v 'inert))

