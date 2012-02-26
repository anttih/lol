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

(define (let? s) (tagged-list? s 'let))
(define (set? s) (tagged-list? s 'set!))

(define (primitive-procedure? p) (procedure? p))

(define (analyze-vector s)
  (define (make-vector-cont c)
    (lambda (xs) (c (list->vector xs))))

  (let ((xs (map (lambda (val) (analyze val)) (vector->list s))))
    (lambda (env c)
      (eval-list env (make-vector-cont c) xs))))

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
      (eval-list env (make-hash-cont c (hash-table-keys s)) vals))))

(define (analyze-self-evaluating v)
  (lambda (env c) (c v)))

(define (analyze-boolean s)
  (let ((bool (boolean-eval s)))
      (lambda (env c) (c bool))))

(define (analyze-variable s)
  (lambda (env c) (c (lookup-variable env s))))

(define (analyze-quoted s)
  (let ((quoted (cdr s)))
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

(define-syntax bail-if
  (syntax-rules ()
    ((_ source value cont alt ...)
     (if (error? value)
       (begin
         (error-calls-set! value (cons source (error-calls value)))
         (cont value))
       (begin alt ...)))))

(define (make-if-cont s c then alt r)
  (lambda (v)
    (bail-if s v c
             (apply (if (true? v) then alt)
                    (list r c)))))

(define (true? v)
  (not (eq? v #f)))

(define (analyze-seq s)

  (define (eval-seq procs env c)
      (if (pair? procs)
        (if (pair? (cdr procs))
          ((car procs) env (make-do-cont procs env c))
          ((car procs) env c))
        (c '())))

  (define (make-do-cont s env c)
    (lambda (v) (eval-seq (cdr s) env c)))

  (let ((procs (map analyze s)))
      (lambda (env c) (eval-seq procs env c))))

;; eval a list of values and return them to the continuation
(define (eval-list env c args)
  (define (make-list-cont c env args)
    (lambda (evaled) (eval-list env (make-gather-cont c evaled) (cdr args))))

  (define (make-gather-cont c evaled)
    (lambda (v) (c (cons evaled v))))

  (if (pair? args)
    ((car args) env (make-list-cont c env args))
    (c '())))

(define (analyze-args s)
  (let ((args (map analyze s)))
    (lambda (env c) (eval-list env c args))))

(define (analyze-anon names seq)
  (let ((s (analyze-seq seq)))
    (lambda (env c) (c (make-compound-procedure names s env)))))

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
     (c 'unspecified)))

(define (analyze-definition s)
  (let ((name (definition-name s))
        (value (analyze-definition-value s)))
    (lambda (env c)
      (value env (make-def-cont c name env)))))

(define (make-apply-cont c args env)
  (lambda (p) (args env (make-invoke-cont c p))))

(define (make-invoke-cont c p)
  (lambda (args) (invoke p args c)))

(define (analyze-application s)
  (let ((p (car s))
        (args (analyze-args (cdr s))))
    (if (pair? p)
      ;; ((fn () ...) ...)
      (let ((f (analyze-lambda p)))
        (lambda (env c)
          (f env (make-apply-cont c args env))))
      ;; (proc ...)
      (lambda (env c)
        (args env (make-invoke-cont c (lookup-variable env p)))))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (invoke p args c)
  (cond ((compound-procedure? p)
         ((caddr p) (extend-env (cadddr p) (cadr p) args) c))
        ((primitive-procedure? p)
         (c (apply p args)))
        (else (error "Cannot invoke"))))

(define (call/cc? s)
  (tagged-list? s 'call/cc))

(define (analyze-call/cc s)
  (define (c-args cc)
    (lambda (env c) (c (list cc))))

  (let ((f (analyze-lambda (cadr s))))
    (lambda (env c)
      (f env (make-apply-cont c (c-args c) env)))))

(define (analyze s)
  (cond ((self-evaluating? s) (analyze-self-evaluating s))
        ((boolean-value? s) (analyze-boolean s))
        ((symbol? s) (analyze-variable s))
        ((quoted? s) (analyze-quoted s))
        ((if? s) (analyze-if s))
        ((cond? s) (analyze-if (expand-cond s)))
        ((let? s) (analyze-let s))
        ((set? s) (analyze-set! s))
        ((vector? s) (analyze-vector s))
        ((hash-table? s) (analyze-hash-table s))
        ((lambda? s) (analyze-lambda s))
        ((sequence? s) (analyze-seq (cdr s)))
        ((definition? s) (analyze-definition s))
        ((call/cc? s) (analyze-call/cc s))
        ((application? s) (analyze-application s))
        (else (error "Unrecognized form"))))

(define (evaluate sexpr c env)
  (set! root-env env)
  ((analyze sexpr) env c))

(define (unspecified? v)
  (tagged-list? v 'unspecified))