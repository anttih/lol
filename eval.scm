(use srfi-69)

(define builtins `((+ . ,+)
                   (- . ,-)
                   (/ . ,/)
                   (* . ,*)))

(define symbols (alist->hash-table builtins))

(define (symbol-resolve symbol)
  (hash-table-ref symbols symbol))

(define (symbol->proc symbol)
  (if (hash-table-exists? symbols symbol)
    (let ((val (hash-table-ref symbols symbol)))
      (if (procedure? val)
        val
        #f))))

(define (definition? sexpr)
  (and (list? sexpr)
       (= 3 (length sexpr))
       (equal? 'def (car sexpr))
       (symbol? (cadr sexpr))))
       
(define (definition-name sexpr)
  (cadr sexpr))

(define (defininition-value sexpr)
  (eval- (caddr sexpr)))

(define (frame-bind frame name val)
    (cons (cons name (car frame))
          (cons val (cdr frame))))

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
      (error "Variable not bound")
      (let ((found (lookup-variable-value (car env) name)))
        (if found
          found
          (lookup-variable (cdr env) name)))))

(define (application? l)
    (pair? l))

(define (parameter-list? s)
  (list? s))

(define (tagged-list? sexpr tag)
  (eq? (car sexpr) tag))

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
  (car p))

(define (proc-params p)
  (cadr p))

(define (proc-sequence p)
  (caddr p))

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

(define (extend-env env names values)
  (cons (cons names values) env))

(define (primitive-procedure? s)
    (procedure? (lookup-variable s)))

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
         (apply-primitive-procedure p))
        ((compound-procedure? p)
         (apply-compound-procedure p args env))))

(define (eval- sexpr env)
    (cond ((number? sexpr) sexpr)
          ((symbol? sexpr) (lookup-variable env sexpr))
		  ((sequence? sexpr) (eval-sequence (cdr sexpr)))
          ((definition? sexpr)
           (define-variable! env
                            (definition-name sexpr)
                            (defininition-value sexpr)))
          ((lambda? sexpr)
           (make-compound-procedure env
                             (lambda-params sexpr)
                             (lambda-sequence sexpr)))
          ((application? sexpr)
           (apply- (symbol-resolve (car sexpr))
                   (list-of-values (cdr sexpr))))
          ((list? sexpr) (list-of-values sexpr))
          (else (print "Unrecognized form"))))


(define (list-of-values sexpr)
  (cond ((null? sexpr) '())
        (else (cons (eval- (car sexpr))
                    (list-of-values (cdr sexpr))))))

(define (repl-)
  (display ";lol> ")
  (print (eval- (read)))
  (repl-))