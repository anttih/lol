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

(define (create-binding! name val env)
  (hash-table-set! symbols name val))

(define (application? l)
  (let ((first (car l)))
    (and (symbol? first)
         (procedure? (symbol->proc first)))))

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
(define (create-procedure env params seq)
  (list env params seq))

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
(define (extend-env env names values)
  (cons (cons names values) env))

(define (apply- p args env)
  (eval- (proc-sequence p)
		 (extend-env
		   (proc-env p)
		   (proc-params p)
		   args)))

(define (eval- sexpr env)
    (cond ((number? sexpr) sexpr)
          ((symbol? sexpr) (symbol-resolve env sexpr))
		  ((sequence? sexpr) (eval-sequence (cdr sexpr)))
          ((definition? sexpr)
           (create-binding! env
                            (definition-name sexpr)
                            (defininition-value sexpr)))
          ((lambda? sexpr)
           (create-procedure env
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
  (print ";lol>")
  (print (eval- (read)))
  (repl-))