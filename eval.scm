(use srfi-69)

(define (apply- p)
  (apply f args))

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

(define (create-binding! name val)
  (hash-table-set! symbols name val))

(define (application? l)
  (let ((first (car l)))
    (and (symbol? first)
         (procedure? (symbol->proc first)))))

(define (parameter-list? s)
  (list? s))

(define (thunk? s)
  (not (null? s)))

(define (lambda? sexpr)
  (and (equal? (car sexpr) 'fn)
       (parameter-list? (cadr sexpr))
       (thunk? (cddr sexpr))))

(define (proc-params s)
  (car s))

(define (proc-sequence s)
  (cons 'begin (cddr s)))

(define (create-procedure params seq)
  (list params seq))

(define (eval- sexpr)
    (cond ((number? sexpr) sexpr)
          ((symbol? sexpr) (symbol-resolve sexpr))
          ((definition? sexpr)
           (create-binding! (definition-name sexpr)
                            (defininition-value sexpr)))
          ((lambda? sexpr)
           (create-procedure (proc-params sexpr)
                             (proc-sequence sexpr)))
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