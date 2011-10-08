(include "eval")

(define (list-of-values sexpr env)
  (cond ((null? sexpr) '())
        (else (cons (eval- (car sexpr) env)
                    (list-of-values (cdr sexpr) env)))))

(define (pretty s env)
  (define (pretty-values l env)
    (if (null? l)
        ""
        (cons (pretty (car l) env)
              (pretty-values (cdr l) env))))

  (define (join-values l env)
    (reduce-right (lambda (c a) (conc c " " a))
                  ""
                  (pretty-values s env)))

  (cond ((number? s) s)
        ((symbol? s)
         (pretty (lookup-variable env s) env))
        ((unspecified? s) "#<unspecified>")
        ((primitive-procedure? s)
         "#<primitive procedure>")
        ((compound-procedure? s)
         "#<compound-procedure>")
        ((pair? s)
         (conc "(" (join-values s env) ")"))
        (else "<Unrecognized form>")))

(define (pretty-print s env)
  (print (pretty s env)))
