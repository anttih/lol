(declare
  (unit pretty)
  (uses evaler))

(define (pretty s env)
  (define (no-formatting? s)
    (or (number? s)
        (symbol? s)))

  (define (implode l sep)
    (reduce-right (lambda (a v) (conc a sep v)) "" l))
    
  (define (join-values l)
    (implode (map (lambda (v) (pretty v env)) l) " "))

  (cond ((no-formatting? s) (format "~a" s))
        ((string? s) (format "\"~a\"" s))
        ((quoted? s) (format "(~a)" (join-values (cdr s))))
        ((unspecified? s) "#<unspecified>")
        ((primitive-procedure? s)
         "#<primitive procedure>")
        ((not (primitive-procedure? s))
         "#<compound-procedure>")
        ((pair? s)
         (conc "(" (join-values s) ")"))
        ((hash-table? s) "#<hash-table>")
        ((vector? s) "#<vector>")
        (else "<Unrecognized form>")))

(define (pretty-print s env)
  (print (pretty s env)))
