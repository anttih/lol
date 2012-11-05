(declare
  (unit pretty)
  (uses evaler))

(define (pretty s)
  (define (no-formatting? s)
    (or (number? s)
        (symbol? s)))

  (define (implode l sep)
    (reduce-right (lambda (a v) (conc a sep v)) "" l))
    
  (define (join-values l)
    (implode (map (lambda (v) (pretty v)) l) " "))

  (cond ((no-formatting? s) (format "~a" s))
        ((string? s) (format "\"~a\"" s))
        ((quoted? s) (pretty (cadr s)))
        ((inert? s) "#<inert>")
        ((error? s) (pretty-error s))
        ((primitive-procedure? s) "#<primitive procedure>")
        ((compound-procedure? s) "#<compound-procedure>")
        ((pair? s) (conc "(" (join-values s) ")"))
        ((hash-table? s) "#<hash-table>")
        ((vector? s) "#<vector>")
        (else "<Unrecognized form>")))

(define (pretty-error e)
  (conc "Error: " (error-msg e) "\n"
        (fold (lambda (call s) (conc s "\n    " (pretty call)))
              ""
              (error-calls e))
        " <--"))

(define (prettyprint s)
  (display (pretty s)))
