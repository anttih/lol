
(define (take-while-consume pred l)
  (let more ((c "")
             (l l))
    (if (null? l)
      (list c l)
      (let ((next (car l)))
        (if (pred next)
          (more (conc c next)
                (cdr l))
          (list c l))))))

(define (read-number l)
  (let* ((token (take-while-consume char-numeric? l))
        (num (car token))
        (rest (cadr token)))
    (list (string->number num) rest)))

(define (read- s)
    (let next ((done '())
               (rest (string->list s)))
      (cond ((null? rest) done)
            ((char-whitespace? (car rest))
             (next done (cdr rest)))
            ((char-numeric? (car rest))
             (let ((token (read-number rest)))
               (next (append done (list (car token)))
                     (cadr token))))
            (else (print "Unrecognized character")))))
