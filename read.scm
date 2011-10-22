
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

(define (char-symbol? c)
  (or (char-alphabetic? c)
      (not (eq? #f (memq c '(#\+ #\/ #\- #\= #\> #\< #\* #\! #\?))))))

(define (read-symbol l)
  (let ((token (take-while-consume char-symbol? l)))
    (list (string->symbol (car token))
          (cadr token))))

(define (open-paren? c)
  (eq? c #\())

(define (close-paren? c)
  (eq? c #\)))

(define (append-one l item)
  (append l (list item)))

(define (tokenize s)
    (let next ((done '())
               (rest (string->list s)))
      (cond ((null? rest) done)
            ((char-whitespace? (car rest))
             (next done (cdr rest)))
            ((char-numeric? (car rest))
             (let ((token (read-number rest)))
               (next (append-one done (list 'number (car token)))
                     (cadr token))))
            ((char-symbol? (car rest))
             (let ((token (read-symbol rest)))
               (next (append-one done (list 'symbol (car token)))
                     (cadr token))))
            ((open-paren? (car rest))
             (next (append-one done '(open-paren))
                   (cdr rest)))
            ((close-paren? (car rest))
             (next (append-one done '(close-paren))
                   (cdr rest)))
            (else (print "Malformed expression")))))

(define (parse-next res tokens)
  (if (null? tokens)
    (list res tokens)
    (let ((next (car tokens)))
      (case (car next)
            ((open-paren) 
             (let ((parsed (parse-next '() (cdr tokens))))
               (parse-next (append-one res (car parsed))
                           (cadr parsed))))
            ((close-paren)
             (list res (cdr tokens)))
            ((symbol)
             (parse-next (append-one res (cadr next))
                         (cdr tokens)))
            ((number)
             (parse-next (append-one res (cadr next))
                         (cdr tokens)))
            (else (print "Unrecognized token"))))))

(define (parse tokens)
  (caar (parse-next '() tokens)))

(define (read-)
  (parse (tokenize (read-line (current-input-port)))))
