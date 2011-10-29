
(def (map f l)
  (if (null? l)
    (list)
    (cons (f (first l))
          (map f (rest l)))))

(def (compose first second)
  (fn (x) (first (second x))))

(def (square x)
  (* x x))

(def (fact n)
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(print (map (compose square fact) (list 3 4 5)))
