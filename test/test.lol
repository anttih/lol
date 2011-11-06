
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

(def my-hash {:key "first value" :other "second value"})

(print (hash-ref my-hash :key))
(print (hash-ref my-hash :other))

(def my-vector [1 2 3 4])

(print (vector-ref my-vector 2))

(def name "success")

(let (name "lulz"
      other (square 2))
  (print "name is " name ", and the square of 2 is " other))

(print name)
