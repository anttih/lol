
(def (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 2))
       (fib (- n 1)))))

(def (timed f)
  (let (start (current-milliseconds))
    (f)
    (- (current-milliseconds) start)))

(display "30th fibonacci: ")
(print (timed (fn () (fib 30))) "ms")

