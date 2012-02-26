
(def (test name expected thunk)
  (let (res (thunk))
    (if (not (equal? res expected))
        (print "[FAIL] " name "    expected " expected " but got " res)
        (print "[PASS] " name))))

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

(test "compose and map ints" (list 36 576 14400)
      (fn () (map (compose square fact) (list 3 4 5))))


(test "hash-ref" "value" (fn () (hash-ref {:key "value"} :key)))

(test "vector nth" 3 (fn () (nth [1 2 3 4] 2)))

(def name "success")

(let (name "lulz"
      other (square 2))
  (print "name is " name ", and the square of 2 is " other))

(print name)

(let (cc (call/cc (fn (c) c)))
  (if (eq? cc 1)
    (print "ok: Non-local return")
    (cc 1)))

;; stupid call/cc test
(test "simple arithmetic" 2 (fn () (+ 1 1)))
(test "This should pass" 1 (fn () 1))

(print "Two times:")
(print (call/cc (fn (c) (set! again c) "- first")))
(again "- second")
