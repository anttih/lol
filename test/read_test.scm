(require-extension test)
(include "read")

(define-syntax read-*
  (syntax-rules ()
    ((read-* str)
     (with-input-from-string str read-))))

(test "read number" '1 (read-* "1"))
(test "read longer number" '12345 (read-* "12345"))
(test "read symbol" 'hello (read-* "hello"))
(test "read symbol with special chars" 'a+-*/=<>!? (read-* "a+-*/=<>!?"))
(test "ignore comment line" 'symbol (read-* ";comment\nsymbol"))
(test "ignore comment at the end" #!eof (read-* ";comment"))

(test "read keyword" hello: (read-* ":hello"))

(test "read s with two numbers" '(1 2) (read-* "(1 2)"))
(test "read dotted pair" '(1 . 2) (read-* "(1 . 2)"))
(test "read dotted pair with pair in cdr" '(1 . (2 3)) (read-* "(1 . (2 3))"))
(test "read dotted pair in let" '((x . rest) (list 1 2)) (read-* "((x . rest) (list 1 2))"))

(test "read proc call" '(+ 1 2) (read-* "(+ 1 2)"))
(test "read nested s" '((proc)) (read-* "((proc))"))
(test "read more complicated s-expression"
      '(* (+ 34 12) 100)
      (read-* "(* (+ 34 12) 100)"))

(test "read input with whitespace" '(proc) (read-* "  (proc)  "))
(test "read input with newlines" '(proc) (read-* " \n (proc)\n  "))

(test "read hash table expression"
	  (alist->hash-table '((key: . val)))
	  (read-* "{:key val}"))

(test "read vector"
	  (vector 'first 'second)
	  (read-* "[first second]"))

(test "read string"
      '"hello"
      (read-* "\"hello\""))
