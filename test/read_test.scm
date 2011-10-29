(require-extension test)
(include "read")

(define-syntax read-*
  (syntax-rules ()
    ((read-* str)
     (with-input-from-string str read-))))

(test "read number" '(1) (read-* "(1)"))
(test "read longer number" '(12345) (read-* "(12345)"))
(test "read symbol" '(hello) (read-* "(hello)"))
(test "read symbol with special chars" '(a+-*/=<>!?) (read-* "(a+-*/=<>!?)"))

(test "read s with two numbers" '(1 2) (read-* "(1 2)"))
(test "read proc call" '(+ 1 2) (read-* "(+ 1 2)"))
(test "read nested s" '((proc)) (read-* "((proc))"))

(test "read input with whitespace" '(proc) (read-* "  (proc)  "))
(test "read input with newlines" '(proc) (read-* " \n (proc)\n  "))
