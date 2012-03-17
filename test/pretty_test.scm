(require-extension test)
(include "pretty")

;; Pretty printing
(test "strings are quoted" "\"hello\"" (pretty "hello"))
(test "numbers are just numbers" "42" (pretty 42))
(test "symbols are not quoted" "symbol" (pretty 'symbol))
(test "quoted list prints like a list" "(name 1)" (pretty '(quote (name 1))))
(test "compound procedures print a special string"
      "#<compound-procedure>"
      (pretty (make-compound-procedure '(names) 'seq 'env)))

(test "error prints with a message"
      "Error: error msg\n\n    (list (if fail 1) 2)\n    (if fail 1) <--"
      (pretty-error (make-error "error msg"
                                '((list (if fail 1) 2)
                                  (if fail 1)))))
