(require-extension test)
(include "read")

(test "tokenize number" (list '(number 1)) (tokenize "1"))
(test "tokenize two numbers" (list '(number 1) '(number 2)) (tokenize "1 2"))
(test "tokenize two longer numbers" (list '(number 11) '(number 21)) (tokenize "11 21"))
(test "tokenize a list" (list '(open-paren) '(number 1) '(close-paren)) (tokenize "(1)"))
(test "tokenize symbol" (list '(symbol name)) (tokenize "name"))

(test "parse list with one number"
      (list '(1))
      (parse '((open-paren) (number 1) (close-paren))))

(test "parse list with two numbers"
      (list '(1 2))
      (parse '((open-paren) (number 1) (number 2) (close-paren))))
