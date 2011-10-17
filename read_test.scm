(require-extension test)
(include "read")

(test "read number" (list 1) (read- "1"))
(test "read two numbers" (list 1 2) (read- "1 2"))
