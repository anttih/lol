#!/usr/bin/env csi -ss

(define (main args)
  (include "repl")
  (if (null? args)
    (repl-)
    (evaluate-from-file (car args))))
