#!/usr/bin/env csi -ss

(use srfi-69)

(define (main args)
  (include "eval")
  (include "read")
  (include "repl")
  (include "pretty")
  (if (null? args)
    (repl-)
    (evaluate-from-file (car args))))
