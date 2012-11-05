(declare
  (unit reader)
  (uses parser-combinators ports data-structures srfi-69))

(use srfi-41)

(define (read- . port)
  (let [(port (optional port (current-input-port)))]
    (let-values [((v cs) (expr (stream->token-stream (port->stream port))))]
      (if (not cs)
        (if v (error v) #!eof)
        v))))
