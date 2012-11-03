(declare
  (unit reader)
  (uses parser-combinators ports data-structures srfi-69))

(use srfi-41)

(define (read- . port)
    (expr (stream->token-stream (port->stream (optional port (current-input-port))))))
