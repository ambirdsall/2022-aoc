(define-module (aoc utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (get-lines split))

(define (get-lines filepath)
  "Return the contents of FILEPATH as a list of newline-delimited strings."
  (call-with-input-file filepath
    (λ (port)
      (let loop ((l (get-line port))
                 (ls (list)))
        (if (eof-object? l)
            (reverse ls)
            (loop (get-line port) (cons l ls)))))))
