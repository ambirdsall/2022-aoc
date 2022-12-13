(define-module (aoc utils)
  #:use-module (ice-9 textual-ports)
  #:export (get-lines get-lines-relative puts))

(define (get-lines filepath)
  "Return the contents of FILEPATH as a list of newline-delimited strings."
  (call-with-input-file filepath
    (λ (port)
      (let loop ((l (get-line port))
                 (ls (list)))
        (if (eof-object? l)
            (reverse ls)
            (loop (get-line port) (cons l ls)))))))

(define (get-lines-relative filepath)
  "A convenience function around get-lines which takes a FILEPATH relative to the current
file's directory."
  (get-lines (string-append (getcwd) "/" filepath)))

(define (puts s)
  (display s)
  (newline))
