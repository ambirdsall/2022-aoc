(add-to-load-path (string-append (dirname (dirname (current-filename))) "/lib"))

(use-modules (aoc utils)
             (aoc srfi-197)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 receive)
             (ice-9 textual-ports))

(define (split l el)
  "Split a list by some delimiter element; the delimiter is lost to the ax."

  (let loop ((collected (list))
             (remaining l))
    (receive (next-partition rest) (break (λ (e) (equal? e el)) remaining)
      (match (list next-partition rest)
        ((() ()) collected) ;; special case bc (cons '() '()) would be (()), not '()
        ((last ()) (cons last collected))
        ((next rem) (loop
                     (cons next collected)
                     (cdr rem))))))) ;; rem is (el . list-we-want)

(define sorted-elf-loads (chain (split (get-lines "input") "")
                        (map (cut map string->number <>) _)
                        (map (cut apply + <>) _)
                        (sort _ >)))

(define part-one (first sorted-elf-loads))

(define part-two (apply + (take sorted-elf-loads 3)))
