(add-to-load-path (string-append (dirname (getcwd)) "/lib"))

(use-modules (aoc utils)
             (aoc srfi-197)
             (srfi srfi-1)
             (ice-9 match))

(define input (get-lines-relative "input"))
(define test-input
  (list
   "2-4,6-8"
   "2-3,4-5"
   "5-7,7-9"
   "2-8,3-7"
   "6-6,4-6"
   "2-6,4-8"))

(define (range-str->list str)
  (match-let (((start stop) (map string->number (string-split str #\-))))
    (let ((len (- stop start -1)))
      (iota len start))))

(define part-one
  (chain input
         (map (lambda (s) (string-split s #\,)) _)
         (map (lambda (pairing) (map range-str->list pairing)) _)
         (filter (match-lambda
                   ((elf-a elf-b) (or (lset<= = elf-a elf-b)
                                      (lset<= = elf-b elf-a)))) _)
         (length _)))

(define part-two
  (chain input
         (map (lambda (s) (string-split s #\,)) _)
         (map (lambda (pairing) (map range-str->list pairing)) _)
         (filter (match-lambda
                   ((elf-a elf-b) (not (nil? (lset-intersection = elf-a elf-b))))) _)
         (length _)))
