(add-to-load-path (string-append (dirname (getcwd)) "/lib"))

(use-modules (aoc utils)
             (aoc srfi-197)
             (srfi srfi-1)
             (ice-9 peg)
             (ice-9 match))

(define input (get-lines "/home/amb/c/2022-aoc/day-five/input"))

(define test-input
  (list
   "    [D]    "
   "[N] [C]    "
   "[Z] [M] [P]"
   " 1   2   3 "
   ""
   "move 1 from 2 to 1"
   "move 3 from 1 to 3"
   "move 2 from 2 to 1"
   "move 1 from 1 to 2"))

(define-peg-pattern num body (+ (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

;; initial stack parsing
(define-peg-pattern no-crate all (ignore "    "))
(define-peg-pattern crate all (and (ignore (and (? " ") "[")) (capture peg-any) (ignore "]")))
(define-peg-pattern row all (+ (or crate no-crate)))
(define-peg-pattern stack-labels none (+ (and " " num " " (? " "))))

;; instructions parsing
(define-peg-pattern how-many all (and (ignore "move ") num))
(define-peg-pattern from all (and (ignore " from ") num))
(define-peg-pattern to all (and (ignore " to ") num))
(define-peg-pattern instruction all (and how-many from to))

(define initial-crate-arrangement
  (take-while (λ (line) (not (match-pattern stack-labels line))) input))

(define initial-crate-arrangement-test
  (take-while (λ (line) (not (match-pattern stack-labels line))) test-input))

;; assumes a maximum of 9 stacks, which works for the given input
(define (assign-stack-numbers stacks)
  (chain stacks
         (zip '("1" "2" "3" "4" "5" "6" "7" "8" "9") _)
         (map (λ (stack) (cons (car stack) (cadr stack))) _))) ; flatten from ("1" ("A" "B" "C")) to ("1" . ("A" "B" "C")

(define stacks
  (chain initial-crate-arrangement
         (map (λ (line) (peg:tree (match-pattern row line))) _)
         (map (match-lambda
                (('row contents) contents)
                (('row . contents) contents)) _)
         (apply zip _)
         (map (λ (stack)
                (filter-map (match-lambda
                              (('crate letter) letter)
                              ('no-crate #f))
                            stack))
              _)

         (assign-stack-numbers _)))

;; an example output line for the instruction "move 5 from 1 to 2", produces (5 "1" "2")
(define instructions
  (chain input
         (list-tail _ (+ 2 (length initial-crate-arrangement)))
         (map (λ (line) (peg:tree (match-pattern instruction line))) _)
         (map (match-lambda
                (('instruction . contents) contents)) _)
         (map (match-lambda
                ((('how-many count) ('from source) ('to dest)) (list (string->number count) source dest)))
              _)))

(define (move-crates! n source dest)
  (let* ((src (assoc-ref stacks source))
         (crate (take src n))
         (next-source (drop src n))
         (destination (assoc-ref stacks dest))
         (next-dest (append crate destination)))
    (assoc-set! stacks source next-source)
    (assoc-set! stacks dest next-dest)))

(define (execute-instruction-9000! instruction)
  (match-let (((count src dest) instruction))
    (let again ((n count))
      (if (< 0 n)
          (begin
            (move-crates! 1 src dest)
            (again (- n 1)))))))

(define (execute-instruction-9001! instruction)
  (match-let (((count src dest) instruction))
    (move-crates! count src dest)))

;; Commented out because the stack manupulation was destructive and I didn't bother going
;; through the whole list-copying song-and-dance. Uncomment and run to get results.

;; (define part-one
;;   (begin
;;     (map (λ (step) (execute-instruction-9000! step)) instructions)
;;     (string-join (map (λ (stack) (cadr stack)) stacks) "")))

;; (define part-two
;;   (begin
;;     (map (λ (step) (execute-instruction-9001! step)) instructions)
;;     (string-join (map (λ (stack) (cadr stack)) stacks) "")))
