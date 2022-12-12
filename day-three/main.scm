(add-to-load-path (string-append (dirname (dirname (current-filename))) "/lib"))

(use-modules (aoc utils)
             (srfi srfi-1)
             (srfi srfi-171))

(define input (get-lines (string-append (dirname (current-filename)) "/" "input")))

(define (list->halves lst)
  "Given a list `lst', returns a new list whose first element is the front half of `lst'
and whose second element is the back half of `lst'."
  (let ((halfway (/ (length lst) 2)))
    (list (list-head lst halfway) (list-tail lst halfway))))

(define (shared-chars char-lists)
  "Given a list of char-lists, returns the list of characters that occur in all sublists."
  (define (shared-chars-one a b)
    (let* ((a-chars (list->char-set a))
           (in-a? (λ (char) (char-set-contains? a-chars char))))
      (filter in-a? b)))
  (reduce shared-chars-one '() char-lists))

(define (char->priority char)
  "Maps each letter of the alphabet to a numeric priority, such that: a => 1, b => 2, z => 26, A => 27, Z => 52"
  (let ((offset (if (char-upper-case? char) 38 96)))
    (- (char->integer char) offset)))

(define inventory-groups->shared-char-priorities
  (compose
   (tmap shared-chars)
   (tmap first)
   (tmap char->priority)))

(define rucksacks->compartment-priorities
  (compose
   (tmap string->list)
   (tmap list->halves)
   inventory-groups->shared-char-priorities))

(define rucksacks->group-priorities
  (compose
   (tmap string->list)
   (tsegment 3)
   inventory-groups->shared-char-priorities))

(define part-one (list-transduce rucksacks->compartment-priorities + input))
(define part-two (list-transduce rucksacks->group-priorities + input))
