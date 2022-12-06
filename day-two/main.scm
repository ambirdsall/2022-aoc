(add-to-load-path (string-append (dirname (dirname (current-filename))) "/lib"))

(use-modules (aoc srfi-197)
             (aoc utils)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-171)
             (ice-9 match))

#!
Hokay. Let's figure out the rules and input format. Again.

INPUT SEMANTICS
===============
"The first column is what your opponent is going to play: A for Rock, B for Paper, and C
for Scissors. The second column--" Suddenly, the Elf is called away to help with someone's tent.

The second column, you reason, must be what you should play in response: X for Rock, Y for
Paper, and Z for Scissors. Winning every time would be suspicious, so the responses must
have been carefully chosen.

THE RULES
=========
Your total score is the sum of your scores for each round.
The score for a single round is
- the score for the shape you selected
  (1 for Rock, 2 for Paper, and 3 for Scissors)
- plus the score for the outcome of the round
  (0 if you lost, 3 if the round was a draw, and 6 if you won).

MY APPROACH
===========
Data:
I'll represent the list of plays as a nested list of (opponents-play my-play) entries.

Strategy:
- There is a 1:1 correspondence between a play's string representation and its score
- RPS has, I think, a pretty arbitrary table, so there is no benefit to using the original string over its corresponding score
- there's a trivial benefit to using the score, tho: less typing!
- so, the scoring algo:
  1. first, replace each of my plays with its score, so rounds would look like
    1.1 e.g. if the opponent plays Rock and I play Paper, the round would then be represented as ("A" 2)
  2. then, replace each opponent's play with my score for that round
    2.1 e.g. since Paper beats Rock, the round above would become (6 2)
  3. then sum each round's two score components
    3.1 e.g. the round above would sum to 8
  4. reduce the list to the final answer by summing all rounds

Since each of these can be done on a row in isolation, it's blatently more efficient to
define those steps with composable transducers and do it all with a single iteration.

!#

(define score-part-one-rounds (compose
                               (tmap (λ (line) (string-split line #\space))) ;; split a round's plays into li
                               (tmap (match-lambda
                                       ((opp "X") (list opp 1))
                                       ((opp "Y") (list opp 2))
                                       ((opp "Z") (list opp 3))))
                               (tmap (match-lambda
                                       (("A" 1) (+ 3 1)) ;; draw, rocks
                                       (("A" 2) (+ 6 2)) ;; paper beats rock
                                       (("A" 3) (+ 0 3)) ;; scissors loses to rock

                                       (("B" 1) (+ 0 1)) ;; rock loses to paper
                                       (("B" 2) (+ 3 2)) ;; draw, papers
                                       (("B" 3) (+ 6 3)) ;; scissors beats paper

                                       (("C" 1) (+ 6 1)) ;; rock beats scissors
                                       (("C" 2) (+ 0 2)) ;; paper loses to scissors
                                       (("C" 3) (+ 3 3)) ;; draw, scissors
                                    ))))

(define score-part-two-rounds (compose
                               (tmap (λ (line) (string-split line #\space))) ;; split a round's plays into li
                               (tmap (match-lambda
                                       ((opp "X") (list opp 0))
                                       ((opp "Y") (list opp 3))
                                       ((opp "Z") (list opp 6))))
                               (tmap (match-lambda
                                       (("A" 0) (+ 0 3)) ;; scissors loses to rock
                                       (("A" 3) (+ 3 1)) ;; rock draws
                                       (("A" 6) (+ 6 2)) ;; paper wins

                                       (("B" 0) (+ 0 1)) ;; rock loses to paper
                                       (("B" 3) (+ 3 2)) ;; papers draws
                                       (("B" 6) (+ 6 3)) ;; scissors wins

                                       (("C" 0) (+ 0 2)) ;; paper loses to scissors
                                       (("C" 3) (+ 3 3)) ;; scissors draws
                                       (("C" 6) (+ 6 1)) ;; rock wins
                                    ))))

(define input (chain (get-lines (string-append (dirname (current-filename)) "/input" ))))

(define part-one (list-transduce score-part-one-rounds + input))
(define part-two (list-transduce score-part-two-rounds + input))
