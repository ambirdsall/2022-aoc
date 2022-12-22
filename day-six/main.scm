(use-modules (srfi srfi-1)
             (ice-9 curried-definitions)
             (ice-9 textual-ports))

(define (add-to-ring ring el)
      (cons* el (drop-right ring 1)))

(define (marker? ring)
  (= (length ring)
     (char-set-size (list->char-set ring))))

(define ((find-marker-in-port marker-length) port)
  (let step ((ring (string->list (get-string-n port marker-length)))
             (latest marker-length))
    (if (marker? ring)
        latest
        (step (add-to-ring ring (get-char port)) (1+ latest)))))

(define part-one
  (call-with-input-file "input" (find-marker-in-port 4)))

(define part-two
  (call-with-input-file "input" (find-marker-in-port 14)))
