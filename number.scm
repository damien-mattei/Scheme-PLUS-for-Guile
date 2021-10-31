;; count even and odd of a list of numbers
;; return (n-even . n-odd)
;; (count-even-and-odd '(1 2 3)) -> '(1 . 2)
;; (count-even-and-odd (get-collatz-values)) -> '(23 . 9)
(define (count-even-and-odd L)
  (define (parity n)
    (if (even? n) '(1 . 0) '(0 . 1)))
  (apply proc-add-pair (map parity L)))



(define (one? n)
  (= n 1))

(define (two? n)
  (= n 2))

