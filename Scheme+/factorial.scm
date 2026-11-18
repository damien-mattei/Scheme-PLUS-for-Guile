;; Translated from Scheme+ for Racket (factorial.rkt) for Guile.

(define-module (Scheme+ factorial)
  #:use-module (Scheme+ racket-compat))

  


(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))



(export
	!)



