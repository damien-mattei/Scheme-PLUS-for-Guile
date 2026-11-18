;; Translated from Scheme+ for Racket (little-greater-or-equal.rkt) for Guile.



(define-module (Scheme+ little-greater-or-equal)
  #:use-module (Scheme+ racket-compat))


  

  ;; {3 ≤ 4}
  ;; #t

  ;; {3 ≤ 4 ≤ 5}
  ;; #t
  (define ≤ <=)

  (define ≥ >=)




(export
	≤
	≥)
 
