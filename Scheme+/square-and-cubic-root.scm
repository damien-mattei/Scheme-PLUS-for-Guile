;; Translated from Scheme+ for Racket (square-and-cubic-root.rkt) for Guile.

(define-module (Scheme+ square-and-cubic-root)
  #:use-module (Scheme+ racket-compat))


  


  ;; (√ 2)
  ;; 1.4142135623730951
  (define √ sqrt)

  ;; (∛ 7)
  ;; 1.912931182772389
  (define ∛ (lambda (x) (expt x (/ 1 3))))



(export
	√
	∛)

