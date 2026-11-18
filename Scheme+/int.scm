;; Translated from Scheme+ for Racket (int.rkt) for Guile.

(define-module (Scheme+ int)
  #:use-module (Scheme+ racket-compat))


  


;; (int -3.7)
;; -3
(define (int x)
  (inexact->exact (truncate x)))

(export
	int)

