;; Translated from Scheme+ for Racket (not-equal.rkt) for Guile.



(define-module (Scheme+ not-equal)
  #:use-module (Scheme+ racket-compat))


   ; or flonums

  


;; not equal operator for numbers

;; scheme@(guile-user)> (<> 1 2)
;; #t
;; scheme@(guile-user)> {1 <> 2}
;; #t
;; scheme@(guile-user)> {1 <> 1}
;; #f


(define (<> x y)
  (not (= x y)))

(define (≠ x y)
  (not (= x y)))

;; specialized versions of ≠ (and <>) for fixnums and flonums
(define (fx≠ x y)
  (not (fx= x y)))

(define (fl≠ x y)
  (not (fl= x y)))




(export
	<>
	≠
	fx≠
	fl≠)
 ; end library
