;; Translated from Scheme+ for Racket (conjunction.rkt) for Guile.

(define-module (Scheme+ conjunction)
  #:use-module (Scheme+ racket-compat))

	

	(define (&& f . r)
	     
	   (cond ((null? r) f)
		 (f (apply && r))
		 (else #f)))
	


(export
	&&)
 ;  end library
