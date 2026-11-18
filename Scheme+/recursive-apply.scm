;; Translated from Scheme+ for Racket (recursive-apply.rkt) for Guile.

(define-module (Scheme+ recursive-apply)
  #:use-module (Scheme+ racket-compat))


	

	;;(require Scheme+/atom)


	(define (recursive-apply L)

	  (cond ((not (list? L)) L) ;;((atom? L) L)
		;; now should be a list
		((null? L) L)
		((procedure? (car L)) ; something to evaluate
		 (apply (car L)
			(map recursive-apply (cdr L))))
		(else L))) ; a simple list of data
	


(export
	recursive-apply)
 ;  end library
