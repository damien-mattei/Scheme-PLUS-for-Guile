;; Translated from Scheme+ for Racket (lambda+.rkt) for Guile.

;; lambda+ : a lambda with return. The infix is written as everywhere between { } and converted by the reader at the
;; reading (lambda-x , the lambda+ of before , converts also its body at the expansion with $nfx$-rec).

(define-module (Scheme+ lambda+)
  #:use-module (Scheme+ racket-compat))

	

	(use-modules
	(Scheme+ return))


	;; (define foo (lambda+ () 7))
	;; (foo)
	;; 7

	;; > (define foo (lambda+ () (return {3 * 5 + 2})
	;;                        'not_good))
	;; > (foo)
	;; 17

	;; the symbol sym is written in the datum d (the body of a lambda+) : the call/cc of return is made only when
	;; return is used (a return produced by another macro of the body is not seen)
	(eval-when (expand load eval)
	  (define (uses-symbol? sym d)
	    (cond ((eq? d sym) #t)
		  ((pair? d) (or (uses-symbol? sym (car d))
				 (uses-symbol? sym (cdr d))))
		  ((vector? d) (uses-symbol? sym (vector->list d)))
		  (else #f))))

	(define-syntax lambda+
	  (lambda (stx)
  	    (syntax-case stx ()
	      ;; without return in its body : a lambda , no call/cc
	      ((_ <formals> <body> <body>* ...)
	       (not (uses-symbol? 'return (syntax->datum #'(<body> <body>* ...))))
	       #'(lambda <formals> <body> <body>* ...))

	      ;; formals can be a list of arguments, a list with a rest argument or only a rest argument
	      ;; (lambda+ (a b . L) ...)  (lambda+ L ...)  (lambda+ () ...)
	      ((_ <formals> <body> <body>* ...)

	       #'(lambda <formals>
		   (call/cc
		    (lambda (ret-id)
		      ;; In the body we adjust the 'return' keyword so that calls
		      ;; to 'return' are replaced with calls to the escape
		      ;; continuation.
		      (syntax-parameterize
		       ([return (syntax-rules ()
				  [(return vals (... ...))
				   (ret-id vals (... ...))])])
		       <body>
		       <body>*
		       ...))))))))

	

(export
	lambda+)
 ; end module
