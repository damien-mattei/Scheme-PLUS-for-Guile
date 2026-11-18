;; Translated from Scheme+ for Racket (lambda-x.rkt) for Guile.

;; DEPRECATED (Scheme+ 18.0) : lambda-x (named lambda+ before) is kept for the compatibility with old programs but it is no more documented
;; nor improved. Its infix parsing is done at expansion time ($nfx$-rec) and knows only the procedures of Racket ,
;; not those of the program. Use lambda with the curly brackets { } : (lambda (x) {x * 2 + 1})

(define-module (Scheme+ lambda-x)
  #:use-module (Scheme+ racket-compat))

	

	(use-modules
	(Scheme+ return)
	(Scheme+ nfx))


	;; (define foo (lambda-x () 7))
	;; (foo)
	;; 7

	;; > (define foo (lambda-x () (return (3 * 5 + 2))
	;;                        'not_good))
	;; (define foo (lambda-x () (return (3 * 5 + 2)) 'not_good))
	;; #<eof>
	;; > (foo)
	;; (foo)
	;; 17
	;; #<eof>
	(define-syntax lambda-x
	  (lambda (stx)
  	    (syntax-case stx ()
	      ;; formals can be a list of arguments, a list with a rest argument or only a rest argument
	      ;; (lambda-x (a b . L) ...)  (lambda-x L ...)  (lambda-x () ...)
	      ((_ <formals> <body> <body>* ...)

	       #'(lambda <formals>
		   (call/cc
		    (lambda (ret-id) ;(#,ret-id)
		      ;; In the body we adjust the 'return' keyword so that calls
		      ;; to 'return' are replaced with calls to the escape
		      ;; continuation.
		      (syntax-parameterize
		       ([return (syntax-rules ()
				  [(return vals (... ...))
				   (ret-id vals (... ...))])])
		       ($nfx$-rec <body>)
		       ($nfx$-rec <body>*)
		       ...))))))))

	

(export
	lambda-x)
 ; end module


;; > (define loo (lambda-x ()
;;                        ((lambda-x () 3))))                
;; (define loo (lambda-x () ((lambda-x () 3))))
;; #<eof>
;; > (loo)
;; (loo)
;; 3

;; > (define x 3)

;; (define x 3)

;; #<eof>
;; > (define foo (rec bar (lambda-x ()
;;                     (when (< x 5)
;;                       (set! x (+ x 1))
;;                       (display "super!")(newline)
;;                       (bar))
;;                     (display "returning") (newline)
;;                     (return 17)
;;                     'not_good)))


;; (define foo
;;   (rec
;;    bar
;;    (lambda-x
;;     ()
;;     (when (< x 5) (set! x (+ x 1)) (display "super!") (newline) (bar))
;;     (display "returning")
;;     (newline)
;;     (return 17)
;;     'not_good)))


;; #<eof>
;; > (foo)


;; (foo)
;; super!
;; super!
;; returning
;; returning
;; returning
;; 17

;; #<eof>
;; > 
