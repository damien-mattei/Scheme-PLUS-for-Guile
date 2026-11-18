;; Translated from Scheme+ for Racket (rec+.rkt) for Guile.

;; DEPRECATED (Scheme+ 18.0) : rec+ is kept for the compatibility with old programs but it is no more documented
;; nor improved. Its infix parsing is done at expansion time ($nfx$-rec) and knows only the procedures of Racket ,
;; not those of the program. Use rec (SRFI 31) or a named let with the curly brackets { }.

(define-module (Scheme+ rec+)
  #:use-module (Scheme+ racket-compat))

	 

	(use-modules
	(Scheme+ return)
	(Scheme+ nfx)
	(Scheme+ lambda-x))

;; (define f (rec+ foo (lambda () (2 + 3))))
;; (f)
;; 5
	
;; list of the arguments from the formals of a procedure
;; (formals->list (a b)) -> (cons a (cons b '()))
;; (formals->list (a b . L)) -> (cons a (cons b L))
;; (formals->list L) -> L
(define-syntax formals->list

  (syntax-rules ()

    ((_ ()) '())
    ((_ (a . rest)) (cons a (formals->list rest)))
    ((_ r) r)))

;; 'return' exits from the current call (see lambda-x),
;; 'return-rec' exits from all the recursive calls to the outermost one (see def)
;; (define f (rec+ (foo n) (if (n = 0) (return-rec 'end) (foo (n - 1)))))
;; (f 5)
;; 'end
(define-syntax rec+
  (syntax-rules ()
   
    ((_ (name . formals) body ...)                ; procedure
     (lambda formals
       (call/cc
	(lambda (ret-rec-id)
	  ;; In the body we adjust the 'return-rec' keyword so that calls
	  ;; to 'return-rec' are replaced with calls to the escape
	  ;; continuation.
	  (syntax-parameterize
	   ([return-rec (syntax-rules ()
			  [(return-rec vals (... ...))
			   (ret-rec-id vals (... ...))])])
	   ;; the recursive calls do not go through the outer call/cc
	   #;(apply (letrec ((name (lambda+ formals body ...)))
		    name)
		  (formals->list formals))
	   (apply (letrec ((name (lambda-x formals body ...)))
		    name)
		  (formals->list formals)))))))
    ((_ name expr)                                ; arbitrary object
     (letrec ((name ($nfx$-rec expr)))
       name))))


(export
	rec+)

