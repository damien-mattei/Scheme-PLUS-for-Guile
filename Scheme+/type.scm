;; Translated from Scheme+ for Racket (type.rkt) for Guile.

(define-module (Scheme+ type)
  #:use-module (Scheme+ racket-compat))

	

	(use-modules
	(Scheme+ operators))

	;; reserved names of Scheme+ for the types : ℕ and ℤ are fixnum? (with the negative ones , bounded : not the sets of
	;; mathematics) , ℝ is flonum? ; as values for the predicates of the overloads chosen at run time ,
	;; (overload-operator + (vec?) add-vec (vec? ℝ)) ; the annotation reads them as fixnum? and flonum? (annot+.rkt)
	;; a natural number that is a fixnum : the type ℕ (an index of a vector or a string , without the negative indexes)
	(define (natural-fixnum? x)
	  (and (fixnum? x) (>= x 0)))

	;; a float that is not negative : the type ℝ⁺ , as Nonnegative-Flonum of Typed Racket it contains -0.0 and +nan.0
	;; ((flsqrt -1.0) is +nan.0) ; sqrt , log , expt of a ℝ⁺ are floats (flsqrt , fllog , flexpt) , not complex numbers
	(define (nonnegative-flonum? x)
	  (and (flonum? x) (not (< x 0.0))))

	#;(define ℕ fixnum?)
	(define ℕ natural-fixnum?)

	;; a fixnum that is not zero : the type ℤ* , a float to the power of a ℤ* is a float ((expt 2.0 0) is the exact 1)
	(define (nonzero-fixnum? x)
	  (and (fixnum? x) (not (= x 0))))

	;; a fixnum strictly positive : the type ℕ* , it is a ℕ and a ℤ*
	(define (positive-fixnum? x)
	  (and (fixnum? x) (> x 0)))

	(define ℤ* nonzero-fixnum?)
	(define ℕ* positive-fixnum?)
	(define ℤ fixnum?)
	(define ℝ flonum?)
	(define ℝ⁺ nonnegative-flonum?)
	;; only defined for now , not used by the annotation
	(define ℚ rational?)
	(define ℂ complex?)

	;; the names of the types fixnum? and flonum? (for the annotation , annot+.rkt , which works on symbols)
	#;(define (fixnum-type-name? s)
	  (and (memq s '(ℕ ℤ)) #t))

	#;(define (flonum-type-name? s)
	  (eq? s 'ℝ))

	;; one predicate for each name , to distinguish ℕ and ℤ later
	(define (natural-type-name? s) ; today the type fixnum?
	  (eq? s 'ℕ))

	(define (integer-type-name? s) ; today the type fixnum?
	  (eq? s 'ℤ))

	(define (nonzero-integer-type-name? s) ; the type nonzero-fixnum?
	  (eq? s 'ℤ*))

	(define (positive-integer-type-name? s) ; the type positive-fixnum?
	  (eq? s 'ℕ*))

	(define (real-type-name? s) ; today the type flonum?
	  (eq? s 'ℝ))

	(define (nonnegative-real-type-name? s) ; the type nonnegative-flonum?
	  (eq? s 'ℝ⁺))

	;; the types of a domain (a part of a type) and their base type , for the overloads : a ℕ is a fixnum for
	;; {i + 1} --> (fx+ i 1) , a ℝ⁺ a flonum for {x + 1.0} --> (fl+ x 1.0) ; #f for the other types
	(define (domain-type-base t)
	  (case t
	    ((natural-fixnum? nonzero-fixnum? positive-fixnum?) 'fixnum?)
	    ((nonnegative-flonum?) 'flonum?)
	    (else #f)))

	;; the types that contain a type of a domain : ℕ* is in ℕ and ℤ* , they are in ℤ (fixnum?)
	(define (domain-type-parents t)
	  (case t
	    ((positive-fixnum?) '(natural-fixnum? nonzero-fixnum? fixnum?))
	    ((natural-fixnum? nonzero-fixnum?) '(fixnum?))
	    ((nonnegative-flonum?) '(flonum?))
	    (else '())))

	;; (:print-type expr) is a command of the REPL , done by the reader (SRFI-105 or SRFI-110) with the tables of the type
	;; annotation , as :print-type of Typed Racket ; elsewhere (in a module) it is an error
	(define (is-print-type? s) ; for the readers : (:print-type expr) typed at the REPL
	  (eq? s ':print-type))

	(define-syntax :print-type (lambda (stx)
	  (raise-syntax-error ':print-type "works only at the REPL (with ANNOTATION-ON)" stx)))
	
	(define-syntax type
	  
	  (syntax-rules ()
	    
	    ((_ arg1 ...)
	     (begin
	       (display "type: Warning : some type annotation has been skipped by the type annotation parser. Parser code must be checked and fixed , here is the critical annotation: ") (display '(type arg1 ...)) (newline) ; quoted : a composite type like (flonum?) must not be evaluated
	     #;(void)))))

	(define err-port (current-error-port))
	
	(define (is-type? s)
	  (eq? s 'type))

	(define (is-type-expression? s)
	  (and (pair? s) (is-type? (car s))))

	;; in annot+ integers (exact integer) are "filtered" before reals
	;;(define integer exact-integer?)
	;;(define real inexact?)

	(define numerical-types '(exact-integer? integer? number? flonum? real? rational? complex? inexact? fixnum? exact?
				  natural-fixnum? nonzero-fixnum? positive-fixnum? nonnegative-flonum?))

	(define (numerical-type? t)
	  (memq t numerical-types))

	(define (numerical-type-or-unknow? t)
	  (or (not t)
	      (memq t numerical-types)))

	(define (some-numerical-types? L)
	  (ormap numerical-type? L))
	
	(define (all-numerical-types? L)
	  ;(display "type : L=" err-port) (display L err-port) (newline err-port)
	  (define rv (andmap numerical-type? L))
	  ;(display "type : rv=" err-port) (display rv err-port) (newline err-port)
	  rv)


	(define (all-numerical-types-or-unknow? L)
	  (andmap numerical-type-or-unknow? L))

	(define (all-numbers? L)
          (andmap number? L))

	;; litteral? : test if s is a Scheme litteral, that is a self-evaluating datum
	;; (a datum which is not evaluated as a variable reference or a procedure call)
	;; or an explicitly quoted/quasiquoted datum.
	;; examples: (litteral? 3)        --> #t
	;;           (litteral? 3.14)     --> #t
	;;           (litteral? "hello")  --> #t
	;;           (litteral? #t)       --> #t
	;;           (litteral? #\a)      --> #t
	;;           (litteral? #(1 2 3)) --> #t
	;;           (litteral? '())      --> #t
	;;           (litteral? 'x)       --> #f  ; x is a symbol/variable, not a litteral by itself
	;;           (litteral? '(quote x))       --> #t  ; quoted datum is a litteral at runtime
	;;           (litteral? '(+ 1 2))         --> #f  ; procedure call, not a litteral
	(define (litteral? s)
	  (or (boolean? s)
	      (number? s)
	      (string? s)
	      (char? s)
	      (vector? s)
	      (hash? s)
	      (bytes? s)
	      (null? s)
	      (and (pair? s)
		   (quote-or-quasi? (car s)))))


	;; litteral-list? : test if lst is a list whose elements are all litterals
	;; examples: (litteral-list? '(1 2.0 "a" #t))  --> #t
	;;           (litteral-list? '(1 x 3))         --> #f  ; x is a symbol/variable
	(define (litteral-list? lst)
	  (andmap litteral? lst))

	(define ANNOTATION-ON 'ANNOTATION-ON)
	(define ANNOTATION-OFF 'ANNOTATION-OFF)


	

(export
	type
	is-type?
	is-type-expression?
	all-numerical-types?
	numerical-type?
	numerical-types
	all-numerical-types-or-unknow?
	some-numerical-types?
	litteral?
	litteral-list?
	all-numbers?
	ANNOTATION-ON
	ANNOTATION-OFF
	:print-type
	is-print-type?
	ℕ
	ℤ
	ℝ
	ℚ
	ℂ
	ℝ⁺
	ℤ*
	ℕ*
	natural-fixnum?
	nonzero-fixnum?
	positive-fixnum?
	nonzero-integer-type-name?
	positive-integer-type-name?
	domain-type-parents
	nonnegative-flonum?
	natural-type-name?
	integer-type-name?
	real-type-name?
	nonnegative-real-type-name?
	domain-type-base)


