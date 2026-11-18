;; Translated from Scheme+ for Racket (examples/test-overload+.rkt) for Guile : the checks of rackunit are written with SRFI 64
;; (tests/rackunit.scm) ; run them with tests/run-tests.sh


;; tests of the overloading of operators and procedures (overload-operator , overload-procedure) :
;; + , - and * on lists , length on vectors and strings , and the new procedures area and volume defined by their first
;; overload-procedure.
;; (most of them come from logiki+.rkt of library-FunctProg , where they had nothing to do with logic)

(define-module (tests test-overload))


	(use-modules
	(Scheme+))
	(use-modules
	(tests rackunit))

	ANNOTATION-ON

	;; the overloadings are at the beginning of the module : the overloaded operators and procedures must be
	;; defined before any use of them at module level.
	;; overload-operator and overload-procedure define the overloaded operator or procedure the first time (the
	;; original one is taken from racket/base) , then add the overload.

	(define (add-n-lists . vn-lst) (implementation-add-n-lists vn-lst))

	(define (implementation-add-n-lists vn-lst)
	  (define plus +)
	  {map-args <+ (cons plus vn-lst)}
	  (apply map map-args))

	(define (sub-n-lists . vn-lst) (implementation-sub-n-lists vn-lst))

	(define (implementation-sub-n-lists vn-lst)
	  (define map-args (cons - vn-lst))
	  (apply map map-args))

	(define (mult-num-list k v) (map (λ (x) (* k x)) v))

	(overload-operator + (list?) add-n-lists (list?)) ; n-ary : any number of lists , returns a list
	(overload-operator - sub-n-lists (list?)) ; n-ary : any number of lists , without return type
	; integer? must be used instead of number? because find-type will test and return integer? before testing
	; number? which is too general
	; (list?) is the return type , for the type annotation
	(overload-operator * (list?) mult-num-list (integer? list?)) ; binary
	(overload-procedure length vector-length (vector?))
	(overload-procedure length string-length (string?))

	;; area and volume are defined by their first overload-procedure
	(define (area-square x) (* x x))
	(define (area-rect x y) (* x y))
	(overload-procedure area area-square (integer?))
	(overload-procedure area area-rect (integer? integer?))

	(define (volume-cube x) (* x x x))
	(define (volume-parallepipede-droit x y z) (* x y z))
	(overload-procedure volume volume-cube (integer?))
	(overload-procedure volume volume-parallepipede-droit (integer? integer? integer?))

	(display "+ =") (display +) (newline)

	;; + , - and * on lists
	(check-equal? (+ '(1 2) '(3 4)) '(4 6))
	(check-equal? (+ '(1 2 3) '(4 5 6)) '(5 7 9))
	(check-equal? {'(1 2 3) - '(4 5 6)} '(-3 -3 -3))
	(check-equal? (- '(1 2 3) '(4 5 6)) '(-3 -3 -3))
	(check-equal? {'(1 2 3) - '(4 5 6) - '(7 8 9)} '(-10 -11 -12))
	(check-equal? (- '(1 2 3)) '(-1 -2 -3))
	{t <+ 3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}
	(check-equal? t '(14 19 24))
	;; the numbers keep the original operators
	(check-equal? (+ 1 2) 3)
	(check-equal? {7 - 2 - 1} 4)
	(check-equal? {3 * 4} 12)

	;; length on lists , vectors and strings
	(check-equal? (length '(1 2 3)) 3)
	(check-equal? (length #(1 2 3 4)) 4)
	(check-equal? (length "abcde") 5)

	;; area and volume
	(check-equal? (area 3) 9)
	(check-equal? (area 2 3) 6)
	(check-equal? (volume 3) 27)
	(check-equal? (volume 3 4 5) 60)

	;; an overload declaration is not allowed in a procedure :
	;; require: not at module level or top level in: (require (rename-in racket/base (* orig-proc)))
	;; (define (foo) ;; ko
	;;   (define x 23)
	;;   (define (mult-num-list k v) (map (λ (x) (* k x)) v))
	;;   (define-overload-existing-operator *)
	;;   (overload * mult-num-list (number? list?))
	;;   {t <- {3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}}
	;;   {x <- 1 + x + 4 * 5}
	;;   t)

	;; If rackunit printed no FAILURE / ERROR block above , every check passed.
	(displayln "test-overload+ : done")

	
