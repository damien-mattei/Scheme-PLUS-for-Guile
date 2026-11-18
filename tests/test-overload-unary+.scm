;; Translated from Scheme+ for Racket (examples/test-overload-unary+.rkt) for Guile : the checks of rackunit are written with SRFI 64
;; (tests/rackunit.scm) ; run them with tests/run-tests.sh


;; Test of the unary operators : overload with exactly one argument (overload-unary-operator) , type annotation and
;; inference , new prefix unary operators recognized in infix (overload-unary-operator , declare-prefix-operator).

;; Author: Damien Mattei

(define-module (tests test-overload-unary))

  (use-modules
	(Scheme+)
	(tests rackunit))

  ANNOTATION-ON

  ;; a matrix : the unary minus (negation) and the binary minus (subtraction)
  (struct mat (v) #:transparent)

  (define (mat-neg M) ; - M
    (mat (vector-map - (mat-v M))))

  (define (mat-sub A B) ; A - B
    (mat (vector-map - (mat-v A) (mat-v B))))

  (define (mat-add A B) ; A + B
    (mat (vector-map + (mat-v A) (mat-v B))))

  (overload-operator - (mat?) mat-sub (mat? mat?)) ; binary
  (overload-operator + (mat?) mat-add (mat? mat?)) ; binary
  (overload-unary-operator - (mat?) mat-neg (mat?)) ; unary : only (- M) , never (- A B)

  (define A (mat (vector 1 2)))
  (define B (mat (vector 5 7)))

  ;; without type : the overload is chosen at runtime
  (check-equal? (- A) (mat (vector -1 -2)))
  (check-equal? (- A B) (mat (vector -4 -5)))
  (check-equal? (- A B A) (mat (vector -5 -7))) ; left associativity : (- (- A B) A)
  (check-equal? (- 3) -3) ; the numbers are unchanged
  (check-equal? (- 10 2 3) 5)

  ;; with the type : the overload is chosen at read time
  (define (neg M)
    (type mat? M)
    {- M}) ; (mat-neg M)
  (check-equal? (neg A) (mat (vector -1 -2)))

  (define (sub M N)
    (type mat? M N)
    {M - N}) ; (mat-sub M N) , not (mat-neg M N)
  (check-equal? (sub A B) (mat (vector -4 -5)))

  (define (add-neg M N)
    (type mat? M N)
    {N + - M}) ; (mat-add N (mat-neg M))
  (check-equal? (add-neg A B) (mat (vector 4 5)))

  ;; inference : the return type of the unary overload
  (define (neg-sub M N)
    (type mat? M N)
    {P <+ - M} ; P is a mat? : (mat-neg M)
    {P - N}) ; (mat-sub P N)
  (check-equal? (neg-sub A B) (mat (vector -6 -9)))

  ;; ~ (bitwise not) typed : (fxnot n) for a fixnum , then the result is a fixnum
  (define (bitwise-not-plus-1 n)
    (type fixnum? n)
    {m <+ ~ n} ; (fxnot n)
    {m + 1}) ; (fx+ m 1)
  (check-equal? (bitwise-not-plus-1 5) -5)

  ;; a new prefix unary operator : ¬ , the logical not , overloaded for the booleans
  (overload-unary-operator ¬ (boolean?) not (boolean?))

  (define (and-not a b)
    (type boolean? a b)
    {a and ¬ b}) ; (and a (not b)) : ¬ is recognized in infix
  (check-equal? (and-not #t #f) #t)
  (check-equal? (and-not #t #t) #f)
  (check-equal? {¬ ¬ #t} #t) ; (¬ (¬ #t))
  {f <+ ¬} ; ¬ as a value : unchanged
  (check-equal? (f #f) #t)

  ;; a prefix unary operator which is not overloaded : declared with declare-prefix-operator
  (define (inv x) (/ 1 x))
  (declare-prefix-operator inv)
  (check-equal? {2 * inv 4} 1/2) ; (* 2 (inv 4))
  (check-equal? {inv 2 ** 2} 1/4) ; (inv (** 2 2)) : as the unary minus , below the exponentiation

  ;; the two precedence engines give the same result (see the section 27 of test-curly-infix+.rkt)
  (use-modules
	((Scheme+ infix-with-precedence-to-prefix) #:select (!*prec-generic-infix-parser-prepare-runtime use-pratt-parser?)))
  (define (parse terms pratt?)
    (parameterize ((use-pratt-parser? pratt?))
      (!*prec-generic-infix-parser-prepare-runtime terms (lambda (op a b) (list op a b)))))
  (define (check-engines terms expected)
    (check-equal? (parse terms #t) expected)
    (check-equal? (parse terms #f) expected))
  (check-engines '(2 * inv 4) '(* 2 (inv 4)))
  (check-engines '(a and inv b ** 2 < c) '(and a (< (inv (** b 2)) c)))

  (displayln "test-overload-unary+ : done")
