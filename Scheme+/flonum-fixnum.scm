;; Translated from Scheme+ for Racket (flonum-fixnum.rkt) for Guile.

(define-module (Scheme+ flonum-fixnum)
  #:use-module (Scheme+ racket-compat))

  ;; the operations of a flonum and a fixnum , written by the type annotation (annot+.rkt of SRFI-105) for {x + i} ,
  ;; {i * x} , {x < i} ... when x is typed flonum? (or ℝ⁺) and i fixnum? (or ℕ) : fl+/fx when the fixnum is the second
  ;; argument , fx/fl+ when it is the first. The fixnum is converted with fx->fl and the flonum operation is used , with
  ;; the same result as the generic operation : when the conversion would change it , the generic operation is called
  ;; (a guard , tested at run time) :
  ;;   + - * / : the exact 0 ((+ -0.0 0) is -0.0 but (fl+ -0.0 0.0) is 0.0 , (* x 0) is the exact 0 , (/ x 0) raises an
  ;;             error) ; x - i and min need no guard
  ;;   < > <= >= = ≠ : |i| > 2^53 (fx->fl rounds i , the generic comparison is exact)
  ;; The macros are hygienic : their variables can not meet the names of the program , and the procedures of their
  ;; bodies are the ones of this module. Each argument is computed once , from the left.

  

  

  ;; the generic operation when the fixnum is the exact 0
  (define-syntax-rule (define-guarded-on-zero fl-op/fx fx/fl-op fl-op generic-op)
    (begin
      (define-syntax-rule (fl-op/fx e1 e2)
        (let ((x e1) (i e2))
          (if (fx= i 0) (generic-op x i) (fl-op x (fx->fl i)))))
      (define-syntax-rule (fx/fl-op e1 e2)
        (let ((i e1) (x e2))
          (if (fx= i 0) (generic-op i x) (fl-op (fx->fl i) x))))))

  (define-guarded-on-zero fl+/fx fx/fl+ fl+ +)
  (define-guarded-on-zero fl*/fx fx/fl* fl* *)
  (define-guarded-on-zero fl//fx fx/fl/ fl/ /)

  ;; x - i : (- -0.0 0) and (fl- -0.0 0.0) are -0.0 , no guard ; i - x : (- 0 0.0) is -0.0 but (fl- 0.0 0.0) is 0.0
  (define-syntax-rule (fl-/fx e1 e2)
    (fl- e1 (fx->fl e2)))

  (define-syntax-rule (fx/fl- e1 e2)
    (let ((i e1) (x e2))
      (if (fx= i 0) (- i x) (fl- (fx->fl i) x))))

  ;; min : no guard (max stays generic : flmax returns the first or the second of 0.0 and -0.0 according to the
  ;; compilation in Racket CS 9.2)
  (define-syntax-rule (flmin/fx e1 e2)
    (flmin e1 (fx->fl e2)))

  (define-syntax-rule (fx/flmin e1 e2)
    (flmin (fx->fl e1) e2))

  ;; the fixnums converted exactly to a flonum : |i| <= 2^53
  (define-syntax-rule (exact-flonum? i)
    (and (fx<= -9007199254740992 i) (fx<= i 9007199254740992)))

  ;; the generic comparison when |i| > 2^53
  (define-syntax-rule (define-guarded-on-size fl-op/fx fx/fl-op fl-op generic-op)
    (begin
      (define-syntax-rule (fl-op/fx e1 e2)
        (let ((x e1) (i e2))
          (if (exact-flonum? i) (fl-op x (fx->fl i)) (generic-op x i))))
      (define-syntax-rule (fx/fl-op e1 e2)
        (let ((i e1) (x e2))
          (if (exact-flonum? i) (fl-op (fx->fl i) x) (generic-op i x))))))

  (define-guarded-on-size fl</fx fx/fl< fl< <)
  (define-guarded-on-size fl>/fx fx/fl> fl> >)
  (define-guarded-on-size fl<=/fx fx/fl<= fl<= <=)
  (define-guarded-on-size fl>=/fx fx/fl>= fl>= >=)
  (define-guarded-on-size fl=/fx fx/fl= fl= =)

  (define-syntax-rule (fl≠ x y) (not (fl= x y)))
  (define-syntax-rule (generic-≠ x y) (not (= x y)))

  (define-guarded-on-size fl≠/fx fx/fl≠ fl≠ generic-≠)

  

(export
	fl+/fx
	fx/fl+
	fl-/fx
	fx/fl-
	fl*/fx
	fx/fl*
	fl//fx
	fx/fl/
	flmin/fx
	fx/flmin
	fl</fx
	fx/fl<
	fl>/fx
	fx/fl>
	fl<=/fx
	fx/fl<=
	fl>=/fx
	fx/fl>=
	fl=/fx
	fx/fl=
	fl≠/fx
	fx/fl≠)
 ; end module
