;; Translated from Scheme+ for Racket (exponentiation.rkt) for Guile.


(define-module (Scheme+ exponentiation)
  #:use-module (Scheme+ racket-compat))


  



;; coding hint: use only macro when necessary

;; (define-syntax **
;;   (syntax-rules ()
;;     ((_ a b) (expt a b))))

;; (define (** a b)
;;   (expt a b))

;;(define ** expt)

  ;; {2 ** 3 ** 4}
  ;; 2417851639229258349412352

  (define (**-not-tail-rec b . pQ)
    (cond ((null? pQ) 1) ; by convention 2^() = 2⁰ = 1 it is like this in Mathematics
	  ((null? (cdr pQ)) (expt b (car pQ))) ; example : 2³ 
	  (else
	   (expt b (apply **-not-tail-rec pQ))))) ; example 2 ³ ⁴ =  2^(3^4) still like this in Mathematics


  ;; a tail recursive version with an accumulator
  ;; replaced by the case-lambda below : the cost was in expt , called even for x² or x³
  #;(define (** b . pQ)
    (define (**-tail-rec p Q)
      (if (null? p)
	  Q ; when finish we return the accumulator
	  (**-tail-rec (cdr p)
		       (expt (car p) Q)))) ; p ^ Q 

    (if (null? pQ)
	1 ; by convention 2^() = 2⁰ = 1 it is like this in Mathematics
	(let ((Qp (reverse pQ))) ; because exponentiation is right-associative we will compute from right to left, so we need to reverse the list
	  (expt b (**-tail-rec (cdr Qp) (car Qp)))))) ; start with Q,a number in the accumulator and p being a list

  ;; the tail recursive version with an accumulator , for more than 3 arguments (rare : the parser nests the chains
  ;; {2 ** 3 ** 4} in (** 2 (** 3 4)))
  #;(define (**-general b . pQ)
    (define (**-tail-rec p Q)
      (if (null? p)
	  Q ; when finish we return the accumulator
	  (**-tail-rec (cdr p)
		       (expt (car p) Q)))) ; p ^ Q 

    (if (null? pQ)
	1 ; by convention 2^() = 2⁰ = 1 it is like this in Mathematics
	(let ((Qp (reverse pQ))) ; because exponentiation is right-associative we will compute from right to left, so we need to reverse the list
	  (expt b (**-tail-rec (cdr Qp) (car Qp)))))) ; start with Q,a number in the accumulator and p being a list

  ; at least one exponent : (**-general b) is an arity error , as (expt b) and (** b)
  (define (**-general b e . es)
    (define (**-tail-rec p Q)
      (if (null? p)
	  Q ; when finish we return the accumulator
	  (**-tail-rec (cdr p)
		       (expt (car p) Q)))) ; p ^ Q 

    (let ((Qp (reverse (cons e es)))) ; because exponentiation is right-associative we will compute from right to left, so we need to reverse the list
      (expt b (**-tail-rec (cdr Qp) (car Qp))))) ; start with Q,a number in the accumulator and p being a list

  ;; b^e : the exact integer exponent 2 by a multiplication (as gcc does for pow(x,2.0)) , 4 times faster than expt for
  ;; a float and more precise : (* b b) is the exact square rounded once (IEEE 754) , (expt b 2) (the pow of C) was
  ;; wrong by one ulp for 800 floats in 10⁶. Not 3 or 4 : b * (b * b) is wrong by one ulp for 26 % of the floats
  ;; (as x**3 in Fortran , or pow with gcc -ffast-math) , expt only for 0.08 % ; the programmer who wants the speed
  ;; writes {x * (x * x)}. case compares with eqv? : 2.0 is not 2 , it keeps expt.
  (define (power b e)
    (case e
      ((2) (* b b))
      (else (expt b e))))

  (define **
    (case-lambda
      ; no clause for one argument : (** b) is an arity error , as (expt b) (it was 1 , by convention 2^() = 2⁰ = 1)
      #;((b) 1)
      ((b e) (power b e)) ; the usual case : 2³
      ((b e1 e2) (power b (power e1 e2))) ; 2 ³ ⁴ = 2^(3^4) , right-associative
      #;((b . pQ) (apply **-general b pQ))
      ((b e1 e2 e3 . pQ) (apply **-general b e1 e2 e3 pQ))))



(export
	**)
 ; end library


;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript)


