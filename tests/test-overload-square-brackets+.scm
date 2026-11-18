;; Translated from Scheme+ for Racket (examples/test-overload-square-brackets+.rkt) for Guile : the checks of rackunit are written with SRFI 64
;; (tests/rackunit.scm) ; run them with tests/run-tests.sh


;; Test of the type annotation of the square brackets overloaded with overload-square-brackets :
;; for an object of known type the annotation pass writes ($bracket-apply-overloaded$ ...) and the getter (or the setter
;; in an assignment) is called directly , chosen from the number of indexes ; otherwise the choice is made at runtime.

;; Author: Damien Mattei

(define-module (tests test-overload-square-brackets))

  (use-modules
	(Scheme+)
	(tests rackunit))

  ANNOTATION-ON

  (struct matrix-vect (v)) ;; matrix based on vector of vectors

  ;; getters and setters : an element M[lin col] , a line M[lin]
  (define (matrix-vect-ref M lin col)
    {v <+ (matrix-vect-v M)}
    {v[lin][col]})

  (define (matrix-vect-set! M lin col x)
    {v <+ (matrix-vect-v M)}
    {v[lin][col] := x})

  (define (matrix-vect-line-ref M lin)
    {v <+ (matrix-vect-v M)}
    {v[lin]})

  (define (matrix-vect-line-set! M lin vect-line)
    {v <+ (matrix-vect-v M)}
    {v[lin] := vect-line})

  (overload-square-brackets matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
  (overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))

  ;; an object of known type : the getter or the setter is called directly
  (type matrix-vect? Mv)
  {Mv <+ (matrix-vect (vector (vector 1 2 3)
			      (vector 4 5 6)))}

  (check-equal? {Mv[1 0]} 4)          ; (matrix-vect-ref Mv 1 0)
  (check-equal? {Mv[1]} #(4 5 6))     ; (matrix-vect-line-ref Mv 1)
  (check-equal? {Mv[1][2]} 6)         ; Mv[1] is a vector

  {i :+ 0}
  {j :+ 2}
  (check-equal? {Mv[(i + 1) (j - 1)]} 5) ; computed indexes

  {Mv[1 0] := -7}                     ; (matrix-vect-set! Mv 1 0 -7)
  (check-equal? {Mv[1 0]} -7)

  {Mv[0] := (vector 7 8 9)}           ; (matrix-vect-line-set! Mv 0 (vector 7 8 9))
  (check-equal? {Mv[0 2]} 9)

  ;; a parameter of known type
  (define (trace M)
    (type matrix-vect? M)
    {M[0 0] + M[1 1]})
  (check-equal? (trace Mv) 12)

  ;; an object without declared type : the generic $bracket-apply$ chooses the getter at runtime , as before
  (define Mw (matrix-vect (vector (vector 1 2)
				  (vector 3 4))))
  (check-equal? {Mw[1 1]} 4)
  {Mw[1 1] := 10}
  (check-equal? {Mw[1 1]} 10)

  ;; the return type of the getter given in the overload : (overload-square-brackets (flonum?) ...) , the type of
  ;; {F[i j]} is known for every object of this type , without a (type flonum? {F[]}) for each object
  (struct fmat (v)) ;; a matrix of flonums

  (define (fmat-ref M lin col)
    {v <+ (fmat-v M)}
    {v[lin][col]})

  (define (fmat-set! M lin col x)
    {v <+ (fmat-v M)}
    {v[lin][col] := x})

  (define (fmat-line-ref M lin)
    {v <+ (fmat-v M)}
    {v[lin]})

  (define (fmat-line-set! M lin line)
    {v <+ (fmat-v M)}
    {v[lin] := line})

  (overload-square-brackets (flonum?) fmat-ref fmat-set! (fmat? number? number?)) ; an element is a flonum
  (overload-square-brackets (vector?) fmat-line-ref fmat-line-set! (fmat? number?)) ; a line is a vector

  (define F (fmat (vector (vector 1.5 2.5)
                          (vector 3.5 4.5))))

  (define (ftrace M)
    (type fmat? M)
    {M[0 0] + M[1 1]}) ; (fl+ (fmat-ref M 0 0) (fmat-ref M 1 1))
  (check-equal? (ftrace F) 6.0)

  (define (fsum-line M i)
    (type fmat? M)
    {line <+ M[i]} ; line is a vector : line[0] is specialized for vectors
    {line[0] + line[1]})
  (check-equal? (fsum-line F 1) 8.0)

  (define (fscale! M a) ; multiply all the coefficients of the matrix by a
    (type fmat? M)
    (type flonum? a)
    {n <+ (vector-length (fmat-v M))} ; number of lines
    {p <+ (vector-length M[0])} ; number of columns : M[0] is a line , a vector
    (for ({i := 0} {i < n} {i := i + 1})
      (for ({j := 0} {j < p} {j := j + 1})
        {M[i j] := a * M[i j]}))) ; (fmat-set! M i j (fl* a (fmat-ref M i j)))
  (fscale! F 2.0)
  (check-equal? (fmat-v F) (vector (vector 3.0 5.0)
                                   (vector 7.0 9.0)))

  (displayln "test-overload-square-brackets+ : done")
