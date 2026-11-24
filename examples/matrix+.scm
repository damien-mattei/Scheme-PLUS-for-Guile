
;; guile version

;; parsed by the Makefile of this directory (make) in parsed_files_directory/matrix+.scm , the module (matrix+) is then
;; found with guile -L parsed_files_directory

;; (use-modules (matrix+))



(define-module (matrix+)

  #:use-module (Scheme+)

  ;;#:use-module (Scheme+ for-next-step)

  #:use-module (Scheme+ array)

  #:use-module (oop goops)

  #:use-module ((guile)
		#:select (*)
		#:prefix orig:) ; not used !!!

  #:use-module ((srfi srfi-43)) ;; vector : vector-map  , warning vector-map has index as extra parameter...

  #:export (<matrix>
	    matrix?
	    matrix
	    matrix-v
	    create-matrix-by-function
	    dim-matrix
	    matrix-ref
	    matrix-set!
	    matrix-line-ref
	    matrix-line-set!
	    vector->matrix-column
	    matrix-column->vector
	    multiply-matrix-matrix
	    multiply-matrix-vector)

  #:replace (*) ; the * overloaded by overload-operator (Scheme+) , in place of the * of Guile

  )


;; the type annotation (as in matrix-by-vectors+.rkt of Scheme+ for Racket) : v[lin][col] of a vector of vectors is
;; specialized for vectors when the module is parsed
ANNOTATION-ON



(define-class <matrix> ()
  
  (v #:accessor matrix-v
     #:getter matrix-get-v
     #:setter matrix-set-v!
     #:init-keyword #:v)
  
  )


(define (matrix vinit)
  (make <matrix> #:v vinit))

(define (matrix? obj)
  (is-a? obj <matrix>))

;; (define M (create-matrix-by-function (lambda (l c) (+ l c)) 2 3))
;; $1 = #<<matrix> 1078a0a70>
(define (create-matrix-by-function fct lin col)
  (matrix (create-vector-2d fct lin col)))

;; return the line and column values of dimension
;; scheme@(guile-user)> (dim-matrix M)
;; $1 = 2
;; $2 = 3
(define (dim-matrix M)

  (when (not (matrix? M))
	(error "argument is not of type matrix"))
  
  {v <+ (matrix-v M)}
  (type vector? v {v[]}) ; a vector of vectors
  {lin <+ (vector-length v)}
  {col <+ (vector-length {v[0]})}
  (values lin col))


;; scheme@(guile-user)> (define M1 (create-matrix-by-function (lambda (l c) (+ l c)) 2 3))
;; scheme@(guile-user)> (define M2 (create-matrix-by-function (lambda (l c) (- l c)) 3 2))
;; scheme@(guile-user)> (matrix-v M1)
;; $2 = #(#(0 1 2) #(1 2 3))
;; scheme@(guile-user)> (matrix-v M2)
;; $3 = #(#(0 -1) #(1 0) #(2 1))
;; (define M1*M2 {M1 * M2})
;; scheme@(guile-user)> (matrix-v M1*M2)
;; $2 = #(#(5 2) #(8 2))

(define (multiply-matrix-matrix M1 M2)
  
  {(n1 p1) <+ (dim-matrix M1)}
  {(n2 p2) <+ (dim-matrix M2)}
  
  (when {p1 ≠ n2} (error "matrix.* : matrix product impossible, incompatible dimensions"))
  
  {v1 <+ (matrix-v M1)}
  {v2 <+ (matrix-v M2)}
  (type vector? v1 v2 {v1[]} {v2[]}) ; vectors of vectors : v1[i][k] and v2[k][j] are specialized for vectors
  ;; of reals : the * and + of the sum are the original ones of Guile , not the generic * of GOOPS of this module
  ;; (define-method) that dispatches on its arguments at each call
  (type ℝ {v1[][]} {v2[][]})

  (define (res i j)
    (type ℕ i j k)
    {sum <+ 0}
    (type ℝ sum)
    (for ({k <+ 0} {k < p1} {k <- k + 1})
	 {sum <- sum + v1[i][k] * v2[k][j]})
    sum)

  {v <+ (create-vector-2d res n1 p2)}
  ;(display "v=") (display v) (newline)

  (matrix v))




;; the overloading of Scheme+ : M1 * M2 , a binary overload of * , returns a matrix
(overload-operator * (guile) (matrix?) multiply-matrix-matrix (matrix? matrix?)) ; the original * of the module (guile) (the #:replace of the module header has already created a local *)


(define (vector->matrix-column v)
  (matrix (vector-map (lambda (i x)
			(make-vector 1 x))
		      v)))

(define (matrix-column->vector Mc)
  {v <+ (matrix-v Mc)}
  (vector-map (lambda (i v2)
		(type vector? v2) ; a line of the column matrix
		{v2[0]})
	      v))

;; scheme@(guile-user)> (define M1 (create-matrix-by-function (lambda (l c) (+ l c)) 2 3))
;; scheme@(guile-user)> (matrix-v M1)
;; $1 = #(#(0 1 2) #(1 2 3))
;; scheme@(guile-user)> {M1 * #(1 2 3)}
;; $2 = #(8 14)
(define (multiply-matrix-vector M v) ;; args: matrix ,vector ;  return vector
  {Mc <+ (vector->matrix-column v)}
  (type matrix? M Mc) ; {M * Mc} calls multiply-matrix-matrix directly
  ;(display Mc)
  (matrix-column->vector {M * Mc}))

;; M * v , a binary overload of * , returns a vector
(overload-operator * (guile) (vector?) multiply-matrix-vector (matrix? vector?))


;; define getter,setter
(define (matrix-ref M lin col)
  {v <+ (matrix-v M)}
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  (type ℕ lin col)
  {v[lin][col]})

(define (matrix-set! M lin col x)
  ;;(display "matrix-set!  : M =") (display M) (newline)
  {v <+ (matrix-v M)}
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  (type ℕ lin col)
  {v[lin][col] <- x})


;; >  (overload-square-brackets matrix-ref matrix-set!  (matrix? number? number?))
;; >  (overload-square-brackets matrix-line-ref	 (lambda (x) '())  (matrix? number?))
;; > $ovrld-square-brackets-lst$
;; '(((#<procedure:matrix?> #<procedure:number?>) (#<procedure:matrix-line-ref> . #<procedure>))
;;   ((#<procedure:matrix?> #<procedure:number?> #<procedure:number?>) (#<procedure:matrix-ref> . #<procedure:matrix-set!>)))
;; > (define Mv (matrix #(#(1 2 3) #(4 5 6))))
;; > Mv
;; #<matrix>
;; > (find-getter-for-overloaded-square-brackets (list Mv 1))
;; #<procedure:matrix-line-ref>
;; > {Mv[1 0]}
;; 4
;; > {Mv[1]}
;; '#(4 5 6)
;; > {Mv[1][0]}
;; 4
;; > 
(define (matrix-line-ref M lin)
  {v <+ (matrix-v M)}
  (type vector? v {v[]}) ; a vector of vectors : v[lin] is specialized for vectors
  (type ℕ lin)
  {v[lin]})


(define (matrix-line-set! M lin vect-line)
  {v <+ (matrix-v M)}
  (type vector? v {v[]}) ; a vector of vectors : v[lin] is specialized for vectors
  (type ℕ lin)
  {v[lin] <- vect-line})
