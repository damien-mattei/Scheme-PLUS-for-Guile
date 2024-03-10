
;; guile version

;; sudo cp matrix+.scm /usr/local/share/guile/site/3.0

; ./curly-infix2prefix4guile.scm   ../AI_Deep_Learning/guile/matrix+.scm > ../AI_Deep_Learning/guile/matrix.scm

;; (use-modules (matrix+))



(define-module (matrix+)

  #:use-module (Scheme+)

  #:use-module (for_next_step)

  #:use-module (array)

  #:use-module (oop goops)

  #:use-module ((guile)
		#:select (*)
		#:prefix orig:)

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
	    matrix-column->vector)
  
  )



(define-class <matrix> ()
  
  (v #:accessor matrix-v
     #:getter matrix-get-v
     #:setter matrix-set-v!
     #:init-keyword #:v)
  
  )


(define-method (matrix vinit)
  (make <matrix> #:v vinit))

(define-method (matrix? obj)
  (equal? (class-name (class-of obj)) (class-name <matrix>)))

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

(define-method (* (M1 <matrix>) (M2 <matrix>))
  
  {(n1 p1) <+ (dim-matrix M1)}
  {(n2 p2) <+ (dim-matrix M2)}
  
  (when {p1 ≠ n2} (error "matrix.* : matrix product impossible, incompatible dimensions"))
  
  {v1 <+ (matrix-v M1)}
  {v2 <+ (matrix-v M2)}
  
  (define (res i j)
    {sum <+ 0}
    (for ({k <+ 0} {k < p1} {k <- k + 1})
	 {sum <- sum + v1[i][k] * v2[k][j]})
    sum)

  {v <+ (create-vector-2d res n1 p2)}
  ;(display "v=") (display v) (newline)

  (matrix v))




(define (vector->matrix-column v)
  (matrix (vector-map (lambda (i x)
			(make-vector 1 x))
		      v)))

(define (matrix-column->vector Mc)
  {v <+ (matrix-v Mc)}
  (vector-map (lambda (i v2) {v2[0]})
	      v))

;; scheme@(guile-user)> (define M1 (create-matrix-by-function (lambda (l c) (+ l c)) 2 3))
;; scheme@(guile-user)> (matrix-v M1)
;; $1 = #(#(0 1 2) #(1 2 3))
;; scheme@(guile-user)> {M1 * #(1 2 3)}
;; $2 = #(8 14)
(define-method (* (M <matrix>) (v <vector>)) ;; args: matrix ,vector ;  return vector
  {Mc <+ (vector->matrix-column v)}
  ;(display Mc)
  (matrix-column->vector {M * Mc}))


;; define getter,setter
(define (matrix-ref M lin col)
  {v <+ (matrix-v M)}
  {v[lin][col]})

(define (matrix-set! M lin col x)
  {v <+ (matrix-v M)}
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
  {v[lin]})


(define (matrix-line-set! M lin vect-line)
  {v <+ (matrix-v M)}
  {v[lin] <- vect-line})
