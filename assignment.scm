;; assignment version Guile (support my growable vectors)

;; This file is part of Scheme+

;; Copyright 2021-2023 Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;; scheme@(guile-user)> {a[2 4] <- 7}
;; $1 = 7

;; scheme@(guile-user)> {a[2 4]}
;; $1 = 999
;; scheme@(guile-user)> {a[2 4] <- 7}
;; $2 = 7
;; scheme@(guile-user)> {a[2 4]}
;; $3 = 7
;; scheme@(guile-user)> {1 -> a[2 4]}
;; $4 = 1
;; scheme@(guile-user)> {a[2 4]}
;; $5 = 1
;; {x <- 2}
;;
;; (define T (make-vector 5))
;; scheme@(guile-user)> {T[3] <- 7}
;; <- : vector or array set!
;; $1 = 7
;;
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $3 = 7
;;
;; scheme@(guile-user)> {T[7 2 4] <- 4}
;; <- : vector or array set!
;; $2 = 4

;; scheme@(guile-user)> {T[3] <- T[7 2 4]}
;; $bracket-apply$
;; <- : vector or array set!
;; $4 = 4
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $5 = 4

;; scheme@(guile-user)> '{x <- y <- 7}
;; $1 = (<- x y 7)


;; {s <+ (string-append "abcdefgh")}
;; "abcdefgh"
;; > {s[2 * 3 - 4 $ 2 * 3 + 1 $ 2 * 4 - 6] <- "0000"}
;; "ab0d0f0h"

;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
(define-syntax <-
  
  (syntax-rules ()
    ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ;; We will let the second $bracket-apply$ be executed and forbid the execution of first $bracket-apply$.
    
    ;; one dimension array, example: {a[4] <- 7}
    ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
    ((_ (bracket-apply container index ...) expr)

     (begin

       ;; add a checking
       ;; (define x 3)
       ;; > (<- (aye x 3) 7)
       ;; . . ../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/required-files/assignment.rkt:1:6: Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index) , first argument  'aye " is not $bracket-apply$."
       (unless (equal? (quote $bracket-apply$) (quote bracket-apply)) 
	       (error "Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index ...) , first argument is not $bracket-apply$:"
		      (quote bracket-apply)))

       (parse-square-brackets-arguments-and-assignment container expr index ...)))
    
    
    ;;(<- x 5)
    ((_ var expr)
     
     (begin
       ;;(display "<- : variable set!") (newline)
       (set! var expr)
       var))

    
    ;; (declare x y z t)
    ;; {x <- y <- z <- t <- 7}
    ;; 7
    ;; (list x y z t)
    ;; (7 7 7 7)

    ;; > (require srfi/25)
    ;; > {I <- (make-array (shape 0 4 0 4))}
    ;; #<array:srfi-9-record-type-descriptor>
    ;; > {I[0 0] <- I[1 1] <- I[2 2] <- I[3 3] <- 1}
    ;; 1
    ;; > {I[0 0]}
    ;; 1
    ;; > {I[0 1]}
    ;; 0
    ;; > I
    ;; #<array:srfi-9-record-type-descriptor>
    
    ((_ var var1 ... expr)
     
     (<- var (<- var1 ... expr))) 
     
    ))







;; (-> 5 x)
;; 5

;; (declare x)
;; {5 -> x}
;; 5

;; > (declare I)
;; > (require srfi/25)
;; > {I <- (make-array (shape 0 4 0 4))}
;; #<array:srfi-9-record-type-descriptor>
;; > {1 -> I[0 0] -> I[1 1] -> I[2 2] -> I[3 3]}
;; 1
;; > {I[0 0]}
;; 1
;; > {I[0 1]}
;; 0

;; > (define T (make-vector 5))
;; > {T[3] <- 7}
;; 7
;; > {T[3] -> T[7 2 4]}
;; 7
;; > {T[7 2 4]}
;; 7
(define-syntax ->
  (syntax-rules ()

    ((_ expr var ...) (<- var ... expr))))




;; > (declare x y z)
;; > {7 → x → y → z}
;; 7
(define-syntax → ;; under Linux this symbol can be typed with the
  ;; combination of keys: Ctrl-Shift-u 2192 where 2192 is the unicode of right arrow

  (syntax-rules () 

    ((_ expr ...) (-> expr ...))))


;; Mac OS use CTRL+CMD+space to bring up the characters popover, then type in u + unicode and hit Enter to get it)

;; > (declare x y)
;; > {x ← y ← 7}
;; 7
;; > (list x y)
;; '(7 7)

;; (declare I)
;; {I <- (make-array 0 2 2)}
;; #2((0 0)
;;    (0 0))
;;
;; {I[0 0] ← I[1 1] ← 1}
;; 1
;;
;; I
;; #2((1 0)
;;    (0 1))

(define-syntax ← ;; under Linux this symbol can be typed with the
  ;; combination of keys: Ctrl-Shift-u 2190 where 2190 is the unicode of left arrow

  (syntax-rules ()

    ((_ var ...) (<- var ...))))



;; (declare x y z)
;;  {(x y z) <v (values 2 4 5)}
;; 2
;; 4
;; 5
;; > (list x y z)
;; '(2 4 5)
;; > (declare u v w)
;; > {(x y z) <v (u v w) <v (values 2 4 5)}
;; 2
;; 4
;; 5
;; > (list x y z u v w)
;; '(2 4 5 2 4 5)
;; > (declare a b c)
;; > {(x y z) <v (u v w) <v (a b c)  <v (values 2 4 5)}
;; 2
;; 4
;; 5
;; > (list x y z u v w a b c)
;; '(2 4 5 2 4 5 2 4 5)
;;
;; (define T (make-vector 5))
;; {(x {T[4]} z) <v (values 1 2 3)}
;; 1
;; 2
;; 3
;; {T[4]}
;; 2

;; > (declare u v w a b c)
;; > {(a b c) <v (x {T[4]} z) <v (u v w) <v (values 1 2 3)}
;; 1
;; 2
;; 3
;; > (list a b c x {T[4]} z u v w)
;; '(1 2 3 1 2 3 1 2 3)
;; > {(x {T[4]} z) <v (u v w) <v (a b c) <v (values 1 2 3)}
;; 1
;; 2
;; 3
;; > (list a b c x {T[4]} z u v w)
;; '(1 2 3 1 2 3 1 2 3)
;; > {(a b c)  <v (u v w) <v (x {T[4]} z) <v (values 1 2 3)}
;; 1
;; 2
;; 3
;; > (list a b c x {T[4]} z u v w)
;; '(1 2 3 1 2 3 1 2 3)

(define-syntax <v
  
  (syntax-rules ()
    
    ((_ (var1 ...) expr) (begin
			   (set!-values-plus (var1 ...) expr)
			   (values var1 ...)))

    ((_ (var10 ...) (var11 ...) ... expr)
     
     (<v (var10 ...) (<v (var11 ...) ... expr)))
    

    ))


;; (declare x y z)
;; {(values 2 4 5) v> (x y z)}
;; 2
;; 4
;; 5
;;  (list x y z)
;; '(2 4 5)

;; (declare x y z u v w)
;; {(values 2 4 5) v> (x y z) v> (u v w)}
;; 2
;; 4
;; 5
;; (list x y z u v w)
;; '(2 4 5 2 4 5)
(define-syntax v>
  
  (syntax-rules ()

    ((_ expr var-list ...) (<v var-list ... expr))))

   
(define-syntax ⇜
  
  (syntax-rules ()

    ((_ var ...) (<v var ...))))


(define-syntax ⇝
  
  (syntax-rules ()

    ((_ expr ...) (v> expr ...))))
     
   

(define (parse-square-brackets-arguments-and-assignment container expr . args-brackets)

  {args <+ (parse-square-brackets-arguments args-brackets)}

  (case (length args)
    ;; 1 argument in [ ]
    ;; T[index]
    ((1) (assignment-argument-1 container (first args) expr))
     
    ;; 2 arguments in [ ]
    ;; ex: T[i1 $] , T[$ i2], T[i1 i2] , T[$ $]
    
    ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) $]}
    ;; '#(3 4 5)
    ((2) (assignment-argument-2 container
				(first args)
				(second args)
				expr))

    ;; 3 arguments in [ ]
    ;; T[i1 $ i2] , T[i1 i2 i3] , T[$ $ s]
    ((3) (assignment-argument-3 container
				(first args)
				(second args)
				(third args)
				expr))


    ;; 4 arguments in [ ]
    ;; T[$ i2 $ s] , T[i1 $ $ s] , T[i1 $ i3 $] , T[i1 i2 i3 i4]
    ((4) (assignment-argument-4 container
				(first args)
				(second args)
				(third args)
				(fourth args)
				expr))

 

    ;; 5 arguments in [ ]
    ;; T[i1 $ i3 $ s] , T[i1 i2 i3 i4 i5]
    ((5) (assignment-argument-5 container
				(first args)
				(second args)
				(third args)
				(fourth args)
				(fifth args)
				expr))


    ;; more than 5 arguments in [ ]
    ;; T[i1 i2 i3 i4 i5 i6 ...]
    (else
     (assignment-argument-6-and-more container expr args))))
     
  

;; > (declare x y z)
;; > (assign-var (x y z) (1 2 3))
;; > x
;; 1
;; > y
;; 2
;; > z
;; 3
;; USELESS
(define-syntax assign-var
  (syntax-rules ()

    ((_ (var ...) (exp ...)) (begin (set! var exp) ...))))





       
       
;; TODO: this function should be used many times in many places above
;; function bug in Guile but not in Racket
;;; WARNING: compilation of /usr/local/share/guile/site/3.0/Scheme+.scm failed:
;;; Syntax error:
;;; assignment.scm:30:11: definition in expression context, where definitions are not allowed, in form (define container-eval container-eval2)
;; While compiling expression:
;; Syntax error:
;; /usr/local/share/guile/site/3.0/assignment.scm:30:11: definition in expression context, where definitions are not allowed, in form (define container-eval vct)
;; (define (copy-stepped-slice vct expr i1 i2 step)
  
;;   (when {step = 0}
;; 	(error "assignment : slice step cannot be zero"))

;;   {i <+ 0}

;;   (if {step < 0} ;; with negative index we start at end of vector (like in Python)
;;       (for ({k <+ i2} {k >= i1} {k <- {k + step}})
;; 	   {vct[k] <- expr[i]}
;; 	   {i <- {i + 1}})
      
;;       (for ({k <+ i1} {k < i2} {k <- {k + step}})
;; 	   {vct[k] <- expr[i]}
;; 	   {i <- {i + 1}})))


(define (copy-stepped-slice container-eval expr-eval i1 i2 step)
  
  (when {step = 0}
	(error "assignment : slice step cannot be zero"))

  {i <+ 0}

  (if {step < 0} ;; with negative index we start at end of vector (like in Python)
      (for ({k <+ i2} {k >= i1} {k <- k + step})
	   {container-eval[k] <- expr-eval[i]}
	   {i <- i + 1})
      
      (for ({k <+ i1} {k < i2} {k <- k + step})
	   {container-eval[k] <- expr-eval[i]}
	   {i <- i + 1})))



;; useless
;; (define (copy-stepped-slice2 vct expr i1 i2 step)
  
;;   (when {step = 0}
;; 	(error "assignment : slice step cannot be zero"))

;;   {i <+ 0}

;;   (if {step < 0} ;; with negative index we start at end of vector (like in Python)
;;       (for ({k <+ i2} {k >= i1} {k <- {k + step}})
;; 	   (vector-set! vct
;; 			k
;; 			(vector-ref expr i))
;; 	   {i <- {i + 1}})
      
;;       (for ({k <+ i1} {k < i2} {k <- {k + step}})
;; 	   (vector-set! vct
;; 			k
;; 			(vector-ref expr i))
;; 	   {i <- {i + 1}})))





(define (assignment-argument-1 container-eval index-eval expr-eval)
  
  
  (if (equal? index-eval slice)  ;; T[$]
      
      (cond ((or (vector? container-eval) (growable-vector? container-eval))
	     (vector-copy! container-eval
			   0
			   expr-eval)
	     container-eval)
	    
	    ((hash-table? container-eval) (error "slicing not permitted with hash table"))
	    ((string? container-eval) (string-copy! container-eval
						    0
						    expr-eval)
	     container-eval)
	    (else (error "slicing not permitted with arrays")))

      
      ;; normal case
      ;; {T[7 2 4] <- 4}
      ;; {T[3] <- T[7 2 4]}
      
      
      (cond ((or (vector? container-eval) (growable-vector? container-eval))
	     (when {index-eval < 0} ;; deal with negative index
	       {index-eval <- (vector-length container-eval) + index-eval})
	     (vector-set! container-eval index-eval expr-eval)
	     expr-eval)
	    
	    ((hash-table? container-eval)
	     (hash-table-set! container-eval index-eval expr-eval)

	     expr-eval

	     )
	    
	    ((string? container-eval)
	     (when {index-eval < 0} ;; deal with negative index
	       {index-eval <- (string-length container-eval) + index-eval})
	     (string-set! container-eval index-eval expr-eval)
	     expr-eval)
	    
	    (else (srfi25-array-set! container-eval index-eval expr-eval)
		  expr-eval)))) ;; returning a value allow the chaining : {T[3] <- A[4] <- T[7 2 4]}




(define (assignment-argument-2 container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)

  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
  
  {index1-or-keyword-eval-pos <+ index1-or-keyword-eval} ;; pos for positive
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}

  (declare container-length container-copy!)
  
  (if (vector? container-eval) 
      ($>
       {container-length <- vector-length}
       {container-copy! <- vector-copy!})
      ($>  ;; a string
       {container-length <- string-length}
       {container-copy! <- string-copy!}))
  
  ;; transform the negative indexes in positive ones when not slices
  (when {(not (equal? index1-or-keyword-eval-pos slice)) and {index1-or-keyword-eval-pos < 0}}
    {index1-or-keyword-eval-pos <- (container-length container-eval) + index1-or-keyword-eval-pos})
  
  (when {(not (equal? index2-or-keyword-eval-pos slice)) and {index2-or-keyword-eval-pos < 0}}
    {index2-or-keyword-eval-pos <- (container-length container-eval) + index2-or-keyword-eval-pos})
  
  
  ;; > (require srfi/25)
  ;; > (define a (make-array (shape 0 5 0 3) 0))
  ;; > {a[1 2]}
  ;; 0
  ;; > {a[1 2] <- 7}
  ;; 7
  ;; > {a[1 2]}
  ;; 7
  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos)
	 
	 ;;  {a <+ (make-vector 7 0)}
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[$ $] <- #(1 2 3)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)
	 (((? (cut equal? <> slice)) (? (cut equal? <> slice))) (container-copy! container-eval
										 0
										 expr-eval)

	  container-eval

	  )
	 
	 ;;  {s <+ (string-append "abcdefgh")}
	 ;; "abcdefgh"
	 ;; > {s[3 $] <- "zob"}
	 ;; > s
	 ;; "abczobgh"
	 ;; > 
	 ((i1 (? (cut equal? <> slice))) (container-copy! container-eval
							  i1
							  expr-eval)
	  container-eval

	  )
	 
	 (((? (cut equal? <> slice)) i2) (container-copy! container-eval
							  0
							  expr-eval
							  0
							  i2)


	  container-eval

	  )
	 
	 ((i1 i2)  (if (vector? container-eval)  ;; normal case
		       (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2))) 
		       (srfi25-array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval))


	  expr-eval ;; returning a value allow the chaining : {T[3 2] <- A[4] <- T[2 4]} 

	  )

	 ) ;; end match

  

  )




(define (assignment-argument-3 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)

  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
  
  {index1-or-keyword-eval-pos <+ index1-or-keyword-eval}
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}
  {index3-or-keyword-or-step-eval-pos <+ index3-or-keyword-or-step-eval}
 

  (declare container-length container-copy!)
  
  (if (vector? container-eval)
      ($>
       {container-length <- vector-length}
       {container-copy! <- vector-copy!})
      ($>  ;; a string
       {container-length <- string-length}
       {container-copy! <- string-copy!}))
  
  ;; transform the negative indexes in positive ones when not slices
  (when {(not (equal? index1-or-keyword-eval-pos slice)) and {index1-or-keyword-eval-pos < 0}}
	{index1-or-keyword-eval-pos <- (container-length container-eval) + index1-or-keyword-eval-pos})
  
  (when {(not (equal? index2-or-keyword-eval-pos slice)) and {index2-or-keyword-eval-pos < 0}}
	{index2-or-keyword-eval-pos <- (container-length container-eval) + index2-or-keyword-eval-pos})
  
  (when {(not (equal? index3-or-keyword-or-step-eval-pos slice)) and {index3-or-keyword-or-step-eval-pos < 0}}
	{index3-or-keyword-or-step-eval-pos <- (container-length container-eval) + index3-or-keyword-or-step-eval-pos})

  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-or-step-eval-pos)
	 

	 ;; T[$ i2 $]
	 (( (? (cut equal? <> slice)) i2 (? (cut equal? <> slice)) )
	  
	  (container-copy! container-eval
			   0
			   expr-eval
			   0
			   i2)

	  container-eval

	  )

	 
	 ;; T[i1 $ $]
	 ((i1 (? (cut equal? <> slice)) (? (cut equal? <> slice)))

	  (container-copy! container-eval
			   i1
			   expr-eval)

	  container-eval

	  )
	 
	 
	 ;; T[$ $ s3]
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; '#(1 2 3 4 5 6 7 8 9)
	 ;; > {v[$ $ 2] <- (vector 0 0 0 0 0)}
	 ;; '#(0 2 0 4 0 6 0 8 0)
	 ;; > {v[$ $ 2] <- (vector -1 -2 -3 -4 -5)}
	 ;;> v
	 ;;'#(-1 2 -2 4 -3 6 -4 8 -5)
	 (((? (cut equal? <> slice)) (? (cut equal? <> slice)) step-not-used)
	  
	  (cond ((vector? container-eval)
		 
		 (when (= 0 index3-or-keyword-or-step-eval)
		       (error "assignment : slice step cannot be zero"))
		 
		 (let* ((size-input (vector-length container-eval))
			(i 0))
		   
		   (if (< index3-or-keyword-or-step-eval 0) ;; with negative index we start at end of vector (like in Python)
		       (for ((define k (- size-input 1)) (>= k 0) (set! k (+ k index3-or-keyword-or-step-eval)))
			    (vector-set! container-eval
					 k
					 (vector-ref expr-eval i))
			    (set! i (+ 1 i)))
		       
		       (for ({k <+ 0} {k < size-input} {k <- k + index3-or-keyword-or-step-eval})
			    (vector-set! container-eval
					 k
					 (vector-ref expr-eval i))
			    {i <- 1 + i})))

		 container-eval
		 )

		;; > {s <+ (string-append "abcdefgh")}
		;; "abcdefgh"
		;; > {s[$ $ 2] <- "0000"}
		;; > s
		;; "0b0d0f0h"
		((string? container-eval)
		 
		 (when (= 0 index3-or-keyword-or-step-eval)
		       (error "assignment : slice step cannot be zero"))
		 
		 (let* ((size-input (string-length container-eval))
			(i 0))
		   
		   (if (< index3-or-keyword-or-step-eval 0) ;; with negative index we start at end of string (like in Python)
		       (for ({k <+ {size-input - 1}} (>= k 0) (set! k (+ k index3-or-keyword-or-step-eval)))
			    (string-set! container-eval
					 k
					 (string-ref expr-eval i))
			    (set! i (+ 1 i)))
		       
		       (for ({k <+ 0} (< k size-input) (set! k (+ k index3-or-keyword-or-step-eval)))
			    (string-set! container-eval
					 k
					 (string-ref expr-eval i))
			    (set! i (+ 1 i)))))

		 container-eval

		 )

		(else (error "Slicing only for vector and string."))))
	 
	 


	 ;; T[i1 $ i3]
	 ;; {s <+ (string-append "abcdefgh")}
	 ;; "abcdefgh"
	 ;; > {s[2 $ 4] <- "zob"}
	 ;; > s
	 ;; "abzoefgh"
	 ;; > {s[2 $ 4] <- "zo"}
	 ;; > s
	 ;; "abzoefgh"
	 ((i1 (? (cut equal? <> slice)) i3) (container-copy! container-eval
							     i1
							     expr-eval
							     0
							     (- i3 i1))

	  container-eval

	  )
	 

	 ;; T[i1 i2 i3]
	 ((i1 i2 i3) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3))) ;;(array-n-dim-set! array value i1 i2)
	      (srfi25-array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval))

	  expr-eval  ;; returning a value allow the chaining : {T[3 5 6] <- A[4 2 3] <- T[7 2 4]}

	  )
	 
	 ) ;; end match

 
  
  )




(define (assignment-argument-4 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval)

  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
  
  {index1-or-keyword-eval-pos <+ index1-or-keyword-eval}
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}
  {index3-or-keyword-eval-pos <+ index3-or-keyword-eval}
  {index4-or-step-eval-pos <+ index4-or-step-eval}
  
  (declare container-length container-copy!)
  
  (if (vector? container-eval)
      ($>
       {container-length <- vector-length}
       {container-copy! <- vector-copy!})
      ($>  ;; a string
       {container-length <- string-length}
       {container-copy! <- string-copy!}))
  
  ;; transform the negative indexes in positive ones when not slices
  (when {(not (equal? index1-or-keyword-eval-pos slice)) and {index1-or-keyword-eval-pos < 0}}
	{index1-or-keyword-eval-pos <- (container-length container-eval) + index1-or-keyword-eval-pos})
  
  (when {(not (equal? index2-or-keyword-eval-pos slice)) and {index2-or-keyword-eval-pos < 0}}
	{index2-or-keyword-eval-pos <- (container-length container-eval) + index2-or-keyword-eval-pos})

  (when {(not (equal? index3-or-keyword-eval-pos slice)) and {index3-or-keyword-eval-pos < 0}}
	{index3-or-keyword-eval-pos <- (container-length container-eval) + index3-or-keyword-eval-pos})
  
  
  (when {(not (equal? index4-or-step-eval-pos slice)) and {index4-or-step-eval-pos < 0}}
	{index4-or-step-eval-pos <- (container-length container-eval) + index4-or-step-eval-pos})
  
  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-eval-pos index4-or-step-eval-pos)

	 ;; T[i1 $ i2 $]
	 ((i1 (? (cut equal? <> slice)) i2 (? (cut equal? <> slice))) {container-eval[i1 slice i2] <- expr-eval})
	 
	 ;; T[$ i2 $ s3]
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; '#(1 2 3 4 5 6 7 8 9)
	 ;; > {v[$ 6 $ 2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(-1 2 -2 4 -3 6 7 8 9)
	 (((? (cut equal? <> slice)) i2 (? (cut equal? <> slice)) step-not-used)

	  {step <+ index4-or-step-eval}
	  
	  (when {step = 0}
		(error "assignment : slice step cannot be zero"))
	  
	  {i <+ 0}
	  
	  (if {step < 0} ;; with negative index we start at end of vector (like in Python)
	      (for ({k <+ i2} {k >= 0} {k <- k + step})
		   {container-eval[k] <- expr-eval[i]}
		   {i <- i + 1})
	      
	      (for ({k <+ 0} {k < i2} {k <- k + step})
		   {container-eval[k] <- expr-eval[i]}
		   {i <- i + 1}))

	  container-eval
	  )
	 
	 
	 ;; T[i1 $ $ s3]
	 ((i1 (? (cut equal? <> slice)) (? (cut equal? <> slice)) step-not-used)

	  {step <+ index4-or-step-eval}

	  ;; > {s <+ (string-append "abcdefgh")}
	  ;; "abcdefgh"
	  ;; {s[3 $ $ 2] <- "0000"}
	  ;; > s
	  ;; "abc0e0g0"
	  
	  (when (= 0 step)
		(error "assignment : slice step cannot be zero"))
	  
	  (let* ((size-input (vector-length container-eval))
		 (i 0))
	    
	    (if (< step 0) ;; with negative index we start at end of vector (like in Python)
		(for ((define k (- size-input 1)) (>= k i1) (set! k (+ k step)))
		     (vector-set! container-eval
				  k
				  (vector-ref expr-eval i))
		     (set! i (+ 1 i)))
		
		(for ({k <+ i1} {k < size-input} {k <- k + step})
		     (vector-set! container-eval
				  k
				  (vector-ref expr-eval i))
		     {i <- 1 + i})))


	  container-eval
	  )
	 
	 
	 
	 ;; T[i1 i2 i3 i4]
	 ((i1 i2 i3 i4) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4))) ;;(array-n-dim-set! array value i1 i2)
	      (srfi25-array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval))

	  expr-eval  ;; returning a value allow the chaining : {T[3 5 6 2] <- A[4 2 3] <- T[2 5]}
	  )
	 
	 ) ;; end match

  
  
  )



(define (assignment-argument-5 container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval)

  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
 
  {index1-eval-pos <+ index1-eval}
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}
  {index3-eval-pos <+ index3-eval}
  {index4-or-keyword-eval-pos <+ index4-or-keyword-eval}
  {index5-or-step-eval-pos <+ index5-or-step-eval}

  (declare container-length container-copy!)
  
  (if (vector? container-eval)
      ($>
       {container-length <- vector-length}
       {container-copy! <- vector-copy!})
      ($>  ;; a string
       {container-length <- string-length}
       {container-copy! <- string-copy!}))
  
  ;; transform the negative indexes in positive ones when not slices
  (when {(not (equal? index1-eval-pos slice)) and {index1-eval-pos < 0}}
	{index1-eval-pos <- (container-length container-eval) + index1-eval-pos})
  
  (when {(not (equal? index2-or-keyword-eval-pos slice)) and {index2-or-keyword-eval-pos < 0}}
	{index2-or-keyword-eval-pos <- (container-length container-eval) + index2-or-keyword-eval-pos})

  (when {(not (equal? index3-eval-pos slice)) and {index3-eval-pos < 0}}
	{index3-eval-pos <- (container-length container-eval) + index3-eval-pos})
  
  
  (when {(not (equal? index4-or-keyword-eval-pos slice)) and {index4-or-keyword-eval-pos < 0}}
	{index4-or-keyword-eval-pos <- (container-length container-eval) + index4-or-keyword-eval-pos})

  (when {(not (equal? index5-or-step-eval-pos slice)) and {index5-or-step-eval-pos < 0}}
	{index5-or-step-eval-pos <- (container-length container-eval) + index5-or-step-eval-pos})
  
  
  
  (match (list index1-eval-pos index2-or-keyword-eval-pos index3-eval-pos index4-or-keyword-eval-pos index5-or-step-eval-pos)

	 ;; T[i1 $ i2 $ step]
	 ;;  {s <+ (string-append "abcdefgh")}
	 ;; "abcdefgh"
	 ;; > {s[2 $ 7 $ 2] <- "0000"}
	 ;; > s
	 ;; "ab0d0f0h"
	 ((i1 (? (cut equal? <> slice)) i2 (? (cut equal? <> slice)) step-not-used)
	  
	  {step <+ index5-or-step-eval}

	  (copy-stepped-slice container-eval expr-eval i1 i2 step)

	  container-eval)
	 
	 
	 ;; T[i1 i2 i3 i4 i5]
	 ((list i1 i2 i3 i4 i5) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4 i5))) ;;(array-n-dim-set! array expr-eval i1 i2)
	      (srfi25-array-set! container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval))

	  expr-eval  ;; returning a value allow the chaining : {T[3 5 6 2 1] <- A[4 2 3] <- T[2 2 4 6 7]}
	  )

	 ) ;; match

  
  
  ) 



(define (assignment-argument-6-and-more container expr args)

  (when (not {(vector? container) or (array? container)})
	     (error "assignment : container type not compatible : " container))
    
  (if (vector? container)
      (function-array-n-dim-set! container expr (reverse args)) ;; (array-n-dim-set! array value index1 index2 ...)
      (srfi25-array-set! container (list->vector args) expr))
  
  expr)  ;; returning a value allow the chaining : {T[3 5 6 2 1] <- A[4 2 3] <- T[2 2 4 6 7]}
