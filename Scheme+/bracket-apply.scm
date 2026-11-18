;; Translated from Scheme+ for Racket (bracket-apply.rkt) for Guile.


;; for Racket

;; This file is part of Scheme+

;; Copyright 2021-2025 Damien MATTEI

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



(define-module (Scheme+ bracket-apply)
  #:use-module (Scheme+ racket-compat))


  

  (use-modules
	(srfi srfi-43)
	((srfi srfi-1) #:select (first second third fourth fifth))
	(srfi srfi-69)
	(Scheme+ parse-square-brackets)
	(Scheme+ for_next_step)
	(Scheme+ array)
	(Scheme+ slice)
	(Scheme+ overload)
	(Scheme+ block))


  
;; SRFI 105 : Curly-infix-expressions in conjunction with specialized $bracket-apply$
;; of Scheme+R6RS allows a syntax like {Container[index]} with vectors
;; and arrays of any dimensions,size and shape and hash tables

;; USELESS must be passed directly by the syntax transformer
#;(define oper-equiv-symbol-syntax (list (cons '* #'orig-*)
				       (cons '- #'orig--)
				       (cons '+ #'orig-+)
				       (cons '/ #'orig-/)))  

;; (define T (make-vector 5))
;; (vector-set! T 3 7)
;; scheme@(guile-user)> {T[3]}
;; $3 = 7
;; {T[3] <- 7}
;; 7

;; ($bracket-apply$ #(0 1 2 3 4) 2)

  
;; scheme@(guile-user)> (define a (make-array 999 '(1 2) '(3 4)))
;; scheme@(guile-user)> (array-ref a 2 4)
;; $3 = 999

;; scheme@(guile-user)> {a[2 4]}
;; $9 = 999

;; scheme@(guile-user)> (define b (make-array 'ho 3))
;; scheme@(guile-user)> (array-ref b 1)
;; $13 = ho

;; scheme@(guile-user)> {b[2]}
;; $15 = ho

;; scheme@(guile-user)> {a[2 4] <- 7}
;; scheme@(guile-user)> {a[2 4]}
;; $19 = 7
;; scheme@(guile-user)> {a[1 3] <- 5}
;; scheme@(guile-user)> {a[1 3] <- a[2 4]}
;; scheme@(guile-user)> {a[1 3]}
;; $20 = 7



;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 : 3 * 5 - 10]}
;; '#(3 4 5)

;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 : 3 * 5 - 10 : 2 * 4 - 6]}
;; '#(3 5)

;; (bracket-apply #(0 1 2 3 4) 2)
;; bracket-apply : #'parsed-args=(#<syntax#1399 list in #2342> 2)

;; 2

;; #|kawa:7|# (define (foo) (bracket-apply #(0 1 2 3 4) 2))
;; bracket-apply : #'parsed-args=(#<syntax#1402 list in #2350> 2)
;; #|kawa:8|# (foo)
  ;; 2

;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > ($bracket-apply$ (vector 0 1 2 3 4) 2)

;; bracket-apply : #'parsed-args={.#<syntax:Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/bracket-apply.scm:126:30 list> .#<syntax:72-interactions from an unsaved editor:8:38 2>}
  ;; 2


;; > ($bracket-apply$ (vector 0 1 2 3 4) 2 + 1)

;; bracket-apply : #'parsed-args={.#<syntax:Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/bracket-apply.scm:126:30 list> {.#<syntax:72-interactions from an unsaved editor:12:40 +> .#<syntax:72-interactions from an unsaved editor:12:38 2> .#<syntax:72-interactions from an unsaved editor:12:42 1>}}
  ;; 3



;; > (define x 1)
;; > ($bracket-apply$ (vector 0 1 2 3 4) 2 + x)

;; bracket-apply : #'parsed-args={.#<syntax:Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/bracket-apply.scm:126:30 list> {.#<syntax:72-interactions from an unsaved editor:17:40 +> .#<syntax:72-interactions from an unsaved editor:17:38 2> .#<syntax:72-interactions from an unsaved editor:17:42 x>}}
;; 3
  
(define-syntax $bracket-apply$  ;;  this implements a possible bracket-apply as proposed in SRFI-105
  
  (lambda (stx)
    
    (syntax-case stx ()

      ;; a version that pre-compil the infix expression, should be faster
      (($bracket-apply$ container arg-bracket ...) ;  . args-brackets

      
       (with-syntax ((parsed-args

		      (cons #'list
			    ; TODO in the doc explain that if there is a need of overload operator between [ expr ]
                            ; then the expression must be between { } :  [{expr}]
		            (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
									   (list (cons '* #'fx*)
										 (cons '- #'fx-)
										 (cons '+ #'fx+)
										 (cons '/ #'fxquotient))))))
	 
	 ;;(newline)
	 ;;(display "bracket-apply : #'parsed-args=") (display #'parsed-args) (newline)
	 ;;(display "bracket-apply : (syntax->datum #'parsed-args)=") (display (syntax->datum #'parsed-args)) (newline)

	 ;; when trying to put this 'case in a subroutine Racket complains of #'parsed-args being outside templates.
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args))) ; we remove 'list from the length of syntax list (list arg1 arg2 ...) to get (arg1 arg2 ...)

	   ((0) #'(apply-square-brackets-argument-0 container))
	   
	   ;; 1 argument in [ ]
	   ;; T[index]
	   ;; the access to a vector by an index (not a slice) is written inline : an appel of procedure (in another module ,
	   ;; never inlined) cost 45 % in a product of matrices , the test of the sign of the index only a few %
	   ((1) #;#'(apply-square-brackets-argument-1 container
						      (first parsed-args))
		(with-syntax ((index (cadr (syntax->list #'parsed-args))))
		  #'(let ((c container) (i index))
		      (cond ((and (vector? c) (fixnum? i)) ; a vector and an index , not a slice
			     (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))) ; a negative index counts from the end as in Python
			    ((and (string? c) (fixnum? i)) ; a string and an index
			     (string-ref c (if (fx< i 0) (fx+ (string-length c) i) i)))
			    (else (apply-square-brackets-argument-1 c i))))))
	   ;; 2 arguments in [ ]
	   ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
	   
	   ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
	   ;; '#(3 4 5)
	   ;; M[i j] of a vector of vectors written inline , each negative index counts from the end of its own vector
	   ((2) #;#'(apply-square-brackets-argument-2 container
						      (first parsed-args)
						      (second parsed-args))
		(with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
		  #'(let ((c container) (i index1) (j index2))
		      (cond ((and (vector? c) (fixnum? i) (fixnum? j)) ; a vector of vectors and two indexes , not slices
			     (let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
			       (vector-ref row (if (fx< j 0) (fx+ (vector-length row) j) j))))
			    #;((and (flomat? c) (fixnum? i) (fixnum? j)) ; flomat : Racket only
			     (ref c i j))
			    (else (apply-square-brackets-argument-2 c i j))))))

	   ;; 3 arguments in [ ]
	   ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
	   ((3) #'(apply-square-brackets-argument-3 container
						    (first parsed-args)
						    (second parsed-args)
						    (third parsed-args)))

	   ;; 4 arguments in [ ]
	   ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
	   ((4) #'(apply-square-brackets-argument-4 container
						    (first parsed-args)
						    (second parsed-args)
						    (third parsed-args)
						    (fourth parsed-args)))

	   ;; 5 arguments in [ ]
	   ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
	   ((5) #'(apply-square-brackets-argument-5 container
						    (first parsed-args)
						    (second parsed-args)
						    (third parsed-args)
						    (fourth parsed-args)
						    (fifth parsed-args)))
	   ;; more than 5 arguments in [ ]
	   ;; T[i1 i2 i3 i4 i5 i6 ...]
	   (else #'(apply-square-brackets-argument-6-and-more container parsed-args)))

	 )))))



;; written by the type annotation pass for an object whose type matches square brackets overloaded with
;; overload-square-brackets : ($bracket-apply-overloaded$ ((getter setter nb-indexes) ...) container arg-bracket ...)
;; example : ($bracket-apply-overloaded$ ((matrix-vect-ref matrix-vect-set! 2) (matrix-vect-line-ref matrix-vect-line-set! 1)) Mv i + 1 j)
;;           --> (matrix-vect-ref Mv (fx+ i 1) j)
;; the getter is chosen here from the number of indexes ; when it can not be chosen (no overload or more than one with
;; this number of indexes , or a slice) it is the generic $bracket-apply$ , that chooses at runtime
(define-syntax $bracket-apply-overloaded$

  (lambda (stx)

    (syntax-case stx ()

      (($bracket-apply-overloaded$ (ovrld ...) container arg-bracket ...)

       (let* ((parsed-args (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
									    (list (cons '* #'fx*)
										  (cons '- #'fx-)
										  (cons '+ #'fx+)
										  (cons '/ #'fxquotient))))
	      (ovrld-found (select-overloaded-square-brackets (map syntax->list (syntax->list #'(ovrld ...)))
							      parsed-args)))

	 (if ovrld-found
	     (with-syntax ((getter (car ovrld-found))
			   ((index ...) parsed-args))
	       #'(getter container index ...))
	     #'($bracket-apply$ container arg-bracket ...)))))))








(define (apply-square-brackets-argument-0 container-eval)


  ;; why a copy???? why not the original object????
  (cond ((vector? container-eval)
	 (vector-copy container-eval)) ;; return a copy of vector

	
	((hash-table? container-eval) (hash-table->alist container-eval)) ;; return the elements of hash table
	
	((string? container-eval)  (string-copy container-eval)) ;; return a copy of the string
				   	
	(else ;; array of SRFI 25	     
	   container-eval))) ;; return the array (no copy procedure in SRFI 25 but exists in Guile)
 


;; {T[:]}
;; '#(1 2 3)
(define (apply-square-brackets-argument-1 container-eval index-eval)
  
  ;; {#(1 2 3 4 5)[-2]}
  ;; 4
  
  ;; {T <+ (vector 1 2 3)}
  ;; '#(1 2 3)
  ;; > {T1 <+ T}
  ;; '#(1 2 3)
  ;; > {T1[1] <- 7}
  ;; 7
  ;; > T
  ;; '#(1 7 3)
  ;; > {T2 <+ T[:]}
  ;; '#(1 7 3)
  ;; > {T2[1] <- 0}
  ;; 0
  ;; > T
  ;; '#(1 7 3)
  ;; > T2
  ;; '#(1 0 3)
  (cond ((vector? container-eval)

	 (if (eq? slice index-eval) ;; T[:] 
	     (vector-copy container-eval) ;; return a copy of vector
	     (if (< index-eval 0) ;; negative index as in Python
		 (vector-ref container-eval (+ (vector-length container-eval) index-eval)) ;; negative indexing
		 (vector-ref container-eval index-eval)))) ;; return an element of the vector

	((hash-table? container-eval) (hash-table-ref container-eval index-eval))
	
	;; sometimes i'm impress by Scheme :o
	;;  {"toto"[2]}
	;; #\t
	;; {"toto"[-1]}
	;; #\o
	((string? container-eval) (if (eq? slice index-eval) ;; T[:] 
				      (string-copy container-eval) ;; return a copy of the string
				      (if (< index-eval 0) ;; negative index as in Python
					  (string-ref container-eval (+ (string-length container-eval) index-eval)) ;; negative indexing
					  (string-ref container-eval index-eval)))) ;; return an element of the string

	#;((flomat? container-eval) (if (eq? slice index-eval) ;; T[:] 
				      (error "apply-square-brackets.* :  slice not allowed with flomat")
				      (row container-eval index-eval)))

	;; array of SRFI 25
	((array? container-eval) ;; T[i1] ,  1 dimension array
	 ;;(display "apply-square-brackets.* : array case : quoted container-eval = ") (display (quote container-eval)) (newline)
	 ;;(display "apply-square-brackets.* : array case : container-eval = ") (display container-eval) (newline)
	 (array-ref container-eval index-eval)) ;; return the element of the array
	
	(else
	 (begin-def
	  (define args-lst (list container-eval index-eval))
	  (define proc (find-getter-for-overloaded-square-brackets args-lst))
	  (apply proc args-lst))))

  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero

)


(define (apply-square-brackets-argument-2 container-eval index1-or-keyword-eval index2-or-keyword-eval)

  (cond ((vector? container-eval) ;; 2 dimension vector ? or 1 dimension vector : T[i1 :] , T[: i2]


	 ;; {#(1 2 3 4 5)[2 :]}
	 ;; '#(3 4 5)
	 ;;
	 ;; {#(1 2 3 4 5)[: 3]}
	 ;; '#(1 2 3)
	 
	 (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: :]
		     (eq? slice index2-or-keyword-eval))
		container-eval)
	       
	       ((eq? slice index1-or-keyword-eval) ;; T[: i2]
		(if (< index2-or-keyword-eval 0) ;; negative index
		    (vector-copy container-eval 0 (+ (vector-length container-eval) index2-or-keyword-eval))
		    (vector-copy container-eval 0 index2-or-keyword-eval)))
	       
	       ((eq? slice index2-or-keyword-eval) ;; T[i1 :]
		(if (< index1-or-keyword-eval 0) ;; negative index
		    (vector-copy container-eval (+ (vector-length container-eval) index1-or-keyword-eval))
		    (vector-copy container-eval index1-or-keyword-eval)))
	       
	       (else ;; T[i1 i2] vector of vectors
		(function-array-n-dim-ref container-eval (reverse (list index1-or-keyword-eval index2-or-keyword-eval))))))
	;;(array-n-dim-ref container index1-or-keyword-eval index2-or-keyword-eval)


	;; {"hello"[: 2]}
	;; "he"
	;; {"hello"[3 :]}
	;; "lo"

	;; {"hello"[: :]}
	;; "hello"
	;; {"hello"[:]}
	;; "hello"
	((string? container-eval) (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: :]
					      (eq? slice index2-or-keyword-eval))
					 container-eval)
					
					((eq? slice index1-or-keyword-eval) ;; T[: i2]
					 (if (< index2-or-keyword-eval 0) ;; negative index
					     (substring container-eval 0 (+ (string-length container-eval) index2-or-keyword-eval))
					     (substring container-eval 0 index2-or-keyword-eval)))

					((eq? slice index2-or-keyword-eval) ;; T[i1 :]
					 (if (< index1-or-keyword-eval 0) ;; negative index
					     (substring container-eval (+ (string-length container-eval) index1-or-keyword-eval))
					     (substring container-eval index1-or-keyword-eval)))
					
					(else ;; syntax error
					 (error "apply-square-brackets-argument-2 : bad arguments in string case,expecting : i2 or i1 :, provided :"
						index1-or-keyword-eval index2-or-keyword-eval) )))

	#;((flomat? container-eval) (if (or (eq? slice index1-or-keyword-eval)
					  (eq? slice index2-or-keyword-eval))
				      (error "apply-square-brackets.* :  slice not allowed with flomat")
				      (ref container-eval index1-or-keyword-eval index2-or-keyword-eval)))
	
	 
	((array? container-eval) ;; T[i1 i2] ,  2 dimension array
	 (array-ref container-eval index1-or-keyword-eval index2-or-keyword-eval)) ;; return the element of the array
	;; note : i do not use negative indexes or slices for array because they could be not starting at zero

	((hash-table? container-eval) (error "hash table used with [ ] and more than 1 argument")) 

	;; 	> matrix-vect?
	;; #<procedure:matrix-vect?>
	;; >  (overload-square-brackets matrix-vect-ref
	;; 	 matrix-vect-set!  (matrix-vect? number? number?))
	;; > $ovrld-square-brackets-lst$
	;; '(((#<procedure:matrix-vect?> #<procedure:number?> #<procedure:number?>) (#<procedure:matrix-vect-ref> . #<procedure:matrix-vect-set!>)))
	;; > (define Mv (matrix-vect #(#(1 2 3) #(4 5 6))))
	;; > Mv
	;; #<matrix-vect>
	;; > (matrix-vect? Mv)
	;; #t
	;; > (find-getter-for-overloaded-square-brackets (list Mv 1 0))
	;; #<procedure:matrix-vect-ref>
	;; > {Mv[1 0]}
	;; 4
	(else
	 (begin-def
	  (define args-lst (list container-eval index1-or-keyword-eval index2-or-keyword-eval))
	  (define proc (find-getter-for-overloaded-square-brackets args-lst))
	  (apply proc args-lst))))

  )

;; todo: made one dimension overload too (note: seems already done! -> todo: remove todos)



(define (apply-square-brackets-argument-3 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)

  ;; {#(1 2 3 4 5 6 7)[2 : 5]}
  ;; '#(3 4 5)
  ;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 : 3 * 5 - 10]}
  ;; '#(3 4 5)
  (cond ((vector? container-eval) ;; 3 dimension vector T[i1 i2 i3]? or T[i1 : i3] or T[: : step] or  T[: i2 :] or  T[i1 : :]

	 ;; {#(1 2 3 4 5 6 7 8)[: : 3]}
	 ;; '#(1 4 7)

	 ;; {#(1 2 3 4 5 6 7 8)[: : -2]}
	 ;; '#(8 6 4 2)
	 (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: : step]
		     (eq? slice index2-or-keyword-eval))

		(when (= 0 index3-or-keyword-or-step-eval)
		      (error "apply-square-brackets-argument-3 : slice step cannot be zero"))
		
		(let* ((size-input (vector-length container-eval))
		       (size (quotient size-input (abs index3-or-keyword-or-step-eval)))
		       (result '())
		       (i 0))
		  
		  (when (not (= (modulo size-input index3-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  (set! result (make-vector size))

		  (if (< index3-or-keyword-or-step-eval 0) ;; with negative index we start at end of vector (like in Python)
		      (for ((define k (- size-input 1)) (>= k 0) (set! k (+ k index3-or-keyword-or-step-eval)))
			   (vector-set! result
					i
					(vector-ref container-eval k))
			   (set! i (+ 1 i)))
		      (for ((define k 0) (< k size-input) (set! k (+ k index3-or-keyword-or-step-eval)))
			   (vector-set! result
					i
					(vector-ref container-eval k))
			   (set! i (+ 1 i))))
		  
		  result))
	       

	       ((eq? slice index2-or-keyword-eval) ;; T[i1 : i3]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
		
		(when (< index3-or-keyword-or-step-eval 0) ;; negative index
		      (set! index3-or-keyword-or-step-eval (+ (vector-length container-eval) index3-or-keyword-or-step-eval)))
		
		(vector-copy container-eval index1-or-keyword-eval index3-or-keyword-or-step-eval))


	       ((eq? slice index2-or-keyword-eval) ;; T[i1 : :]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
		
		(vector-copy container-eval index1-or-keyword-eval))
	       
	       
	       ((and (eq? slice index1-or-keyword-eval)  (eq? slice index3-or-keyword-or-step-eval)) ;; T[: i2 :]
		
		(when (< index2-or-keyword-eval 0) ;; negative index
		      (set! index2-or-keyword-eval (+ (vector-length container-eval) index2-or-keyword-eval)))
		
		(vector-copy container-eval 0 index3-or-keyword-or-step-eval))

	       
	       ;; T[i1 i2 i3] vector of vectors of vectors
	       (else
		(function-array-n-dim-ref container-eval
					  (list index3-or-keyword-or-step-eval index2-or-keyword-eval index1-or-keyword-eval))))) ;; was reverse list ...
	;;or use array-n-dim-ref macro


	;; {"elephant"[2 : 5]}
	;; "eph"
	;;  {"abcdefghijkl"[: : 2]}
	;; "acegik"
	;; {"abcdefghijkl"[: : -3]}
	;; "lifc"
	;; {"123456789"[ :  : -1]}
	;; "987654321"
	((string? container-eval) ;;  T[: : step] or T[i1 : i3] or error
	 
	 (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: : step]
		     (eq? slice index2-or-keyword-eval))

		(when (= 0 index3-or-keyword-or-step-eval)
		      (error "apply-square-brackets-argument-3 : slice step cannot be zero"))
		
		(let* ((size-input (string-length container-eval))
		       (size (quotient size-input (abs index3-or-keyword-or-step-eval)))
		       (result '())
		       (i 0))
		  
		  (when (not (= (modulo size-input index3-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  
		  (set! result (make-string size))

		  (if (< index3-or-keyword-or-step-eval 0) ;; with negative index we start at end of vector (like in Python)
		      (for ((define k (- size-input 1)) (>= k 0) (set! k (+ k index3-or-keyword-or-step-eval)))
			   (string-set! result
					i
					(string-ref container-eval k))
			   (set! i (+ 1 i)))
		      (for ((define k 0) (< k size-input) (set! k (+ k index3-or-keyword-or-step-eval)))
			   (string-set! result
					i
					(string-ref container-eval k))
			   (set! i (+ 1 i))))
		  
		  result))
	       
	       ((eq? slice index2-or-keyword-eval) ;; T[i1 : i3]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (string-length container-eval) index1-or-keyword-eval)))
		
		(when (< index3-or-keyword-or-step-eval 0) ;; negative index
		      (set! index3-or-keyword-or-step-eval (+ (string-length container-eval) index3-or-keyword-or-step-eval)))
		
		(substring container-eval index1-or-keyword-eval index3-or-keyword-or-step-eval))


	       
	       ((eq? slice index2-or-keyword-eval) ;; T[i1 : :]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (string-length container-eval) index1-or-keyword-eval)))
		
		(string-copy container-eval index1-or-keyword-eval))
	       
	       

	       ((and (eq? slice index1-or-keyword-eval)  (eq? slice index3-or-keyword-or-step-eval)) ;; T[: i2 :]
		
		(when (< index2-or-keyword-eval 0) ;; negative index
		      (set! index2-or-keyword-eval (+ (string-length container-eval) index2-or-keyword-eval)))
		
		(string-copy container-eval 0 index3-or-keyword-or-step-eval))

	       

	       (else
		(error "apply-square-brackets-argument-3 : in string case, provided too much arguments:"
		       index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval))))

	((hash-table? container-eval) (error "hash table used with [ ] and more than 1 argument"))

	#;((flomat? container-eval) (error "apply-square-brackets.* :  flomat used with 3 arguments"))
			
	
	(else ;; T[i1 i2 i3] ,  3 dimension array
	 (array-ref container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)))) ;; return the element of the array
  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero



;; {"123456789"[3 :  : -2]}
;; "42"
;; {(vector 1 2 3 4 5)[2 : : -1]}
;;'#(3 2 1)
;; {(vector 1 2 3 4 5)[2 : : 1]}
;;'#(3 4 5)
(define (apply-square-brackets-argument-4 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)

  
  (cond ((vector? container-eval)

	 ;; {#(1 2 3 4 5 6 7 8 9)[: 7 : 2]}
	 ;; '#(1 3 5 7)
	 ;; {#(1 2 3 4 5 6 7 8 9)[: 6 : -1]}
	 ;; '#(6 5 4 3 2 1)
	 ;; {#(1 2 3 4 5 6 7 8 9)[: 6 : -2]}
	 ;; '#(6 4 2)
	 ;; {#(1 2 3 4 5 6 7 8 9)[: -3 : -2]}
	 ;; '#(6 4 2)
	 (cond ((and (eq? slice index1-or-keyword-eval)  ;; T[: i2 : s]
		     (eq? slice index3-or-keyword-eval))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "apply-square-brackets-argument-4 : slice step cannot be zero"))
		
		(let* ((size 0) ;; result size
		       (result '())
		       (i 0))

		  (when (< index2-or-keyword-eval 0) ;; negative index
			(set! index2-or-keyword-eval (+ (vector-length container-eval) index2-or-keyword-eval)))

		  (set! size (quotient index2-or-keyword-eval (abs index4-or-keyword-or-step-eval)))
		  
		  ;;(displayln size)
		  (when (not (= (modulo index2-or-keyword-eval index4-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  
		  ;;(displayln size)
		  (set! result (make-vector size))
		  
		  (if (< index4-or-keyword-or-step-eval 0) 
		      
		      (for ((define k (- index2-or-keyword-eval 1)) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
			   (vector-set! result
					i
					(vector-ref container-eval k))
			   (set! i (+ 1 i)))
		      
		      (for ((define k 0) (< k index2-or-keyword-eval) (set! k (+ k index4-or-keyword-or-step-eval)))
			   (vector-set! result
					i
					(vector-ref container-eval k))
			   (set! i (+ 1 i))))
		  
		  result))



	       ;; {#(1 2 3 4 5 6 7 8 9)[3 : : 2]}
	       ;; '#(4 6 8)
	       ;; > {#(1 2 3 4 5 6 7 8 9)[3 : : -2]}
	       ;; '#(4 2)
	       ;; > {#(1 2 3 4 5 6 7 8 9)[-3 : : 2]}
	       ;; '#(7 9)
	       ;; {#(1 2 3 4 5 6 7 8 9)[-3 : : -2]}
	       ;; '#(7 5 3 1)
	       ((and (eq? index2-or-keyword-eval slice)  ;; T[i1 : : s]
		     (eq? index3-or-keyword-eval slice))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "apply-square-brackets-argument-4 : slice step cannot be zero"))
		
		(let* ((size-container-eval (vector-length container-eval))
		       (i1 (if (< index1-or-keyword-eval 0) ;; negative index
			       (+ size-container-eval index1-or-keyword-eval)
			       index1-or-keyword-eval))
		       (size-input (if (> index4-or-keyword-or-step-eval 0)
				       (- size-container-eval i1)
				       (+ i1 1)))
		       (size (quotient size-input (abs index4-or-keyword-or-step-eval))) ;; result size
		       (result '())
		       (i 0))
		  
		  (when (not (= (modulo size-input index4-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  (set! result (make-vector size))

		  (if (< index4-or-keyword-or-step-eval 0) 
		      
		      (for ((define k i1) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
			   (vector-set! result
					i
					(vector-ref container-eval k))
			   (set! i (+ 1 i)))
		      
		      (for ((define k i1) (< k size-container-eval) (set! k (+ k index4-or-keyword-or-step-eval)))
			   ;;(displayln k)
			   (vector-set! result
					i
					(vector-ref container-eval k))
			   (set! i (+ 1 i))))
		  
		  result))


	       ((and (eq? index2-or-keyword-eval slice) ;; T[i1 : i3 :]
		     (eq? index4-or-keyword-or-step-eval slice))

		(let ((i1 index1-or-keyword-eval)
		      (i3 index3-or-keyword-eval))
		  (when (< i1 0) ;; negative index
			(set! i1 (+ (vector-length container-eval) i1)))
		  (when (< i3 0) ;; negative index
			(set! i3 (+ (vector-length container-eval) i3)))
		  (vector-copy container-eval i1 i3)))

	       
	       ;; T[i1 i2 i3 i4] vector of vectors of vectors ...
	       (else		
		(function-array-n-dim-ref container-eval (list index4-or-keyword-or-step-eval index3-or-keyword-eval index2-or-keyword-eval index1-or-keyword-eval))))) ;; was reverse list ...
	;;or use array-n-dim-ref macro

	
	
	;; {"123456789"[: -3 : -2]}
	;; "642"
	((string? container-eval) 

	 ;; {"abcdefghijklmno"[: 7 : 2]}
	 ;; "aceg"
	 ;; > {"123456789"[: -3 : -2]}
	 ;; "642"
	 (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: i2 : s]
		     (eq? slice index3-or-keyword-eval))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "apply-square-brackets-argument-4 : slice step cannot be zero"))
		
		(let* ((size 0)
		       (result '())
		       (i 0)
		       (i2 index2-or-keyword-eval))

		  (when (< i2 0) ;; negative index
			(set! i2 (+ (string-length container-eval) i2)))

		  (set! size (quotient i2 (abs index4-or-keyword-or-step-eval)))
		  
		  (when (not (= (modulo i2 index4-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  (set! result (make-string size))

		  (if (< index4-or-keyword-or-step-eval 0)
		      
		      (for ((define k (- i2 1)) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
			   (string-set! result
					i
					(string-ref container-eval k))
			   (set! i (+ 1 i)))
		      
		      (for ((define k 0) (< k i2) (set! k (+ k index4-or-keyword-or-step-eval)))
			   (string-set! result
					i
					(string-ref container-eval k))
			   (set! i (+ 1 i))))
		  
		  result))


	       ;; {"abcdefghijklmno"[3 : : 2]}
	       ;; "dfhjln"
	       ;; > {"123456789"[3 :  : 2]}
	       ;; "468"
	       ;; > {"123456789"[3 :  : -2]}
	       ;; "42"
	       ;; > {"123456789"[-3 :  : -2]}
	       ;; "7531"
	       ;; > {"123456789"[-3 :  : 2]}
	       ;; "79"
	       ((and (eq? index2-or-keyword-eval slice)  ;; T[i1 : : s]
		     (eq? index3-or-keyword-eval slice))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "apply-square-brackets-argument-4 : slice step cannot be zero"))
		
		(let* ((size-container-eval (string-length container-eval))
		       (i1 (if (< index1-or-keyword-eval 0) ;; negative index
			       (+ size-container-eval index1-or-keyword-eval)
			       index1-or-keyword-eval))
		       (size-input (if (> index4-or-keyword-or-step-eval 0)
				       (- size-container-eval i1)
				       (+ i1 1)))
		       (size (quotient size-input (abs index4-or-keyword-or-step-eval)))
		       (result '())
		       (i 0))
		  
		  (when (not (= (modulo size-input index4-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  (set! result (make-string size))

		  (if (< index4-or-keyword-or-step-eval 0) 
		      
		      (for ((define k i1) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
			   (string-set! result
					i
					(string-ref container-eval k))
			   (set! i (+ 1 i)))
		      
		      (for ((define k i1) (< k size-container-eval) (set! k (+ k index4-or-keyword-or-step-eval)))
			   ;;(displayln k)
			   (string-set! result
					i
					(string-ref container-eval k))
			   (set! i (+ 1 i))))
		  
		  result))
	       
	       
	       ((and (eq? slice index2-or-keyword-eval) ;; T[i1 : i3 :] 
		     (eq? slice index4-or-keyword-or-step-eval))

		(let ((i1  index1-or-keyword-eval)
		      (i3 index3-or-keyword-eval))
		  (when (< i1 0) ;; negative index
			(set! i1 (+ (string-length container-eval) i1)))
		  (when (< i3 0) ;; negative index
			(set! i3 (+ (string-length container-eval) i3)))
		  (substring container-eval i1 i3)))

	       
	       ;; T[i1 i2 i3 i4] vector of vectors of vectors ... but we are in string context !!!
	       (else	
		(error "apply-square-brackets-argument-4 : in string case, provided too much arguments:"
		       index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval))))

	((hash-table? container-eval) (error "hash table used with [ ] and more than 1 argument"))

	#;((flomat? container-eval) (error "apply-square-brackets.* :  flomat used with 4 arguments"))
	
	(else ;; T[i1 i2 i3 i4] ,  4 dimension array
	 (array-ref container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)))) ;; return the element of the array
  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero




(define (apply-square-brackets-argument-5 container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)

  ;; {#(1 2 3 4 5 6 7 8 9)[2 : 5 : 1]}
  ;; '#(3 4 5)
  ;; {#(1 2 3 4 5 6 7 8 9)[5 : 2 : -1]}
  ;; '#(6 5 4)
  ;; {#(1 2 3 4 5 6 7 8 9)[2 : 5 : -1]}
  ;; '#()
  ;; {#(1 2 3 4 5 6 7 8 9)[-1 : 5 : -1]}
  ;; '#(9 8 7)
  ;; {#(1 2 3 4 5 6 7 8 9)[-0 : 5 : -1]}
  ;; '#()
  (cond ((vector? container-eval)
	 
	 (if (and (eq? index2-or-keyword-eval slice)  ;; T[i1 : i3 : s]
		  (eq? index4-or-keyword-eval slice))

	     (begin
	       
	       (when (= 0 index5-or-step-eval)
		 (error "apply-square-brackets-argument-5 : slice step cannot be zero"))
	       
	       (let* ((size-container-eval (vector-length container-eval))
		      
		      (i1 (if (< index1-eval 0) ;; negative index
			      (+ size-container-eval index1-eval)
			      index1-eval))

		      (i3 (if (< index3-eval 0) ;; negative index
			      (+ size-container-eval index3-eval)
			      index3-eval))
		      
		      (size-input (if (> index5-or-step-eval 0)
				      (- i3 i1)
				      (- i1 i3)))
		      
		      (size (quotient size-input (abs index5-or-step-eval))) ;; result size
		      (result '())
		      (i 0))
		 
		 (when (not (= (modulo size-input index5-or-step-eval) 0))
		   (set! size (+ 1 size)))

		 (if (<= size 0)
		     
		     (make-vector 0)

		     (begin
		       (set! result (make-vector size))
		       
		       (if (< index5-or-step-eval 0) 
			   
			   (for ((define k i1) (> k i3) (set! k (+ k index5-or-step-eval))) ;; we do not include i1-th element
				;; i do not allow Python index over size of vector
				;; (when (>= k size-container)
				;; 	 (continue))
				
				(vector-set! result
					     i
					     (vector-ref container-eval k))
				(set! i (+ 1 i)))
			   
			   (for ((define k i1) (< k i3) (set! k (+ k index5-or-step-eval)))
				;;(displayln k)
				(vector-set! result
					     i
					     (vector-ref container-eval k))
				(set! i (+ 1 i))))
		       
		       result))))


	     ;; T[i1 i2 i3 i4 i5] vector of vectors of vectors ...
	     (function-array-n-dim-ref container-eval (list index5-or-step-eval
							    index4-or-keyword-eval
							    index3-eval
							    index2-or-keyword-eval
							    index1-eval)))) ;; was reverse list ...
	;;or use array-n-dim-ref macro

	
	
	;; {"0123456789"[5 : 2 : -1]}
	;; "543"
	;; {"0123456789"[5 :  : -1]}
	;; "543210"
	;; {"0123456789"[5 : 0 : -1]}
	;; "54321"
	((string? container-eval) 

	 (if (and (eq? index2-or-keyword-eval slice)  ;; T[i1 : i3 : s]
		  (eq? index4-or-keyword-eval slice))

	     (begin
	       
	       (when (= 0 index5-or-step-eval)
		 (error "apply-square-brackets-argument-5 : slice step cannot be zero"))
	       
	       (let* ((size-container-eval (string-length container-eval))
		      
		      (i1 (if (< index1-eval 0) ;; negative index
			      (+ size-container-eval index1-eval)
			      index1-eval))

		      (i3 (if (< index3-eval 0) ;; negative index
			      (+ size-container-eval index3-eval)
			      index3-eval))
		      
		      (size-input (if (> index5-or-step-eval 0)
				      (- i3 i1)
				      (- i1 i3)))
		      
		      (size (quotient size-input (abs index5-or-step-eval))) ;; result size
		      (result '())
		      (i 0))
		 
		 (when (not (= (modulo size-input index5-or-step-eval) 0))
		   (set! size (+ 1 size)))

		 (if (<= size 0)
		     
		     (make-string 0)

		     (begin
		       (set! result (make-string size))
		       
		       (if (< index5-or-step-eval 0) 
			   
			   (for ((define k i1) (> k i3) (set! k (+ k index5-or-step-eval))) ;; we do not include i1-th element
				;; i do not allow Python index over size of string
				;; (when (>= k size-container)
				;; 	 (continue))
				
				(string-set! result
					     i
					     (string-ref container-eval k))
				(set! i (+ 1 i)))
			   
			   (for ((define k i1) (< k i3) (set! k (+ k index5-or-step-eval)))
				;;(displayln k)
				(string-set! result
					     i
					     (string-ref container-eval k))
				(set! i (+ 1 i))))
		       
		       result))))

	     
	     ;; T[i1 i2 i3 i4 i5] vector of vectors of vectors ... but we are in string context !!!
	     
	     (error "apply-square-brackets-argument-5 : in string case, provided too much arguments:" index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)))


	((hash-table? container-eval) (error "hash table used with [ ] and more than 1 argument"))

	#;((flomat? container-eval) (error "apply-square-brackets.* :  flomat used with 5 arguments"))
	
	(else ;; T[i1 i2 i3 i4 i5] ,  5 dimension array
	 (array-ref container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)))) ;; return the element of the array
  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero



(define (apply-square-brackets-argument-6-and-more container-eval args)

  (cond ((hash-table? container-eval) (error "hash table used with [ ] and more than 1 argument"))
	((string? container-eval) (error "string used with [ ] and more than 5 arguments"))
	((vector? container-eval)
	 (function-array-n-dim-ref container-eval (reverse args)))
	#;((flomat? container-eval) (error "apply-square-brackets.* :  flomat used with more than 5 arguments"))
	(else
	 (array-ref container-eval (list->vector args)))))   ;; array SRFI 25



; vector procedures and macros

(define-syntax $bracket-apply-vector$  ;;  this implements a specialized bracket-apply for vectors
  
  (lambda (stx)
    
    (syntax-case stx ()

      ;; a version that pre-compil the infix expression, should be faster
      (($bracket-apply-vector$ container arg-bracket ...) ;  . args-brackets

      
       (with-syntax ((parsed-args

		      (cons #'list
                            
                            (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
                                                                           (list (cons '* #'fx*)
                                                                                 (cons '- #'fx-)
                                                                                 (cons '+ #'fx+)
                                                                                 (cons '/ #'fxquotient))))))

         ;; Warning : not sure we really seeing what object are really are (syntax vs list)
	 ;;(newline)
         ;(display "bracket-apply-vector : fx+ = ") (display fx+) (newline)
	 ;(display "bracket-apply-vector : #'parsed-args=") (display #'parsed-args) (newline)
	 ;(display "bracket-apply-vector : (syntax->datum #'parsed-args)=") (display (syntax->datum #'parsed-args)) (newline)

         ;(display "bracket-apply-vector : passed parsed-args.") (newline)

         ;(display "bracket-apply-vector : (length (cdr (syntax->datum #'parsed-args)))=") (display  (length (cdr (syntax->datum #'parsed-args)))) (newline)
                   
	 ;; when trying to put this 'case in a subroutine Racket complains of #'parsed-args being outside templates.
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args))) ; we remove 'list from the length of syntax list (list arg1 arg2 ...) to get (arg1 arg2 ...)

	   ((0) #'(apply-square-brackets-argument-0-vector container))
	   
	   ;; 1 argument in [ ]
	   ;; T[index]
	   ;; written inline (an appel of procedure cost 45 % in a product of matrices) : a literal index >= 0 is a plain
	   ;; vector-ref , else the test of the sign of the index (negative : from the end as in Python)
	   ((1) #;#'(apply-square-brackets-argument-1-vector container
                                                             (first parsed-args))
		(with-syntax ((index (cadr (syntax->list #'parsed-args))))
		  (let ((d (syntax->datum #'index)))
		    (if (and (fixnum? d) (>= d 0))
			#'(vector-ref container index)
			#'(let ((c container) (i index))
			    (if (fixnum? i) ; an index , not a slice
				(vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))
				(apply-square-brackets-argument-1-vector c i)))))))
	   ;; 2 arguments in [ ]
	   ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
	   
	   ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
	   ;; '#(3 4 5)
	   ;; M[i j] of a vector of vectors written inline , each negative index counts from the end of its own vector
	   ((2) #;#'(apply-square-brackets-argument-2-vector container
                                                             (first parsed-args)
                                                             (second parsed-args))
		(with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
		  #'(let ((c container) (i index1) (j index2))
		      (if (and (fixnum? i) (fixnum? j)) ; two indexes , not slices
			  (let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
			    (vector-ref row (if (fx< j 0) (fx+ (vector-length row) j) j)))
			  (apply-square-brackets-argument-2-vector c i j)))))

	   ;; 3 arguments in [ ]
	   ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
	   ((3) #'(apply-square-brackets-argument-3-vector container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)))

	   ;; 4 arguments in [ ]
	   ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
	   ((4) #'(apply-square-brackets-argument-4-vector container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)
                                                           (fourth parsed-args)))

	   ;; 5 arguments in [ ]
	   ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
	   ((5) #'(apply-square-brackets-argument-5-vector container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)
                                                           (fourth parsed-args)
                                                           (fifth parsed-args)))
	   ;; more than 5 arguments in [ ]
	   ;; T[i1 i2 i3 i4 i5 i6 ...]
	   (else #'(apply-square-brackets-argument-6-and-more-vector container parsed-args))))))))




;; The accesses written by the type annotation pass when the type of the indexes is known (no slice) : ℕ (natural) or ℤ
;; (integer). With ℕ there is no negative index : vector-ref / string-ref directly , no test at all ; with ℤ only the test
;; of the sign (a negative index counts from the end as in Python). Vectors : one index v[i] or two indexes M[i j] (a
;; vector of vectors , each negative index counts from the end of its own vector) ; strings : one index s[i].
;; Any other form goes to $bracket-apply-vector$ / $bracket-apply-string$.

(eval-when (expand load eval)
  ;; the indexes between [ ] , parsed as by $bracket-apply-vector$ (an infix expression in [ ] : {v[i + 1]})
  (define (typed-indexes arg-brackets)
    (parse-square-brackets-arguments-lister-syntax arg-brackets
                                                   (list (cons '* #'fx*)
                                                         (cons '- #'fx-)
                                                         (cons '+ #'fx+)
                                                         (cons '/ #'fxquotient)))))

(define-syntax $bracket-apply-vector-natural$
  (lambda (stx)
    (syntax-case stx ()
      ((_ container arg-bracket ...)
       (let ((indexes (typed-indexes (syntax->list #'(arg-bracket ...)))))
         (case (length indexes)
           ((1) (with-syntax ((i (car indexes)))
                  #'(vector-ref container i)))
           ((2) (with-syntax ((i (car indexes)) (j (cadr indexes)))
                  #'(vector-ref (vector-ref container i) j)))
           (else #'($bracket-apply-vector$ container arg-bracket ...))))))))

(define-syntax $bracket-apply-vector-integer$
  (lambda (stx)
    (syntax-case stx ()
      ((_ container arg-bracket ...)
       (let ((indexes (typed-indexes (syntax->list #'(arg-bracket ...)))))
         (case (length indexes)
           ((1) (with-syntax ((index (car indexes)))
                  #'(let ((c container) (i index))
                      (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i)))))
           ((2) (with-syntax ((index1 (car indexes)) (index2 (cadr indexes)))
                  #'(let ((c container) (i index1) (j index2))
                      (let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
                        (vector-ref row (if (fx< j 0) (fx+ (vector-length row) j) j))))))
           (else #'($bracket-apply-vector$ container arg-bracket ...))))))))

(define-syntax $bracket-apply-string-natural$
  (lambda (stx)
    (syntax-case stx ()
      ((_ container arg-bracket ...)
       (let ((indexes (typed-indexes (syntax->list #'(arg-bracket ...)))))
         (if (= (length indexes) 1)
             (with-syntax ((i (car indexes)))
               #'(string-ref container i))
             #'($bracket-apply-string$ container arg-bracket ...)))))))

(define-syntax $bracket-apply-string-integer$
  (lambda (stx)
    (syntax-case stx ()
      ((_ container arg-bracket ...)
       (let ((indexes (typed-indexes (syntax->list #'(arg-bracket ...)))))
         (if (= (length indexes) 1)
             (with-syntax ((index (car indexes)))
               #'(let ((c container) (i index))
                   (string-ref c (if (fx< i 0) (fx+ (string-length c) i) i))))
             #'($bracket-apply-string$ container arg-bracket ...)))))))


(define (apply-square-brackets-argument-0-vector container-eval)

  ;(display "apply-square-brackets-argument-0-vector")  (newline)
  ;; why a copy???? why not the original object????
  (vector-copy container-eval)) ;; return a copy of vector


;; {T[:]}
;; '#(1 2 3)
(define (apply-square-brackets-argument-1-vector container-eval index-eval)

  ;(display "apply-square-brackets-argument-1-vector")  (newline)
  ;; {#(1 2 3 4 5)[-2]}
  ;; 4
  
  ;; {T <+ (vector 1 2 3)}
  ;; '#(1 2 3)
  ;; > {T1 <+ T}
  ;; '#(1 2 3)
  ;; > {T1[1] <- 7}
  ;; 7
  ;; > T
  ;; '#(1 7 3)
  ;; > {T2 <+ T[:]}
  ;; '#(1 7 3)
  ;; > {T2[1] <- 0}
  ;; 0
  ;; > T
  ;; '#(1 7 3)
  ;; > T2
  ;; '#(1 0 3)

  (if (eq? slice index-eval) ;; T[:] 
      (vector-copy container-eval) ;; return a copy of vector
      (if (< index-eval 0) ;; negative index as in Python
          (vector-ref container-eval (+ (vector-length container-eval) index-eval)) ;; negative indexing
          (vector-ref container-eval index-eval)))) ;; return an element of the vector


(define (apply-square-brackets-argument-2-vector container-eval index1-or-keyword-eval index2-or-keyword-eval)

  ;; 2 dimension vector ? or 1 dimension vector : T[i1 :] , T[: i2]


  ;; {#(1 2 3 4 5)[2 :]}
  ;; '#(3 4 5)
  ;;
  ;; {#(1 2 3 4 5)[: 3]}
  ;; '#(1 2 3)
  
  (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: :]
	      (eq? slice index2-or-keyword-eval))
	 container-eval)
	
	((eq? slice index1-or-keyword-eval) ;; T[: i2]
	 (if (< index2-or-keyword-eval 0) ;; negative index
	     (vector-copy container-eval 0 (+ (vector-length container-eval) index2-or-keyword-eval))
	     (vector-copy container-eval 0 index2-or-keyword-eval)))
	
	((eq? slice index2-or-keyword-eval) ;; T[i1 :]
	 (if (< index1-or-keyword-eval 0) ;; negative index
	     (vector-copy container-eval (+ (vector-length container-eval) index1-or-keyword-eval))
	     (vector-copy container-eval index1-or-keyword-eval)))
	
	(else ;; T[i1 i2] vector of vectors
	 (function-array-n-dim-ref container-eval (reverse (list index1-or-keyword-eval index2-or-keyword-eval))))))
;;(array-n-dim-ref container index1-or-keyword-eval index2-or-keyword-eval)


(define (apply-square-brackets-argument-3-vector container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)

  ;(display "apply-square-brackets-argument-3-vector")  (newline)
  ;; {#(1 2 3 4 5 6 7)[2 : 5]}
  ;; '#(3 4 5)
  ;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 : 3 * 5 - 10]}
  ;; '#(3 4 5)
  ;; 3 dimension vector T[i1 i2 i3]? or T[i1 : i3] or T[: : step] or  T[: i2 :] or  T[i1 : :]

  ;; {#(1 2 3 4 5 6 7 8)[: : 3]}
  ;; '#(1 4 7)

  ;; {#(1 2 3 4 5 6 7 8)[: : -2]}
  ;; '#(8 6 4 2)
  (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: : step]
              (eq? slice index2-or-keyword-eval))

         (when (= 0 index3-or-keyword-or-step-eval)
           (error "apply-square-brackets-argument-3 : slice step cannot be zero"))
         
         (let* ((size-input (vector-length container-eval))
                (size (quotient size-input (abs index3-or-keyword-or-step-eval)))
                (result '())
                (i 0))
           
           (when (not (= (modulo size-input index3-or-keyword-or-step-eval) 0))
             (set! size (+ 1 size)))
           (set! result (make-vector size))

           (if (< index3-or-keyword-or-step-eval 0) ;; with negative index we start at end of vector (like in Python)
               (for ((define k (- size-input 1)) (>= k 0) (set! k (+ k index3-or-keyword-or-step-eval)))
                 (vector-set! result
                              i
                              (vector-ref container-eval k))
                 (set! i (+ 1 i)))
               (for ((define k 0) (< k size-input) (set! k (+ k index3-or-keyword-or-step-eval)))
                 (vector-set! result
                              i
                              (vector-ref container-eval k))
                 (set! i (+ 1 i))))
           
           result))
        

        ((eq? slice index2-or-keyword-eval) ;; T[i1 : i3]
         
         (when (< index1-or-keyword-eval 0) ;; negative index
           (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
         
         (when (< index3-or-keyword-or-step-eval 0) ;; negative index
           (set! index3-or-keyword-or-step-eval (+ (vector-length container-eval) index3-or-keyword-or-step-eval)))
         
         (vector-copy container-eval index1-or-keyword-eval index3-or-keyword-or-step-eval))


        ((eq? slice index2-or-keyword-eval) ;; T[i1 : :]
         
         (when (< index1-or-keyword-eval 0) ;; negative index
           (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
         
         (vector-copy container-eval index1-or-keyword-eval))
        
        
        ((and (eq? slice index1-or-keyword-eval)  (eq? slice index3-or-keyword-or-step-eval)) ;; T[: i2 :]
         
         (when (< index2-or-keyword-eval 0) ;; negative index
           (set! index2-or-keyword-eval (+ (vector-length container-eval) index2-or-keyword-eval)))
         
         (vector-copy container-eval 0 index3-or-keyword-or-step-eval))

        
        ;; T[i1 i2 i3] vector of vectors of vectors
        (else
         (function-array-n-dim-ref container-eval
                                   (list index3-or-keyword-or-step-eval index2-or-keyword-eval index1-or-keyword-eval))))) ;; was reverse list ...


;; {(vector 1 2 3 4 5)[2 : : -1]}
;;'#(3 2 1)
;; {(vector 1 2 3 4 5)[2 : : 1]}
;;'#(3 4 5)
(define (apply-square-brackets-argument-4-vector container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)

  ;; {#(1 2 3 4 5 6 7 8 9)[: 7 : 2]}
  ;; '#(1 3 5 7)
  ;; {#(1 2 3 4 5 6 7 8 9)[: 6 : -1]}
  ;; '#(6 5 4 3 2 1)
  ;; {#(1 2 3 4 5 6 7 8 9)[: 6 : -2]}
  ;; '#(6 4 2)
  ;; {#(1 2 3 4 5 6 7 8 9)[: -3 : -2]}
  ;; '#(6 4 2)
  (cond ((and (eq? slice index1-or-keyword-eval)  ;; T[: i2 : s]
              (eq? slice index3-or-keyword-eval))
         
         (when (= 0 index4-or-keyword-or-step-eval)
           (error "apply-square-brackets-argument-4 : slice step cannot be zero"))
         
         (let* ((size 0) ;; result size
                (result '())
                (i 0))

           (when (< index2-or-keyword-eval 0) ;; negative index
             (set! index2-or-keyword-eval (+ (vector-length container-eval) index2-or-keyword-eval)))

           (set! size (quotient index2-or-keyword-eval (abs index4-or-keyword-or-step-eval)))
           
           ;;(displayln size)
           (when (not (= (modulo index2-or-keyword-eval index4-or-keyword-or-step-eval) 0))
             (set! size (+ 1 size)))
           
           ;;(displayln size)
           (set! result (make-vector size))
           
           (if (< index4-or-keyword-or-step-eval 0) 
               
               (for ((define k (- index2-or-keyword-eval 1)) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
                 (vector-set! result
                              i
                              (vector-ref container-eval k))
                 (set! i (+ 1 i)))
               
               (for ((define k 0) (< k index2-or-keyword-eval) (set! k (+ k index4-or-keyword-or-step-eval)))
                 (vector-set! result
                              i
                              (vector-ref container-eval k))
                 (set! i (+ 1 i))))
           
           result))



        ;; {#(1 2 3 4 5 6 7 8 9)[3 : : 2]}
        ;; '#(4 6 8)
        ;; > {#(1 2 3 4 5 6 7 8 9)[3 : : -2]}
        ;; '#(4 2)
        ;; > {#(1 2 3 4 5 6 7 8 9)[-3 : : 2]}
        ;; '#(7 9)
        ;; {#(1 2 3 4 5 6 7 8 9)[-3 : : -2]}
        ;; '#(7 5 3 1)
        ((and (eq? index2-or-keyword-eval slice)  ;; T[i1 : : s]
              (eq? index3-or-keyword-eval slice))
         
         (when (= 0 index4-or-keyword-or-step-eval)
           (error "apply-square-brackets-argument-4 : slice step cannot be zero"))
         
         (let* ((size-container-eval (vector-length container-eval))
                (i1 (if (< index1-or-keyword-eval 0) ;; negative index
                        (+ size-container-eval index1-or-keyword-eval)
                        index1-or-keyword-eval))
                (size-input (if (> index4-or-keyword-or-step-eval 0)
                                (- size-container-eval i1)
                                (+ i1 1)))
                (size (quotient size-input (abs index4-or-keyword-or-step-eval))) ;; result size
                (result '())
                (i 0))
           
           (when (not (= (modulo size-input index4-or-keyword-or-step-eval) 0))
             (set! size (+ 1 size)))
           (set! result (make-vector size))

           (if (< index4-or-keyword-or-step-eval 0) 
               
               (for ((define k i1) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
                 (vector-set! result
                              i
                              (vector-ref container-eval k))
                 (set! i (+ 1 i)))
               
               (for ((define k i1) (< k size-container-eval) (set! k (+ k index4-or-keyword-or-step-eval)))
                 ;;(displayln k)
                 (vector-set! result
                              i
                              (vector-ref container-eval k))
                 (set! i (+ 1 i))))
           
           result))


        ((and (eq? index2-or-keyword-eval slice) ;; T[i1 : i3 :]
              (eq? index4-or-keyword-or-step-eval slice))

         (let ((i1 index1-or-keyword-eval)
               (i3 index3-or-keyword-eval))
           (when (< i1 0) ;; negative index
             (set! i1 (+ (vector-length container-eval) i1)))
           (when (< i3 0) ;; negative index
             (set! i3 (+ (vector-length container-eval) i3)))
           (vector-copy container-eval i1 i3)))

        
        ;; T[i1 i2 i3 i4] vector of vectors of vectors ...
        (else		
         (function-array-n-dim-ref container-eval
                                   (list index4-or-keyword-or-step-eval index3-or-keyword-eval index2-or-keyword-eval index1-or-keyword-eval))))) ;; was reverse list ...
;;or use array-n-dim-ref macro


(define (apply-square-brackets-argument-5-vector container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)

  ;; {#(1 2 3 4 5 6 7 8 9)[2 : 5 : 1]}
  ;; '#(3 4 5)
  ;; {#(1 2 3 4 5 6 7 8 9)[5 : 2 : -1]}
  ;; '#(6 5 4)
  ;; {#(1 2 3 4 5 6 7 8 9)[2 : 5 : -1]}
  ;; '#()
  ;; {#(1 2 3 4 5 6 7 8 9)[-1 : 5 : -1]}
  ;; '#(9 8 7)
  ;; {#(1 2 3 4 5 6 7 8 9)[-0 : 5 : -1]}
  ;; '#()
  
  (if (and (eq? index2-or-keyword-eval slice)  ;; T[i1 : i3 : s]
           (eq? index4-or-keyword-eval slice))

      (begin
        
        (when (= 0 index5-or-step-eval)
          (error "apply-square-brackets-argument-5 : slice step cannot be zero"))
        
        (let* ((size-container-eval (vector-length container-eval))
               
               (i1 (if (< index1-eval 0) ;; negative index
                       (+ size-container-eval index1-eval)
                       index1-eval))

               (i3 (if (< index3-eval 0) ;; negative index
                       (+ size-container-eval index3-eval)
                       index3-eval))
               
               (size-input (if (> index5-or-step-eval 0)
                               (- i3 i1)
                               (- i1 i3)))
               
               (size (quotient size-input (abs index5-or-step-eval))) ;; result size
               (result '())
               (i 0))
          
          (when (not (= (modulo size-input index5-or-step-eval) 0))
            (set! size (+ 1 size)))

          (if (<= size 0)
              
              (make-vector 0)

              (begin
                (set! result (make-vector size))
                
                (if (< index5-or-step-eval 0) 
                    
                    (for ((define k i1) (> k i3) (set! k (+ k index5-or-step-eval))) ;; we do not include i1-th element
                      ;; i do not allow Python index over size of vector
                      ;; (when (>= k size-container)
                      ;; 	 (continue))
                      
                      (vector-set! result
                                   i
                                   (vector-ref container-eval k))
                      (set! i (+ 1 i)))
                    
                    (for ((define k i1) (< k i3) (set! k (+ k index5-or-step-eval)))
                      ;;(displayln k)
                      (vector-set! result
                                   i
                                   (vector-ref container-eval k))
                      (set! i (+ 1 i))))
                
                result))))


      ;; T[i1 i2 i3 i4 i5] vector of vectors of vectors ...
      (function-array-n-dim-ref container-eval (list index5-or-step-eval
                                                     index4-or-keyword-eval
                                                     index3-eval
                                                     index2-or-keyword-eval
                                                     index1-eval)))) ;; was reverse list ...
;;or use array-n-dim-ref macro


(define (apply-square-brackets-argument-6-and-more-vector container-eval args)

  (function-array-n-dim-ref container-eval (reverse args)))



; string procedures and macros

(define-syntax $bracket-apply-string$  ;;  this implements a specialized bracket-apply for strings

  (lambda (stx)

    (syntax-case stx ()

      ;; a version that pre-compil the infix expression, should be faster
      (($bracket-apply-string$ container arg-bracket ...) ;  . args-brackets


       (with-syntax ((parsed-args

		      (cons #'list

                            (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
                                                                           (list (cons '* #'fx*)
                                                                                 (cons '- #'fx-)
                                                                                 (cons '+ #'fx+)
                                                                                 (cons '/ #'fxquotient))))))

	 ;; when trying to put this 'case in a subroutine Racket complains of #'parsed-args being outside templates.
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args))) ; we remove 'list from the length of syntax list (list arg1 arg2 ...) to get (arg1 arg2 ...)

	   ((0) #'(apply-square-brackets-argument-0-string container))

	   ;; 1 argument in [ ]
	   ;; T[index]
	   ;; written inline as for the vectors : an index (not a slice) , negative from the end as in Python
	   ((1) #;#'(apply-square-brackets-argument-1-string container
                                                             (first parsed-args))
		(with-syntax ((index (cadr (syntax->list #'parsed-args))))
		  #'(let ((c container) (i index))
		      (if (fixnum? i)
			  (string-ref c (if (fx< i 0) (fx+ (string-length c) i) i))
			  (apply-square-brackets-argument-1-string c i)))))
	   ;; 2 arguments in [ ]
	   ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]

	   ;; {"hello"[2 :]}
	   ;; "llo"
	   ((2) #'(apply-square-brackets-argument-2-string container
                                                           (first parsed-args)
                                                           (second parsed-args)))

	   ;; 3 arguments in [ ]
	   ;; T[i1 : i2] , T[: : s]
	   ((3) #'(apply-square-brackets-argument-3-string container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)))

	   ;; 4 arguments in [ ]
	   ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :]
	   ((4) #'(apply-square-brackets-argument-4-string container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)
                                                           (fourth parsed-args)))

	   ;; 5 arguments in [ ]
	   ;; T[i1 : i3 : s]
	   ((5) #'(apply-square-brackets-argument-5-string container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)
                                                           (fourth parsed-args)
                                                           (fifth parsed-args)))
	   ;; more than 5 arguments in [ ] : not allowed with strings
	   (else #'(apply-square-brackets-argument-6-and-more-string container parsed-args))))))))



(define (apply-square-brackets-argument-0-string container-eval)

  (string-copy container-eval)) ;; return a copy of the string


;; {T[:]}
;; "abc"
(define (apply-square-brackets-argument-1-string container-eval index-eval)

  ;; {"toto"[2]}
  ;; #\t
  ;; {"toto"[-1]}
  ;; #\o
  (if (eq? slice index-eval) ;; T[:]
      (string-copy container-eval) ;; return a copy of the string
      (if (< index-eval 0) ;; negative index as in Python
          (string-ref container-eval (+ (string-length container-eval) index-eval)) ;; negative indexing
          (string-ref container-eval index-eval)))) ;; return an element of the string


;; {"hello"[: 2]}
;; "he"
;; {"hello"[3 :]}
;; "lo"
;; {"hello"[: :]}
;; "hello"
(define (apply-square-brackets-argument-2-string container-eval index1-or-keyword-eval index2-or-keyword-eval)

  (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: :]
	      (eq? slice index2-or-keyword-eval))
	 container-eval)

	((eq? slice index1-or-keyword-eval) ;; T[: i2]
	 (if (< index2-or-keyword-eval 0) ;; negative index
	     (substring container-eval 0 (+ (string-length container-eval) index2-or-keyword-eval))
	     (substring container-eval 0 index2-or-keyword-eval)))

	((eq? slice index2-or-keyword-eval) ;; T[i1 :]
	 (if (< index1-or-keyword-eval 0) ;; negative index
	     (substring container-eval (+ (string-length container-eval) index1-or-keyword-eval))
	     (substring container-eval index1-or-keyword-eval)))

	(else ;; syntax error
	 (error "apply-square-brackets-argument-2-string : bad arguments in string case,expecting : i2 or i1 :, provided :"
		index1-or-keyword-eval index2-or-keyword-eval))))


;; {"elephant"[2 : 5]}
;; "eph"
;;  {"abcdefghijkl"[: : 2]}
;; "acegik"
;; {"abcdefghijkl"[: : -3]}
;; "lifc"
;; {"123456789"[ :  : -1]}
;; "987654321"
(define (apply-square-brackets-argument-3-string container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)

  (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: : step]
	      (eq? slice index2-or-keyword-eval))

	 (when (= 0 index3-or-keyword-or-step-eval)
	       (error "apply-square-brackets-argument-3-string : slice step cannot be zero"))

	 (let* ((size-input (string-length container-eval))
		(size (quotient size-input (abs index3-or-keyword-or-step-eval)))
		(result '())
		(i 0))

	   (when (not (= (modulo size-input index3-or-keyword-or-step-eval) 0))
		 (set! size (+ 1 size)))

	   (set! result (make-string size))

	   (if (< index3-or-keyword-or-step-eval 0) ;; with negative index we start at end of vector (like in Python)
	       (for ((define k (- size-input 1)) (>= k 0) (set! k (+ k index3-or-keyword-or-step-eval)))
		    (string-set! result
				 i
				 (string-ref container-eval k))
		    (set! i (+ 1 i)))
	       (for ((define k 0) (< k size-input) (set! k (+ k index3-or-keyword-or-step-eval)))
		    (string-set! result
				 i
				 (string-ref container-eval k))
		    (set! i (+ 1 i))))

	   result))

	((eq? slice index2-or-keyword-eval) ;; T[i1 : i3]

	 (when (< index1-or-keyword-eval 0) ;; negative index
	       (set! index1-or-keyword-eval (+ (string-length container-eval) index1-or-keyword-eval)))

	 (when (< index3-or-keyword-or-step-eval 0) ;; negative index
	       (set! index3-or-keyword-or-step-eval (+ (string-length container-eval) index3-or-keyword-or-step-eval)))

	 (substring container-eval index1-or-keyword-eval index3-or-keyword-or-step-eval))



	((eq? slice index2-or-keyword-eval) ;; T[i1 : :]

	 (when (< index1-or-keyword-eval 0) ;; negative index
	       (set! index1-or-keyword-eval (+ (string-length container-eval) index1-or-keyword-eval)))

	 (string-copy container-eval index1-or-keyword-eval))


	((and (eq? slice index1-or-keyword-eval)  (eq? slice index3-or-keyword-or-step-eval)) ;; T[: i2 :]

	 (when (< index2-or-keyword-eval 0) ;; negative index
	       (set! index2-or-keyword-eval (+ (string-length container-eval) index2-or-keyword-eval)))

	 (string-copy container-eval 0 index3-or-keyword-or-step-eval))



	(else
	 (error "apply-square-brackets-argument-3-string : in string case, provided too much arguments:"
		index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval))))


;; {"abcdefghijklmno"[: 7 : 2]}
;; "aceg"
;; {"123456789"[: -3 : -2]}
;; "642"
(define (apply-square-brackets-argument-4-string container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)

  (cond ((and (eq? slice index1-or-keyword-eval) ;; T[: i2 : s]
	      (eq? slice index3-or-keyword-eval))

	 (when (= 0 index4-or-keyword-or-step-eval)
	       (error "apply-square-brackets-argument-4-string : slice step cannot be zero"))

	 (let* ((size 0)
		(result '())
		(i 0)
		(i2 index2-or-keyword-eval))

	   (when (< i2 0) ;; negative index
		 (set! i2 (+ (string-length container-eval) i2)))

	   (set! size (quotient i2 (abs index4-or-keyword-or-step-eval)))

	   (when (not (= (modulo i2 index4-or-keyword-or-step-eval) 0))
		 (set! size (+ 1 size)))
	   (set! result (make-string size))

	   (if (< index4-or-keyword-or-step-eval 0)

	       (for ((define k (- i2 1)) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
		    (string-set! result
				 i
				 (string-ref container-eval k))
		    (set! i (+ 1 i)))

	       (for ((define k 0) (< k i2) (set! k (+ k index4-or-keyword-or-step-eval)))
		    (string-set! result
				 i
				 (string-ref container-eval k))
		    (set! i (+ 1 i))))

	   result))


	;; {"abcdefghijklmno"[3 : : 2]}
	;; "dfhjln"
	;; {"123456789"[3 :  : 2]}
	;; "468"
	;; {"123456789"[3 :  : -2]}
	;; "42"
	;; {"123456789"[-3 :  : -2]}
	;; "7531"
	;; {"123456789"[-3 :  : 2]}
	;; "79"
	((and (eq? index2-or-keyword-eval slice)  ;; T[i1 : : s]
	      (eq? index3-or-keyword-eval slice))

	 (when (= 0 index4-or-keyword-or-step-eval)
	       (error "apply-square-brackets-argument-4-string : slice step cannot be zero"))

	 (let* ((size-container-eval (string-length container-eval))
		(i1 (if (< index1-or-keyword-eval 0) ;; negative index
			(+ size-container-eval index1-or-keyword-eval)
			index1-or-keyword-eval))
		(size-input (if (> index4-or-keyword-or-step-eval 0)
				(- size-container-eval i1)
				(+ i1 1)))
		(size (quotient size-input (abs index4-or-keyword-or-step-eval)))
		(result '())
		(i 0))

	   (when (not (= (modulo size-input index4-or-keyword-or-step-eval) 0))
		 (set! size (+ 1 size)))
	   (set! result (make-string size))

	   (if (< index4-or-keyword-or-step-eval 0)

	       (for ((define k i1) (>= k 0) (set! k (+ k index4-or-keyword-or-step-eval)))
		    (string-set! result
				 i
				 (string-ref container-eval k))
		    (set! i (+ 1 i)))

	       (for ((define k i1) (< k size-container-eval) (set! k (+ k index4-or-keyword-or-step-eval)))
		    (string-set! result
				 i
				 (string-ref container-eval k))
		    (set! i (+ 1 i))))

	   result))


	((and (eq? slice index2-or-keyword-eval) ;; T[i1 : i3 :]
	      (eq? slice index4-or-keyword-or-step-eval))

	 (let ((i1  index1-or-keyword-eval)
	       (i3 index3-or-keyword-eval))
	   (when (< i1 0) ;; negative index
		 (set! i1 (+ (string-length container-eval) i1)))
	   (when (< i3 0) ;; negative index
		 (set! i3 (+ (string-length container-eval) i3)))
	   (substring container-eval i1 i3)))


	(else
	 (error "apply-square-brackets-argument-4-string : in string case, provided too much arguments:"
		index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval))))


;; {"123456789"[5 : 2 : -1]}
;; "543"
;; {"0123456789"[5 :  : -1]}
;; "543210"
;; {"0123456789"[5 : 0 : -1]}
;; "54321"
(define (apply-square-brackets-argument-5-string container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)

  (if (and (eq? index2-or-keyword-eval slice)  ;; T[i1 : i3 : s]
	   (eq? index4-or-keyword-eval slice))

      (begin

	(when (= 0 index5-or-step-eval)
	  (error "apply-square-brackets-argument-5-string : slice step cannot be zero"))

	(let* ((size-container-eval (string-length container-eval))

	       (i1 (if (< index1-eval 0) ;; negative index
		       (+ size-container-eval index1-eval)
		       index1-eval))

	       (i3 (if (< index3-eval 0) ;; negative index
		       (+ size-container-eval index3-eval)
		       index3-eval))

	       (size-input (if (> index5-or-step-eval 0)
			       (- i3 i1)
			       (- i1 i3)))

	       (size (quotient size-input (abs index5-or-step-eval))) ;; result size
	       (result '())
	       (i 0))

	  (when (not (= (modulo size-input index5-or-step-eval) 0))
	    (set! size (+ 1 size)))

	  (if (<= size 0)

	      (make-string 0)

	      (begin
		(set! result (make-string size))

		(if (< index5-or-step-eval 0)

		    (for ((define k i1) (> k i3) (set! k (+ k index5-or-step-eval))) ;; we do not include i1-th element
			 (string-set! result
				      i
				      (string-ref container-eval k))
			 (set! i (+ 1 i)))

		    (for ((define k i1) (< k i3) (set! k (+ k index5-or-step-eval)))
			 (string-set! result
				      i
				      (string-ref container-eval k))
			 (set! i (+ 1 i))))

		result))))

      ;; T[i1 i2 i3 i4 i5] not allowed with strings
      (error "apply-square-brackets-argument-5-string : in string case, provided too much arguments:" index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)))


(define (apply-square-brackets-argument-6-and-more-string container-eval args)

  (error "apply-square-brackets-argument-6-and-more-string : string used with [ ] and more than 5 arguments"))



; hash table procedures and macros

(define-syntax $bracket-apply-hash-table$  ;;  this implements a specialized bracket-apply for hash tables

  (lambda (stx)

    (syntax-case stx ()

      ;; a version that pre-compil the infix expression, should be faster
      (($bracket-apply-hash-table$ container arg-bracket ...) ;  . args-brackets


       (with-syntax ((parsed-args

		      (cons #'list

                            (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
                                                                           (list (cons '* #'fx*)
                                                                                 (cons '- #'fx-)
                                                                                 (cons '+ #'fx+)
                                                                                 (cons '/ #'fxquotient))))))

	 ;; when trying to put this 'case in a subroutine Racket complains of #'parsed-args being outside templates.
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args))) ; we remove 'list from the length of syntax list (list arg1 arg2 ...) to get (arg1 arg2 ...)

	   ((0) #'(apply-square-brackets-argument-0-hash-table container))

	   ;; 1 argument in [ ]
	   ;; T[key]
	   ((1) #'(apply-square-brackets-argument-1-hash-table container
                                                                (first parsed-args)))
	   ;; more than 1 argument in [ ] : not allowed with hash tables
	   ((2) #'(apply-square-brackets-argument-2-hash-table container
                                                                (first parsed-args)
                                                                (second parsed-args)))

	   ((3) #'(apply-square-brackets-argument-3-hash-table container
                                                                (first parsed-args)
                                                                (second parsed-args)
                                                                (third parsed-args)))

	   ((4) #'(apply-square-brackets-argument-4-hash-table container
                                                                (first parsed-args)
                                                                (second parsed-args)
                                                                (third parsed-args)
                                                                (fourth parsed-args)))

	   ((5) #'(apply-square-brackets-argument-5-hash-table container
                                                                (first parsed-args)
                                                                (second parsed-args)
                                                                (third parsed-args)
                                                                (fourth parsed-args)
                                                                (fifth parsed-args)))

	   (else #'(apply-square-brackets-argument-6-and-more-hash-table container parsed-args))))))))



;; {H[]}
;; return the elements of the hash table as an alist
(define (apply-square-brackets-argument-0-hash-table container-eval)

  (hash-table->alist container-eval))


;; {H[key]}
(define (apply-square-brackets-argument-1-hash-table container-eval index-eval)

  (hash-table-ref container-eval index-eval))


(define (apply-square-brackets-argument-2-hash-table container-eval index1-or-keyword-eval index2-or-keyword-eval)

  (error "apply-square-brackets-argument-2-hash-table : hash table used with [ ] and more than 1 argument"))


(define (apply-square-brackets-argument-3-hash-table container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)

  (error "apply-square-brackets-argument-3-hash-table : hash table used with [ ] and more than 1 argument"))


(define (apply-square-brackets-argument-4-hash-table container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)

  (error "apply-square-brackets-argument-4-hash-table : hash table used with [ ] and more than 1 argument"))


(define (apply-square-brackets-argument-5-hash-table container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)

  (error "apply-square-brackets-argument-5-hash-table : hash table used with [ ] and more than 1 argument"))


(define (apply-square-brackets-argument-6-and-more-hash-table container-eval args)

  (error "apply-square-brackets-argument-6-and-more-hash-table : hash table used with [ ] and more than 1 argument"))



; array procedures and macros (SRFI 25)

(define-syntax $bracket-apply-array$  ;;  this implements a specialized bracket-apply for SRFI 25 arrays

  (lambda (stx)

    (syntax-case stx ()

      ;; a version that pre-compil the infix expression, should be faster
      (($bracket-apply-array$ container arg-bracket ...) ;  . args-brackets


       (with-syntax ((parsed-args

		      (cons #'list

                            (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
                                                                           (list (cons '* #'fx*)
                                                                                 (cons '- #'fx-)
                                                                                 (cons '+ #'fx+)
                                                                                 (cons '/ #'fxquotient))))))

	 ;; when trying to put this 'case in a subroutine Racket complains of #'parsed-args being outside templates.
	 ;; not written inline as the vectors : array-ref of SRFI 25 written directly gave no measurable gain , the cost is
	 ;; in array-ref itself
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args))) ; we remove 'list from the length of syntax list (list arg1 arg2 ...) to get (arg1 arg2 ...)

	   ((0) #'(apply-square-brackets-argument-0-array container))

	   ;; 1 argument in [ ]
	   ;; T[i1] ,  1 dimension array
	   ((1) #'(apply-square-brackets-argument-1-array container
                                                          (first parsed-args)))

	   ;; 2 arguments in [ ]
	   ;; T[i1 i2] ,  2 dimension array
	   ((2) #'(apply-square-brackets-argument-2-array container
                                                          (first parsed-args)
                                                          (second parsed-args)))

	   ;; 3 arguments in [ ]
	   ;; T[i1 i2 i3] ,  3 dimension array
	   ((3) #'(apply-square-brackets-argument-3-array container
                                                          (first parsed-args)
                                                          (second parsed-args)
                                                          (third parsed-args)))

	   ;; 4 arguments in [ ]
	   ;; T[i1 i2 i3 i4] ,  4 dimension array
	   ((4) #'(apply-square-brackets-argument-4-array container
                                                          (first parsed-args)
                                                          (second parsed-args)
                                                          (third parsed-args)
                                                          (fourth parsed-args)))

	   ;; 5 arguments in [ ]
	   ;; T[i1 i2 i3 i4 i5] ,  5 dimension array
	   ((5) #'(apply-square-brackets-argument-5-array container
                                                          (first parsed-args)
                                                          (second parsed-args)
                                                          (third parsed-args)
                                                          (fourth parsed-args)
                                                          (fifth parsed-args)))
	   ;; more than 5 arguments in [ ]
	   ;; T[i1 i2 i3 i4 i5 i6 ...] , n dimension array
	   (else #'(apply-square-brackets-argument-6-and-more-array container parsed-args))))))))


;; note : i do not use negative indexes or slices for array because they could be not starting at zero

;; {A[]}
(define (apply-square-brackets-argument-0-array container-eval)

  container-eval) ;; return the array (no copy procedure in SRFI 25 but exists in Guile)


;; {A[i1]} ,  1 dimension array
(define (apply-square-brackets-argument-1-array container-eval index-eval)

  (array-ref container-eval index-eval))


;; {A[i1 i2]} ,  2 dimension array
(define (apply-square-brackets-argument-2-array container-eval index1-eval index2-eval)

  (array-ref container-eval index1-eval index2-eval))


;; {A[i1 i2 i3]} ,  3 dimension array
(define (apply-square-brackets-argument-3-array container-eval index1-eval index2-eval index3-eval)

  (array-ref container-eval index1-eval index2-eval index3-eval))


;; {A[i1 i2 i3 i4]} ,  4 dimension array
(define (apply-square-brackets-argument-4-array container-eval index1-eval index2-eval index3-eval index4-eval)

  (array-ref container-eval index1-eval index2-eval index3-eval index4-eval))


;; {A[i1 i2 i3 i4 i5]} ,  5 dimension array
(define (apply-square-brackets-argument-5-array container-eval index1-eval index2-eval index3-eval index4-eval index5-eval)

  (array-ref container-eval index1-eval index2-eval index3-eval index4-eval index5-eval))


;; {A[i1 i2 i3 i4 i5 i6 ...]} , n dimension array
(define (apply-square-brackets-argument-6-and-more-array container-eval args)

  (array-ref container-eval (list->vector args)))



; flomat procedures and macros (removed : sci requires libgfortran)

#;(define (apply-square-brackets-argument-0-flomat container-eval)

  container-eval)


#;(define (apply-square-brackets-argument-1-flomat container-eval index-eval)

  (if (eq? slice index-eval) ;; T[:] 
      (error "apply-square-brackets.* :  slice not allowed with flomat")
      (row container-eval index-eval)))


#;(define (apply-square-brackets-argument-2-flomat container-eval index1-or-keyword-eval index2-or-keyword-eval)

  (if (or (eq? slice index1-or-keyword-eval)
	  (eq? slice index2-or-keyword-eval))
      (error "apply-square-brackets.* :  slice not allowed with flomat")
      (ref container-eval index1-or-keyword-eval index2-or-keyword-eval)))


#;(define (apply-square-brackets-argument-3-flomat container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)
  (error "apply-square-brackets.* :  flomat used with 3 arguments"))

#;(define (apply-square-brackets-argument-4-flomat container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)
  (error "apply-square-brackets.* :  flomat used with 4 arguments"))

#;(define (apply-square-brackets-argument-5-flomat container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)
  (error "apply-square-brackets.* :  flomat used with 5 arguments"))

#;(define (apply-square-brackets-argument-6-and-more-flomat container-eval args)
  (error "apply-square-brackets.* :  flomat used with more than 5 arguments"))


#;(define-syntax $bracket-apply-flomat$  ;;  this implements a specialized bracket-apply for flomats
  
  (lambda (stx)
    
    (syntax-case stx ()

      ;; a version that pre-compil the infix expression, should be faster
      (($bracket-apply-flomat$ container arg-bracket ...) ;  . args-brackets

      
       (with-syntax ((parsed-args

		      (cons #'list
		     
		              (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
                                                                             (list (cons '* #'fx*)
                                                                                   (cons '- #'fx-)
                                                                                   (cons '+ #'fx+)
                                                                                   (cons '/ #'fxquotient))))))
	 
	 ;;(newline)
	 ;;(display "bracket-apply-flomat : #'parsed-args=") (display #'parsed-args) (newline)
	 ;;(display "bracket-apply-flomat : (syntax->datum #'parsed-args)=") (display (syntax->datum #'parsed-args)) (newline)

	 ;; when trying to put this 'case in a subroutine Racket complains of #'parsed-args being outside templates.
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args))) ; we remove 'list from the length of syntax list (list arg1 arg2 ...) to get (arg1 arg2 ...)

	   ((0) #'(apply-square-brackets-argument-0-flomat container))
	   
	   ;; 1 argument in [ ]
	   ;; T[index]
	   ((1) #'(apply-square-brackets-argument-1-flomat container
                                                           (first parsed-args)))
	   ;; 2 arguments in [ ]
	   ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
	   
	   ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
	   ;; '#(3 4 5)
	   ;; the element F[i j] written inline (flomat : Racket only) : no slice , no negative index , as before
	   ((2) #;#'(apply-square-brackets-argument-2-flomat container
                                                             (first parsed-args)
                                                             (second parsed-args))
		(with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
		  #'(let ((c container) (i index1) (j index2))
		      (if (and (fixnum? i) (fixnum? j))
			  (ref c i j)
			  (apply-square-brackets-argument-2-flomat c i j)))))

	   ;; 3 arguments in [ ]
	   ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
	   ((3) #'(apply-square-brackets-argument-3-flomat container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)))

	   ;; 4 arguments in [ ]
	   ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
	   ((4) #'(apply-square-brackets-argument-4-flomat container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)
                                                           (fourth parsed-args)))

	   ;; 5 arguments in [ ]
	   ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
	   ((5) #'(apply-square-brackets-argument-5-flomat container
                                                           (first parsed-args)
                                                           (second parsed-args)
                                                           (third parsed-args)
                                                           (fourth parsed-args)
                                                           (fifth parsed-args)))
	   ;; more than 5 arguments in [ ]
	   ;; T[i1 i2 i3 i4 i5 i6 ...]
	   (else #'(apply-square-brackets-argument-6-and-more-flomat container parsed-args))))))))





(export
	$bracket-apply$
	apply-square-brackets-argument-1
	$bracket-apply-vector$
	$bracket-apply-string$
	$bracket-apply-hash-table$
	$bracket-apply-array$
	$bracket-apply-overloaded$
	$bracket-apply-vector-natural$
	$bracket-apply-vector-integer$
	$bracket-apply-string-natural$
	$bracket-apply-string-integer$)
 ; end library

; (define-overload-existing-operator +)
; (overload-existing-operator + (vector?) vector-append (vector? vector?))
; {{#(1 2) + #(3 4)}[2]}
;; REPL Curly Infix:

;; ($bracket-apply$ (+ #(1 2) #(3 4)) 2)

;; Parsed annotations. :
;; ($bracket-apply$ (vector-append #(1 2) #(3 4)) 2)

; 3
