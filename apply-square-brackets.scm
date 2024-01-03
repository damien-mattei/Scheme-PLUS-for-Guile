;; for guile (version compatible with my growable vector class)

;; This file is part of Scheme+

;; Copyright 2021-2024 Damien MATTEI

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



;; if you want it as a module:
;;(define-module (apply-square-brackets)

;;  #:export ($bracket-apply$ slice))



;; SRFI 105 : Curly-infix-expressions in conjunction with specialized $bracket-apply$
;; of Scheme+ allows a syntax like {Container[index]} with vectors
;; and arrays of any dimensions,size and shape and hash tables

;; (define T (make-vector 5))
;; (vector-set! T 3 7)
;; scheme@(guile-user)> {T[3]}
;; $3 = 7
;; {T[3] <- 7}
;; 7

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

;; scheme@(guile-user)> '{b[2]}
;; ($bracket-apply$ b 2)

;; scheme@(guile-user)> {a[2 4] <- 7}
;; scheme@(guile-user)> {a[2 4]}
;; $19 = 7
;; scheme@(guile-user)> {a[1 3] <- 5}
;; scheme@(guile-user)> {a[1 3] <- a[2 4]}
;; scheme@(guile-user)> {a[1 3]}
;; $20 = 7

;; scheme@(guile-user)> '{a[1 3] <- a[2 4]}
;; (<- ($bracket-apply$ a 1 3) ($bracket-apply$ a 2 4))


;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 $ 3 * 5 - 10]}
;; '#(3 4 5)

;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 $ 3 * 5 - 10 $ 2 * 4 - 6]}
;; '#(3 5)
(define ($bracket-apply$ container . args-brackets)  ;;  this implements a possible $bracket-apply$ as proposed in SRFI-105

  ;;(display args-brackets) (newline)
  ($bracket-apply$next container (parse-square-brackets-arguments args-brackets)))



(define ($bracket-apply$next container args)   ;; optimized version

  ;;(display args) (newline)
  (case (length args)

    ((0) (apply-square-brackets-argument-0 container))

    
    ;; 1 argument in [ ]
    ;; T[index]
    ((1) (apply-square-brackets-argument-1 container
					   (first args)))
    ;; 2 arguments in [ ]
    ;; ex: T[i1 $] , T[$ i2], T[i1 i2] , T[$ $]
    
    ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) $]}
    ;; '#(3 4 5)
    ((2) (apply-square-brackets-argument-2 container
					   (first args)
					   (second args)))

    ;; 3 arguments in [ ]
    ;; T[i1 $ i2] , T[i1 i2 i3] , T[$ $ s]
    ((3) (apply-square-brackets-argument-3 container
					   (first args)
					   (second args)
					   (third args)))

    ;; 4 arguments in [ ]
    ;; T[$ i2 $ s] , T[i1 $ $ s] , T[i1 $ i3 $] , T[i1 i2 i3 i4]
    ((4) (apply-square-brackets-argument-4 container
					   (first args)
					   (second args)
					   (third args)
					   (fourth args)))

    ;; 5 arguments in [ ]
    ;; T[i1 $ i3 $ s] , T[i1 i2 i3 i4 i5]
    ((5) (apply-square-brackets-argument-5 container
					   (first args)
					   (second args)
					   (third args)
					   (fourth args)
					   (fifth args)))
    ;; more than 5 arguments in [ ]
    ;; T[i1 i2 i3 i4 i5 i6 ...]
    (else 
     (if (vector? container)
	 (function-array-n-dim-ref container (reverse args)) 
	 (array-ref container (list->vector args))))))   ;; array SRFI 25




(define (apply-square-brackets-argument-0 container-eval)
   
  (cond ((vector? container-eval)
	 (vector-copy container-eval)) ;; return a copy of vector

	
	((hash-table? container-eval) (hash-table->alist container-eval)) ;; return the elements of hash table
	
	((string? container-eval)  (string-copy container-eval)) ;; return a copy of the string
				   	
	(else ;; array of SRFI 25	     
	 (array-copy container-eval)))) ;; return a copy of the array
 


;; {T[$]}
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
  ;; > {T2 <+ T[$]}
  ;; '#(1 7 3)
  ;; > {T2[1] <- 0}
  ;; 0
  ;; > T
  ;; '#(1 7 3)
  ;; > T2
  ;; '#(1 0 3)
  (cond ((or (vector? container-eval)  (growable-vector? container-eval))

	 (if (equal? slice index-eval) ;; T[$] . T[1 $ 5] 
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
	((string? container-eval) (if (equal? slice index-eval) ;; T[$] . T[1 $ 5] 
				      (string-copy container-eval) ;; return a copy of the string
				      (if (< index-eval 0) ;; negative index as in Python
					  (string-ref container-eval (+ (string-length container-eval) index-eval)) ;; negative indexing
					  (string-ref container-eval index-eval)))) ;; return an element of the string

	;; array of SRFI 25
	((array? container-eval) ;; T[i1] ,  1 dimension array
	 ;;(display "apply-square-brackets.* : array case : quoted container-eval = ") (display (quote container-eval)) (newline)
	 ;;(display "apply-square-brackets.* : array case : container-eval = ") (display container-eval) (newline)
	 (array-ref container-eval index-eval)) ;; return the element of the array
	
	(else
	 (let () ;; require a let block for definition with guile but not for racket ???
	   (define args-lst (list container-eval index-eval))
	   (define proc (find-getter-for-overloaded-square-brackets args-lst))
	   (apply proc args-lst))))

  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero

)






(define (apply-square-brackets-argument-2 container-eval index1-or-keyword-eval index2-or-keyword-eval)

  (cond ((or (vector? container-eval)  (growable-vector? container-eval)) ;; 2 dimension vector ? or T[i1 $] , T[$ i2]


	 ;; {#(1 2 3 4 5)[2 $]}
	 ;; '#(3 4 5)
	 ;;
	 ;; {#(1 2 3 4 5)[$ 3]}
	 ;; '#(1 2 3)
	 
	 (cond ((and (equal? slice index1-or-keyword-eval) ;; T[$ $]
		     (equal? slice index2-or-keyword-eval))
		container-eval)
	       
	       ((equal? slice index1-or-keyword-eval) ;; T[$ i2]
		(if (< index2-or-keyword-eval 0) ;; negative index
		    (vector-copy container-eval 0 (+ (vector-length container-eval) index2-or-keyword-eval))
		    (vector-copy container-eval 0 index2-or-keyword-eval)))
	       
	       ((equal? slice index2-or-keyword-eval) ;; T[i1 $]
		(if (< index1-or-keyword-eval 0) ;; negative index
		    (vector-copy container-eval (+ (vector-length container-eval) index1-or-keyword-eval))
		    (vector-copy container-eval index1-or-keyword-eval)))
	       
	       (else ;; T[i1 i2] vector of vectors
		(function-array-n-dim-ref container-eval (reverse (list index1-or-keyword-eval index2-or-keyword-eval))))))
	;;(array-n-dim-ref container index1-or-keyword-eval index2-or-keyword-eval)


	;; {"hello"[$ 2]}
	;; "he"
	;; {"hello"[3 $]}
	;; "lo"

	;; {"hello"[$ $]}
	;; "hello"
	;; {"hello"[$]}
	;; "hello"
	((string? container-eval) (cond ((and (equal? slice index1-or-keyword-eval) ;; T[$ $]
					      (equal? slice index2-or-keyword-eval))
					 container-eval)
					
					((equal? slice index1-or-keyword-eval) ;; T[$ i2]
					 (if (< index2-or-keyword-eval 0) ;; negative index
					     (substring container-eval 0 (+ (string-length container-eval) index2-or-keyword-eval))
					     (substring container-eval 0 index2-or-keyword-eval)))

					((equal? slice index2-or-keyword-eval) ;; T[i1 $]
					 (if (< index1-or-keyword-eval 0) ;; negative index
					     (substring container-eval (+ (string-length container-eval) index1-or-keyword-eval))
					     (substring container-eval index1-or-keyword-eval)))
					
					(else ;; syntax error
					 (error "$bracket-apply$ : bad arguments in string case,expecting $ i2 or i1 $, provided :" index1-or-keyword-eval index2-or-keyword-eval) )))


	((array? container-eval) ;; T[i1 i2] ,  2 dimension array
	 (array-ref container-eval index1-or-keyword-eval index2-or-keyword-eval)) ;; return the element of the array
	;; note : i do not use negative indexes or slices for array because they could be not starting at zero
	
	(else
	 (let () ;; require a let block for definition with guile but not for racket ???
	   (define args-lst (list container-eval index1-or-keyword-eval index2-or-keyword-eval))
	   (define proc (find-getter-for-overloaded-square-brackets args-lst))
	   (apply proc args-lst))))
  
  )



(define (apply-square-brackets-argument-3 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)

  ;; {#(1 2 3 4 5 6 7)[2 $ 5]}
  ;; '#(3 4 5)
  ;; {#(1 2 3 4 5 6 7)[2 * 5 - 8 $ 3 * 5 - 10]}
  ;; '#(3 4 5)
  (cond ((or (vector? container-eval) (growable-vector? container-eval)) ;; 3 dimension vector T[i1 i2 i3]? or T[i1 $ i3] or T[$ $ step] or  T[$ i2 $] or  T[i1 $ $]

	 ;; {#(1 2 3 4 5 6 7 8)[$ $ 3]}
	 ;; '#(1 4 7)

	 ;; {#(1 2 3 4 5 6 7 8)[$ $ -2]}
	 ;; '#(8 6 4 2)
	 (cond ((and (equal? slice index1-or-keyword-eval) ;; T[$ $ step]
		     (equal? slice index2-or-keyword-eval))

		(when (= 0 index3-or-keyword-or-step-eval)
		      (error "$bracket-apply$ : slice step cannot be zero"))
		
		(let* ((size-input (vector-length container-eval))
		       (size (quotient size-input (abs index3-or-keyword-or-step-eval)))
		       (result '())
		       (i 0)
		       (make-container make-vector))

		  (when (growable-vector? container-eval)
			(set! make-container make-growable-vector))
		  
		  (when (not (= (modulo size-input index3-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  (set! result (make-container size))

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
	       

	       ((equal? slice index2-or-keyword-eval) ;; T[i1 $ i3]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
		
		(when (< index3-or-keyword-or-step-eval 0) ;; negative index
		      (set! index3-or-keyword-or-step-eval (+ (vector-length container-eval) index3-or-keyword-or-step-eval)))
		
		(vector-copy container-eval index1-or-keyword-eval index3-or-keyword-or-step-eval))


	       ((equal? slice index2-or-keyword-eval) ;; T[i1 $ $]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
		
		(vector-copy container-eval index1-or-keyword-eval))
	       
	       
	       ({(equal? slice index1-or-keyword-eval) and (equal? slice index3-or-keyword-or-step-eval)} ;; T[$ i2 $]
		
		(when (< index2-or-keyword-eval 0) ;; negative index
		      (set! index2-or-keyword-eval (+ (vector-length container-eval) index2-or-keyword-eval)))
		
		(vector-copy container-eval 0 index3-or-keyword-or-step-eval))

	       
	       ;; T[i1 i2 i3] vector of vectors of vectors
	       (else
		(function-array-n-dim-ref container-eval
					  (list index3-or-keyword-or-step-eval index2-or-keyword-eval index1-or-keyword-eval))))) ;; was reverse list ...
	;;or use array-n-dim-ref macro


	;; {"elephant"[2 $ 5]}
	;; "eph"
	;;  {"abcdefghijkl"[$ $ 2]}
	;; "acegik"
	;; {"abcdefghijkl"[$ $ -3]}
	;; "lifc"
	;; {"123456789"[ $  $ -1]}
	;; "987654321"
	((string? container-eval) ;;  T[$ $ step] or T[i1 $ i3] or error
	 
	 (cond ((and (equal? slice index1-or-keyword-eval) ;; T[$ $ step]
		     (equal? slice index2-or-keyword-eval))

		(when (= 0 index3-or-keyword-or-step-eval)
		      (error "$bracket-apply$ : slice step cannot be zero"))
		
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
	       
	       ((equal? slice index2-or-keyword-eval) ;; T[i1 $ i3]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (vector-length container-eval) index1-or-keyword-eval)))
		
		(when (< index3-or-keyword-or-step-eval 0) ;; negative index
		      (set! index3-or-keyword-or-step-eval (+ (vector-length container-eval) index3-or-keyword-or-step-eval)))
		
		(substring container-eval index1-or-keyword-eval index3-or-keyword-or-step-eval))


	       
	       ((equal? slice index2-or-keyword-eval) ;; T[i1 $ $]
		
		(when (< index1-or-keyword-eval 0) ;; negative index
		      (set! index1-or-keyword-eval (+ (string-length container-eval) index1-or-keyword-eval)))
		
		(string-copy container-eval index1-or-keyword-eval))
	       
	       

	       ({(equal? slice index1-or-keyword-eval) and (equal? slice index3-or-keyword-or-step-eval)} ;; T[$ i2 $]
		
		(when (< index2-or-keyword-eval 0) ;; negative index
		      (set! index2-or-keyword-eval (+ (string-length container-eval) index2-or-keyword-eval)))
		
		(string-copy container-eval 0 index3-or-keyword-or-step-eval))

	       

	       (else
		(error "$bracket-apply$ : in string case, provided too much arguments:" index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval))))


	
	(else ;; T[i1 i2 i3] ,  3 dimension array
	 (array-ref container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval)))) ;; return the element of the array
  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero




;; {"123456789"[3 $  $ -2]}
;; "42"
(define (apply-square-brackets-argument-4 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)

  
  (cond ((or (vector? container-eval) (growable-vector? container-eval))

	 ;; {#(1 2 3 4 5 6 7 8 9)[$ 7 $ 2]}
	 ;; '#(1 3 5 7)
	 ;; {#(1 2 3 4 5 6 7 8 9)[$ 6 $ -1]}
	 ;; '#(6 5 4 3 2 1)
	 ;; {#(1 2 3 4 5 6 7 8 9)[$ 6 $ -2]}
	 ;; '#(6 4 2)
	 ;; {#(1 2 3 4 5 6 7 8 9)[$ -3 $ -2]}
	 ;; '#(6 4 2)
	 (cond ((and (equal? slice index1-or-keyword-eval)  ;; T[$ i2 $ s]
		     (equal? slice index3-or-keyword-eval))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "$bracket-apply$ : slice step cannot be zero"))
		
		(let* ((size 0) ;; result size
		       (result '())
		       (i 0)
		       (make-container make-vector))

		  (when (growable-vector? container-eval)
			(set! make-container make-growable-vector))

		  (when (< index2-or-keyword-eval 0) ;; negative index
			(set! index2-or-keyword-eval (+ (vector-length container-eval) index2-or-keyword-eval)))

		  (set! size (quotient index2-or-keyword-eval (abs index4-or-keyword-or-step-eval)))
		  
		  ;;(displayln size)
		  (when (not (= (modulo index2-or-keyword-eval index4-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  
		  ;;(displayln size)
		  (set! result (make-container size))
		  
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



	       ;; {#(1 2 3 4 5 6 7 8 9)[3 $ $ 2]}
	       ;; '#(4 6 8)
	       ;; > {#(1 2 3 4 5 6 7 8 9)[3 $ $ -2]}
	       ;; '#(4 2)
	       ;; > {#(1 2 3 4 5 6 7 8 9)[-3 $ $ 2]}
	       ;; '#(7 9)
	       ;; {#(1 2 3 4 5 6 7 8 9)[-3 $ $ -2]}
	       ;; '#(7 5 3 1)
	       ((and (equal? index2-or-keyword-eval slice)  ;; T[i1 $ $ s]
		     (equal? index3-or-keyword-eval slice))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "$bracket-apply$ : slice step cannot be zero"))
		
		(let* ((size-container-eval (vector-length container-eval))
		       (i1 (if (< index1-or-keyword-eval 0) ;; negative index
			       (+ size-container-eval index1-or-keyword-eval)
			       index1-or-keyword-eval))
		       (size-input (if (> index4-or-keyword-or-step-eval 0)
				       (- size-container-eval i1)
				       (+ i1 1)))
		       (size (quotient size-input (abs index4-or-keyword-or-step-eval))) ;; result size
		       (result '())
		       (i 0)
		       (make-container make-vector))

		  (when (growable-vector? container-eval)
			(set! make-container make-growable-vector))
		  
		  (when (not (= (modulo size-input index4-or-keyword-or-step-eval) 0))
			(set! size (+ 1 size)))
		  (set! result (make-container size))

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


	       ((and (equal? index2-or-keyword-eval slice) ;; T[i1 $ i3 $]
		     (equal? index4-or-keyword-or-step-eval slice))

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

	
	
	;; {"123456789"[$ -3 $ -2]}
	;; "642"
	((string? container-eval) 

	 ;; {"abcdefghijklmno"[$ 7 $ 2]}
	 ;; "aceg"
	 ;; > {"123456789"[$ -3 $ -2]}
	 ;; "642"
	 (cond ((and (equal? slice index1-or-keyword-eval) ;; T[$ i2 $ s]
		     (equal? slice index3-or-keyword-eval))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "$bracket-apply$ : slice step cannot be zero"))
		
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


	       ;; {"abcdefghijklmno"[3 $ $ 2]}
	       ;; "dfhjln"
	       ;; > {"123456789"[3 $  $ 2]}
	       ;; "468"
	       ;; > {"123456789"[3 $  $ -2]}
	       ;; "42"
	       ;; > {"123456789"[-3 $  $ -2]}
	       ;; "7531"
	       ;; > {"123456789"[-3 $  $ 2]}
	       ;; "79"
	       ((and (equal? index2-or-keyword-eval slice)  ;; T[i1 $ $ s]
		     (equal? index3-or-keyword-eval slice))
		
		(when (= 0 index4-or-keyword-or-step-eval)
		      (error "$bracket-apply$ : slice step cannot be zero"))
		
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
	       
	       
	       ((and (equal? slice index2-or-keyword-eval) ;; T[i1 $ i3 $] 
		     (equal? slice index4-or-keyword-or-step-eval))

		(let ((i1  index1-or-keyword-eval)
		      (i3 index3-or-keyword-eval))
		  (when (< i1 0) ;; negative index
			(set! i1 (+ (string-length container-eval) i1)))
		  (when (< i3 0) ;; negative index
			(set! i3 (+ (string-length container-eval) i3)))
		  (substring container-eval i1 i3)))

	       
	       ;; T[i1 i2 i3 i4] vector of vectors of vectors ... but we are in string context !!!
	       (else	
		(error "$bracket-apply$ : in string case, provided too much arguments:" index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval))))

	
	(else ;; T[i1 i2 i3 i4] ,  4 dimension array
	 (array-ref container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-keyword-or-step-eval)))) ;; return the element of the array
  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero








(define (apply-square-brackets-argument-5 container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)

  ;; {#(1 2 3 4 5 6 7 8 9)[2 $ 5 $ 1]}
  ;; '#(3 4 5)
  ;; {#(1 2 3 4 5 6 7 8 9)[5 $ 2 $ -1]}
  ;; '#(6 5 4)
  ;; {#(1 2 3 4 5 6 7 8 9)[2 $ 5 $ -1]}
  ;; '#()
  ;; {#(1 2 3 4 5 6 7 8 9)[-1 $ 5 $ -1]}
  ;; '#(9 8 7)
  ;; {#(1 2 3 4 5 6 7 8 9)[-0 $ 5 $ -1]}
  ;; '#()
  (cond ((or (vector? container-eval) (growable-vector? container-eval))
	 
	 (if (and (equal? index2-or-keyword-eval slice)  ;; T[i1 $ i3 $ s]
		  (equal? index4-or-keyword-eval slice))

	     (begin
	       
	       (when (= 0 index5-or-step-eval)
		 (error "$bracket-apply$ : slice step cannot be zero"))
	       
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
		      (i 0)
		      (make-container make-vector))

		  (when (growable-vector? container-eval)
			(set! make-container make-growable-vector))
		 
		  (when (not (= (modulo size-input index5-or-step-eval) 0))
			(set! size (+ 1 size)))

		 (if (<= size 0)
		     
		     (make-container 0)

		     (begin
		       (set! result (make-container size))
		       
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

	
	
	;; {"0123456789"[5 $ 2 $ -1]}
	;; "543"
	;; {"0123456789"[5 $  $ -1]}
	;; "543210"
	;; {"0123456789"[5 $ 0 $ -1]}
	;; "54321"
	((string? container-eval) 

	 (if (and (equal? index2-or-keyword-eval slice)  ;; T[i1 $ i3 $ s]
		  (equal? index4-or-keyword-eval slice))

	     (begin
	       
	       (when (= 0 index5-or-step-eval)
		 (error "$bracket-apply$ : slice step cannot be zero"))
	       
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
	     
	     (error "$bracket-apply$ : in string case, provided too much arguments:" index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)))

	
	(else ;; T[i1 i2 i3 i4 i5] ,  5 dimension array
	 (array-ref container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)))) ;; return the element of the array
  ;; note : i do not use negative indexes or slices for array because they could be not starting at zero




;; TODO :this code is only here to use Scheme+ but it should be in other place (scheme-infix.rkt)
;; split the expression using slice as separator
(def (parse-square-brackets-arguments args-brackets)

  (when (null? args-brackets)
	(return args-brackets))

  (declare result partial-result)
  
  (def (psba args) ;; parse square brackets arguments

       
       ;;(display partial-result) (newline)
       (when (null? args)
	     ;;(display "before !*prec") (newline)
	     {result <- (append result (!*prec partial-result))} ;; !*prec is defined in scheme-infix.scm
	     
	     ;;(display "after !*prec") (newline)
	     ;;(display result) (newline)
	     (return-rec result)) ;; return from all recursive calls
       
       {fst <+ (car args)}
       
       (if (equal? slice fst)
	   
	   ($>
	    (when (not (null? partial-result))
		  {result <- (append result (!*prec partial-result))} ;; evaluate and store the expression
		  {partial-result <- '()}) ;; empty for the next possible portion between slice operator
	    {result <- (append result (list fst))}) ;; append the slice operator
	   
	   {partial-result <- (append partial-result (list fst))}) ;; not a slice operator but append it
       
       (psba (cdr args))) ;; end def, recurse

  (psba args-brackets)) ;; initial call
