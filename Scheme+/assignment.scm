;; Translated from Scheme+ for Racket (assignment.rkt) for Guile.


;; assignment (Racket version)

;; This file is part of Scheme+

;; Copyright 2023-2025 Damien MATTEI

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

;; note: modified with AI agentic

(define-module (Scheme+ assignment)
  #:use-module (Scheme+ racket-compat))

 


  (use-modules
	((Scheme+ operators) #:select (BRACKET-APPLY-TYPED-INDEX-op? BRACKET-APPLY-VECTOR-NATURAL-op? BRACKET-APPLY-VECTOR-INTEGER-op? BRACKET-APPLY-STRING-NATURAL-op?))
	(Scheme+ parse-square-brackets)
	(ice-9 match)
	((srfi srfi-1) #:select (first second third fourth fifth))
	(srfi srfi-69)
	(Scheme+ parse-square-brackets)
	(Scheme+ for_next_step)
	(Scheme+ array)
	(Scheme+ slice)
	(Scheme+ declare)
	(Scheme+ block)
	(Scheme+ def-function)
	(Scheme+ defvar)
	(Scheme+ bracket-apply)
	(Scheme+ overload))


 
;; display { } as they are.
;;(print-mpair-curly-braces #f)


;; note that slicing separator is now : , no more $ (see slice.scm)

;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
;;> {v[7 : 3 : -2] <- (vector -1 -2 -3)}
;;. . ../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/assignment.rkt:103:38: :: undefined;
;; cannot reference an identifier before its definition
;;> {v[7 : 3 : -2] <- (vector -1 -2 -3)}
;;> v
;;'#(1 2 3 4 5 -2 7 -1 9)

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
;; > {s[2 * 3 - 4 : 2 * 3 + 1 : 2 * 4 - 6] <- "0000"}
;; "ab0d0f0h"


;; > (define T #(1 2 3 4 5 6 7))
;; > {T[T[2]]}
;; 4

;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro

(define-syntax <-
  
  (lambda (stx)
    
    (syntax-case stx ()

      ;; silly case
      ((_ ( ) expr)
       #'()) ;; void is not portable ;'())

      ;; one value in values !
      ;; > {(x) <- (values 7)}
      ;; > x
      ;; 7
      ((_ (var) expr)

       #'(define-or/and-set!-values (var) expr))

      ;; example: {a[4] <- 7}
      ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
      ((_ (brket-aply container index ...) expr)  ; possible to have NO index :
					; minimal form is (_ (brket-aply container) expr)

       #| We will let the second $bracket-apply$ be executed and use the return value
          but do not execute the first $bracket-apply$ as we needs to interpret the syntax and deal with their arguments. |#
       (cond	      

	 ; indexes typed ℕ (natural) or ℤ (integer) by the type annotation pass , vectors and strings (no slice) :
	 ; {v[i] := x} --> (vector-set! v i x) for ℕ , with the test of the sign for ℤ (a negative index counts from the
	 ; end as in Python) ; {M[i j] := x} : the same on the row M[i] ; see the accesses in bracket-apply.rkt
	 ((BRACKET-APPLY-TYPED-INDEX-op? (syntax->datum #'brket-aply))
	  (let* ((oper (syntax->datum #'brket-aply))
		 (natural-head? (or (BRACKET-APPLY-VECTOR-NATURAL-op? oper) (BRACKET-APPLY-STRING-NATURAL-op? oper)))
		 (vector-head? (or (BRACKET-APPLY-VECTOR-NATURAL-op? oper) (BRACKET-APPLY-VECTOR-INTEGER-op? oper)))
		 (indexes (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
									  (list (cons '* #'fx*)
										(cons '- #'fx-)
										(cons '+ #'fx+)
										(cons '/ #'fxquotient)))))
	    (with-syntax ((setter (if vector-head? #'vector-set! #'string-set!))
			  (getter (if vector-head? #'vector-ref #'string-ref))
			  (len (if vector-head? #'vector-length #'string-length)))
	      (cond ((and natural-head? (= (length indexes) 1))
		     (with-syntax ((i (car indexes)))
		       #'(setter container i expr)))
		    ((and natural-head? vector-head? (= (length indexes) 2))
		     (with-syntax ((i (car indexes)) (j (cadr indexes)))
		       #'(vector-set! (vector-ref container i) j expr)))
		    ((= (length indexes) 1)
		     (with-syntax ((index (car indexes)))
		       #'(let ((c container) (i index) (e expr))
			   (setter c (if (fx< i 0) (fx+ (len c) i) i) e))))
		    ((and vector-head? (= (length indexes) 2))
		     (with-syntax ((index1 (car indexes)) (index2 (cadr indexes)))
		       #'(let ((c container) (i index1) (j index2) (e expr))
			   (let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
			     (vector-set! row (if (fx< j 0) (fx+ (vector-length row) j) j) e)))))
		    (vector-head? #'(<- ($bracket-apply-vector$ container index ...) expr))
		    (else #'(<- ($bracket-apply-string$ container index ...) expr))))))

	 ; square brackets overloaded by overload-square-brackets , for an object of known type (written by the type
	 ; annotation pass) : ($bracket-apply-overloaded$ ((getter setter nb-indexes) ...) object arg-bracket ...)
	 ; the setter is chosen from the number of indexes : {Mv[i j] := x} --> (matrix-vect-set! Mv i j x) ,
	 ; otherwise the generic assignment chooses at runtime
	 ((eq? (quote $bracket-apply-overloaded$) (syntax->datum #'brket-aply))

	  (syntax-case #'(container index ...) ()

	    (((ovrld ...) object arg-bracket ...)

	     (let* ((parsed-args (parse-square-brackets-arguments-lister-syntax (syntax->list #'(arg-bracket ...))
										  (list (cons '* #'fx*)
											(cons '- #'fx-)
											(cons '+ #'fx+)
											(cons '/ #'fxquotient))))
		    (ovrld-found (select-overloaded-square-brackets (map syntax->list (syntax->list #'(ovrld ...)))
								    parsed-args)))

	       (if ovrld-found
		   (with-syntax ((setter (cadr ovrld-found))
				 ((idx ...) parsed-args))
		     #'(setter object idx ... expr))
		   #'(<- ($bracket-apply$ object arg-bracket ...) expr))))))

	 ; vector case (specialized)
         ((eq? (quote $bracket-apply-vector$) (syntax->datum #'brket-aply)) ;; curly-infix

          ;;(newline)
          ;; display { } as they are.
          ;;(print-mpair-curly-braces #f)

          ;; we parse arguments at posteriori of SRFI 105 parser
          (with-syntax

              ((parsed-args

                (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
                                                                            (list (cons '* #'fx*)
                                                                                  (cons '- #'fx-)
                                                                                  (cons '+ #'fx+)
                                                                                  (cons '/ #'fxquotient)))))
               ;;#`(list #,@(parse-square-brackets-arguments-lister-syntax #'(index ...))))
	      ;;(parse-square-brackets-arguments-lister-syntax #'(index ...)))

	       ;; is the RHS itself a {OtherVector[...]} form ? if so, we know at macro-expansion
	       ;; time (thanks to the type annotation pass having already rewritten it to
	       ;; $bracket-apply-vector$) that it evaluates to a vector, with no runtime vector?
	       ;; check needed ; this lets assignment-argument-*-vector use vector-copy! (a bulk
	       ;; copy) instead of an element-by-element loop for plain (step 1) slice assignment
               
               #| here we manipulate syntax, example above or: v3 so you can not test with vector?
                  unless it is already litteral which is not always the case |#
	       (is-vec (let ((d (syntax->datum #'expr))) 
			 ;(display "<- : d=") (display d) (newline)
			(if (and (pair? d) (eq? (car d) '$bracket-apply-vector$))
			    #'#t ; seems we can drop the syntax here : #t simple
			    #'#f)))

	      ) ; end definitions

	    ;(display "<- : #'is-vec=") (display #'is-vec) (newline)
	    ;(display "<- : #'parsed-args=") (display #'parsed-args) (newline)
            
	   ;; (display "<- : (list? #'parsed-args)=") (display (list? #'parsed-args)) (newline)
	   ;; (display "<- : (mlist? #'parsed-args)=") (display (mlist? #'parsed-args)) (newline)
	   ;; (display "<- : (length #'parsed-args)=") (display (length #'parsed-args)) (newline)

	   ;; we parse arguments at posteriori
	   ;; putting code here optimise run-time
	   (case (length
		  (cdr
		   (syntax->datum #'parsed-args)))


	     ;; 0 argument in []
	     ;; T[]
	     ;; {v[] <- #(1 2 3)}
	     ;; > v
	     ;;'#(1 2 3)
	     ((0)
	      ;;#'(begin
	      ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
	      #'(assignment-argument-0 container expr));)  ; possible to have NO index

	     ;; 1 argument in [ ]
	     ;; T[index]
	     ((1)
	      ;;(begin
	      ;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
	      ;; written inline for an index (not a slice) , as the access in bracket-apply.rkt
	      #;#'(assignment-argument-1-vector container
                                                (first parsed-args)
                                                expr)
	      (with-syntax ((index (cadr (syntax->list #'parsed-args))))
		#'(let ((c container) (i index) (e expr))
		    (if (fixnum? i)
			(vector-set! c (if (fx< i 0) (fx+ (vector-length c) i) i) e) ; a negative index counts from the end as in Python
			(assignment-argument-1-vector c i e)))));)

	     ;; 2 arguments in [ ]
	     ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
	     ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
	     ;; '#(3 4 5)
	     ((2)
	      ;; #'(begin
	      ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
	      ;; M[i j] of a vector of vectors written inline , each negative index counts from the end of its own vector
	      #;#'(assignment-argument-2-vector container
                                                (first parsed-args)
                                                (second parsed-args)
                                                expr
                                                is-vec)
	      (with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
		#'(let ((c container) (i index1) (j index2) (e expr))
		    (if (and (fixnum? i) (fixnum? j)) ; two indexes , not slices
			(let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
			  (vector-set! row (if (fx< j 0) (fx+ (vector-length row) j) j) e))
			(assignment-argument-2-vector c i j e is-vec)))));)

	     ;; 3 arguments in [ ]
	     ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
	     ((3)
	      ;; #'(begin
	      ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
	      ;;#'parsed-args);)
	      #'(assignment-argument-3-vector container
                                              (first parsed-args)
                                              (second parsed-args)
                                              (third parsed-args)
                                              expr
                                              is-vec));)

	     ;; 4 arguments in [ ]
	     ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
	     ((4) #'(assignment-argument-4-vector container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  expr
                                                  is-vec))

	     ;; 5 arguments in [ ]
	     ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
	     ((5) #'(assignment-argument-5-vector container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  (fifth parsed-args)
                                                  expr
                                                  is-vec))

	     ;; more than 5 arguments in [ ]
	     ;; T[i1 i2 i3 i4 i5 i6 ...]
	     (else ; case
	      #'(assignment-argument-6-and-more-vector container parsed-args expr)))))


	 ; string case (specialized)
         ((eq? (quote $bracket-apply-string$) (syntax->datum #'brket-aply)) ;; curly-infix

          ;; we parse arguments at posteriori of SRFI 105 parser
          (with-syntax

              ((parsed-args

                (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
                                                                            (list (cons '* #'fx*)
                                                                                  (cons '- #'fx-)
                                                                                  (cons '+ #'fx+)
                                                                                  (cons '/ #'fxquotient)))))

	       ;; is the RHS itself a {OtherString[...]} form ? if so, we know at macro-expansion
	       ;; time (thanks to the type annotation pass having already rewritten it to
	       ;; $bracket-apply-string$) that it evaluates to a string, with no runtime string?
	       ;; check needed ; this lets assignment-argument-*-string use string-copy! (a bulk
	       ;; copy) instead of an element-by-element loop for plain (step 1) slice assignment
	       (is-str (let ((d (syntax->datum #'expr)))
			 (if (and (pair? d) (eq? (car d) '$bracket-apply-string$))
			     #'#t
			     #'#f)))

	      ) ; end definitions

	   ;; we parse arguments at posteriori
	   ;; putting code here optimise run-time
	   (case (length
		  (cdr
		   (syntax->datum #'parsed-args)))


	     ;; 0 argument in []
	     ;; T[]
	     ;; {s[] <- "abc"}
	     ((0)
	      #'(assignment-argument-0 container expr))  ; possible to have NO index

	     ;; 1 argument in [ ]
	     ;; T[index]
	     ((1)
	      ;; written inline as for the vectors : an index (not a slice) , negative from the end as in Python
	      #;#'(assignment-argument-1-string container
                                                (first parsed-args)
                                                expr)
	      (with-syntax ((index (cadr (syntax->list #'parsed-args))))
		#'(let ((c container) (i index) (e expr))
		    (if (fixnum? i)
			(string-set! c (if (fx< i 0) (fx+ (string-length c) i) i) e)
			(assignment-argument-1-string c i e)))))

	     ;; 2 arguments in [ ]
	     ;; ex: T[i1 :] , T[: i2] , T[: :]
	     ((2)
	      #'(assignment-argument-2-string container
                                              (first parsed-args)
                                              (second parsed-args)
                                              expr
                                              is-str))

	     ;; 3 arguments in [ ]
	     ;; T[i1 : i2] , T[: : s]
	     ((3)
	      #'(assignment-argument-3-string container
                                              (first parsed-args)
                                              (second parsed-args)
                                              (third parsed-args)
                                              expr
                                              is-str))

	     ;; 4 arguments in [ ]
	     ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :]
	     ((4) #'(assignment-argument-4-string container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  expr
                                                  is-str))

	     ;; 5 arguments in [ ]
	     ;; T[i1 : i3 : s]
	     ((5) #'(assignment-argument-5-string container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  (fifth parsed-args)
                                                  expr
                                                  is-str))

	     ;; more than 5 arguments in [ ] : not allowed with strings
	     (else ; case
	      #'(assignment-argument-6-and-more-string container parsed-args expr)))))


	 ; hash table case (specialized)
         ((eq? (quote $bracket-apply-hash-table$) (syntax->datum #'brket-aply)) ;; curly-infix

          ;; we parse arguments at posteriori of SRFI 105 parser
          (with-syntax

              ((parsed-args

                (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
                                                                            (list (cons '* #'fx*)
                                                                                  (cons '- #'fx-)
                                                                                  (cons '+ #'fx+)
                                                                                  (cons '/ #'fxquotient)))))

	      ) ; end definitions

	   ;; we parse arguments at posteriori
	   ;; putting code here optimise run-time
	   (case (length
		  (cdr
		   (syntax->datum #'parsed-args)))


	     ;; 0 argument in []
	     ;; T[]
	     ;; {h[] <- expr}
	     ((0)
	      #'(assignment-argument-0 container expr))  ; possible to have NO index

	     ;; 1 argument in [ ]
	     ;; T[key]
	     ((1)
	      #'(assignment-argument-1-hash-table container
                                                   (first parsed-args)
                                                   expr))

	     ;; more than 1 argument in [ ] : not allowed with hash tables
	     ((2)
	      #'(assignment-argument-2-hash-table container
                                                   (first parsed-args)
                                                   (second parsed-args)
                                                   expr))

	     ((3)
	      #'(assignment-argument-3-hash-table container
                                                   (first parsed-args)
                                                   (second parsed-args)
                                                   (third parsed-args)
                                                   expr))

	     ((4) #'(assignment-argument-4-hash-table container
                                                       (first parsed-args)
                                                       (second parsed-args)
                                                       (third parsed-args)
                                                       (fourth parsed-args)
                                                       expr))

	     ((5) #'(assignment-argument-5-hash-table container
                                                       (first parsed-args)
                                                       (second parsed-args)
                                                       (third parsed-args)
                                                       (fourth parsed-args)
                                                       (fifth parsed-args)
                                                       expr))

	     (else ; case
	      #'(assignment-argument-6-and-more-hash-table container parsed-args expr)))))


	 ; array case (specialized)
         ((eq? (quote $bracket-apply-array$) (syntax->datum #'brket-aply)) ;; curly-infix

          ;; we parse arguments at posteriori of SRFI 105 parser
          (with-syntax

              ((parsed-args

                (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
                                                                            (list (cons '* #'fx*)
                                                                                  (cons '- #'fx-)
                                                                                  (cons '+ #'fx+)
                                                                                  (cons '/ #'fxquotient)))))

	      ) ; end definitions

	   ;; we parse arguments at posteriori
	   ;; putting code here optimise run-time
	   (case (length
		  (cdr
		   (syntax->datum #'parsed-args)))


	     ;; 0 argument in []
	     ;; T[]
	     ;; {a[] <- expr}
	     ((0)
	      #'(assignment-argument-0 container expr))  ; possible to have NO index

	     ;; 1 argument in [ ]
	     ;; T[i1] ,  1 dimension array
	     ((1)
	      #'(assignment-argument-1-array container
                                             (first parsed-args)
                                             expr))

	     ;; 2 arguments in [ ]
	     ;; T[i1 i2] ,  2 dimension array
	     ((2)
	      #'(assignment-argument-2-array container
                                             (first parsed-args)
                                             (second parsed-args)
                                             expr))

	     ;; 3 arguments in [ ]
	     ;; T[i1 i2 i3] ,  3 dimension array
	     ((3)
	      #'(assignment-argument-3-array container
                                             (first parsed-args)
                                             (second parsed-args)
                                             (third parsed-args)
                                             expr))

	     ;; 4 arguments in [ ]
	     ;; T[i1 i2 i3 i4] ,  4 dimension array
	     ((4) #'(assignment-argument-4-array container
                                                 (first parsed-args)
                                                 (second parsed-args)
                                                 (third parsed-args)
                                                 (fourth parsed-args)
                                                 expr))

	     ;; 5 arguments in [ ]
	     ;; T[i1 i2 i3 i4 i5] ,  5 dimension array
	     ((5) #'(assignment-argument-5-array container
                                                 (first parsed-args)
                                                 (second parsed-args)
                                                 (third parsed-args)
                                                 (fourth parsed-args)
                                                 (fifth parsed-args)
                                                 expr))

	     ;; more than 5 arguments in [ ] , n dimension array
	     (else ; case
	      #'(assignment-argument-6-and-more-array container parsed-args expr)))))


        ; general case (not specialized for optimisation)
        ((eq? (quote $bracket-apply$) (syntax->datum #'brket-aply)) ;; curly-infix

         ;;(newline)
         ;; display { } as they are.
         ;;(print-mpair-curly-braces #f)

         ;;(display "<- : #'(index ...) = ") (display #'(index ...)) (newline)

         ;; we parse arguments at posteriori of SRFI 105 parser
         (with-syntax
              ;;(let-syntax
              ;; (let
              ((parsed-args

                (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
									    (list (cons '* #'fx*)
										  (cons '- #'fx-)
										  (cons '+ #'fx+)
										  (cons '/ #'fxquotient)))))
               ;;#`(list #,@(parse-square-brackets-arguments-lister-syntax #'(index ...))))
               ;;(parse-square-brackets-arguments-lister-syntax #'(index ...)))

               ) ; end definitions
            
            ;;(display "<- : #'parsed-args=") (display #'parsed-args) (newline)
            ;; (display "<- : (list? #'parsed-args)=") (display (list? #'parsed-args)) (newline)
            ;; (display "<- : (mlist? #'parsed-args)=") (display (mlist? #'parsed-args)) (newline)
            ;; (display "<- : (length #'parsed-args)=") (display (length #'parsed-args)) (newline)

           ;; we parse arguments at posteriori
            ;; putting code here optimise run-time
            (case (length
                   (cdr
                    (syntax->datum #'parsed-args)))
              
              
              ;; 0 argument in []
              ;; T[]
              ;; {v[] <- #(1 2 3)}
              ;; > v
              ;;'#(1 2 3)
              ((0) 
               ;;#'(begin
               ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
               #'(assignment-argument-0 container expr));)  ; possible to have NO index

              ;; 1 argument in [ ]
              ;; T[index]
              ((1)
               ;;(begin
               ;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
               ;; written inline for a vector and an index (not a slice) , as the access in bracket-apply.rkt
               #;#'(assignment-argument-1 container
                                          (first parsed-args)
                                          expr)
               (with-syntax ((index (cadr (syntax->list #'parsed-args))))
                 #'(let ((c container) (i index) (e expr))
                     (cond ((and (vector? c) (fixnum? i))
                            (vector-set! c (if (fx< i 0) (fx+ (vector-length c) i) i) e)) ; a negative index counts from the end as in Python
                           ((and (string? c) (fixnum? i))
                            (string-set! c (if (fx< i 0) (fx+ (string-length c) i) i) e))
                           (else (assignment-argument-1 c i e))))));)

              ;; 2 arguments in [ ]
              ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]   
              ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
              ;; '#(3 4 5)
              ((2)
               ;; #'(begin
               ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
               ;; M[i j] of a vector of vectors written inline , as the access in bracket-apply.rkt
               #;#'(assignment-argument-2 container
                                          (first parsed-args)
                                          (second parsed-args)
                                          expr)
               (with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
                 #'(let ((c container) (i index1) (j index2) (e expr))
                     (cond ((and (vector? c) (fixnum? i) (fixnum? j))
                            (let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
                              (vector-set! row (if (fx< j 0) (fx+ (vector-length row) j) j) e)))
                           #;((and (flomat? c) (fixnum? i) (fixnum? j)) ; flomat : Racket only
                            (mset! c i j e))
                           (else (assignment-argument-2 c i j e))))));)

              ;; 3 arguments in [ ]
              ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
              ((3)
               ;; #'(begin
               ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
               ;;#'parsed-args);)
               #'(assignment-argument-3 container					  
                                        (first parsed-args)
                                        (second parsed-args)
                                        (third parsed-args)
                                        expr));)
              
              ;; 4 arguments in [ ]
              ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
              ((4) #'(assignment-argument-4 container
                                            (first parsed-args)
                                            (second parsed-args)
                                            (third parsed-args)
                                            (fourth parsed-args)
                                            expr))

              ;; 5 arguments in [ ]
              ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
              ((5) #'(assignment-argument-5 container
                                            (first parsed-args)
                                            (second parsed-args)
                                            (third parsed-args)
                                            (fourth parsed-args)
                                            (fifth parsed-args)
                                            expr))

              ;; more than 5 arguments in [ ]
              ;; T[i1 i2 i3 i4 i5 i6 ...]
              (else ; case
               #'(assignment-argument-6-and-more container parsed-args expr)))))


	 ;; flomat case (removed : sci requires libgfortran)
	 #;((eq? (quote $bracket-apply-flomat$) (syntax->datum #'brket-aply)) ;; curly-infix

	  ;;(newline)
	  ;; display { } as they are.
	  ;;(print-mpair-curly-braces #f)

	  ;; we parse arguments at posteriori of SRFI 105 parser
	  (with-syntax
	      
	      ((parsed-args

		(cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
									    (list (cons '* #'fx*)
										  (cons '- #'fx-)
										  (cons '+ #'fx+)
										  (cons '/ #'fxquotient)))))
	       ;;#`(list #,@(parse-square-brackets-arguments-lister-syntax #'(index ...))))
	       ;;(parse-square-brackets-arguments-lister-syntax #'(index ...)))

	       ) ; end definitions
	    
	    ;; (display "<- : #'parsed-args=") (display #'parsed-args) (newline)
	    ;; (display "<- : (list? #'parsed-args)=") (display (list? #'parsed-args)) (newline)
	    ;; (display "<- : (mlist? #'parsed-args)=") (display (mlist? #'parsed-args)) (newline)
	    ;; (display "<- : (length #'parsed-args)=") (display (length #'parsed-args)) (newline)

	    ;; we parse arguments at posteriori
	    ;; putting code here optimise run-time
	    (case (length
		   (cdr
		    (syntax->datum #'parsed-args)))
	      
	      
	     ;; 0 argument in []
	     ;; T[]
	     ;; {v[] <- #(1 2 3)}
	     ;; > v
	     ;;'#(1 2 3)
	     ((0) 
	      ;;#'(begin
	      ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
	      #'(assignment-argument-0 container expr));)  ; possible to have NO index

	     ;; 1 argument in [ ]
	     ;; T[index] : row setting not allowed with flomat
	     ((1)
	      ;;(begin
	      ;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
	      #'(assignment-argument-1-flomat container
                                              (first parsed-args)
                                              expr));)

	     ;; 2 arguments in [ ]
	     ;; T[i1 i2] : element (row,col) assignment ; slicing not allowed with flomat
	     ((2)
	      ;; #'(begin
	      ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
	      ;; the element F[i j] written inline (flomat : Racket only) : no slice , no negative index , as before
	      #;#'(assignment-argument-2-flomat container
                                                (first parsed-args)
                                                (second parsed-args)
                                                expr)
	      (with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
		#'(let ((c container) (i index1) (j index2) (e expr))
		    (if (and (fixnum? i) (fixnum? j))
			(mset! c i j e)
			(assignment-argument-2-flomat c i j e)))));)

	     ;; 3 arguments in [ ] : not allowed with flomat
	     ((3)
	      ;; #'(begin
	      ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
	      ;;#'parsed-args);)
	      #'(assignment-argument-3-flomat container
                                              (first parsed-args)
                                              (second parsed-args)
                                              (third parsed-args)
                                              expr));)

	     ;; 4 arguments in [ ] : not allowed with flomat
	     ((4) #'(assignment-argument-4-flomat container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  expr))

	     ;; 5 arguments in [ ] : not allowed with flomat
	     ((5) #'(assignment-argument-5-flomat container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  (fifth parsed-args)
                                                  expr))

	     ;; more than 5 arguments in [ ] : not allowed with flomat
	     (else ; case
	      #'(assignment-argument-6-and-more-flomat container parsed-args expr)))))


	 

         ;; i suppose the example below is done in the 'else' of 'cond'
         ;; {(x y z) <- (values 1 2 3)}
         (else ; cond
          #'(define-or/and-set!-values (brket-aply container index ...) expr)))) ;; the argument's names does not match the use
      

      
      ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
      ;; We will let the second $bracket-apply$ be executed and block the execution of first $bracket-apply$.
      
      ;; one dimension array, example: {a[4] <- 7}
      ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
      ;; ((_ (bracket-apply container index index1 ...) expr)

      ;;  (begin

      ;;    ;; add a checking
      ;;    ;; (define x 3)
      ;;    ;; > (<- (aye x 3) 7)
      ;;    ;; . . ../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/required-files/assignment.rkt:1:6: Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index) , first argument  'aye " is not $bracket-apply$."
      ;;    (unless (eq? (quote $bracket-apply$) (quote bracket-apply)) 
      ;; 	       (error "Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index ...) , first argument is not $bracket-apply$:"
      ;; 		      (quote bracket-apply)))

      ;;    ;;(display "<- : container name:") (display (quote container)) (newline)
      ;;    ;;(display "<- : container:") (display container) (newline)
      ;;    ;;(display "<- : expr:") (display expr) (newline)
       
    ;;(<- x 5)
    ((_ var expr)
     
     ;; (begin
     ;;   (display "<- : variable set!") (newline)
     ;;   (display "var =") (display var) (newline)
     ;;   (display "expr =") (display expr) (newline)
     ;;#`(set! var expr))
       ;; (display "after set! : var =") (display var) (newline)))
     ;;var))

     #'(if-defined var
		   (set! var expr)
		   (define var expr)))

    
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

    ;; > (declare a b c d)
    ;; > {(a b) <- (c d) <- (values 5 7)}
    ;; > a
    ;; 5
    ;; > b
    ;; 7
    ;; > c
    ;; 5
    ;; > d
    ;; 7

    ;; without declare:
    ;; > {(a b) <- (c d) <- (values 5 7)}
    ;; > (list a b c d)
    ;; '(5 7 5 7)
    ((_ var var1 ... expr) ; i suppose varN are of the type (x y z ...)
     
     ;;(<- var (<- var1 ... expr))) ; probably this is commented because the syntax is not allowed

     #'(begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result
	 ;;(display "<- : case (_ var var1 ... expr)") (newline)
	 ;;(<- var expr)
	 ;; create-return-values computed again expr for each var : {s <- T[0] <- (cons 9 s)} gave T[0] = (9 9 . s)
	 ;;(define return-values-of-expr (create-return-values expr))
	 (define values-of-expr (call-with-values (lambda () expr) list)) ; expr is computed only once
	 ;;(<- var (return-values-of-expr))
	 (<- var (apply values values-of-expr))
	 ;;(display "<- : case : passed (<- var expr)") (newline)
	 ;;(display "<- : case : var=") (display var) (newline)
	 ;;(<- var1 var)
	 ;;(<- var1 (return-values-of-expr))
	 (<- var1 (apply values values-of-expr))
	 ...))

    )))



; specialized for vectors only
(define-syntax <-vector-vector*
  
  (lambda (stx)
    
    (syntax-case stx ()

      
      ;; example: {a[4] <- 7}
      ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
      ((_ (brket-aply container index ...) expr)  ; possible to have NO index :
					; minimal form is (_ (brket-aply container) expr)

       #| We will let the second $bracket-apply$ be executed and use the return value
          but do not execute the first $bracket-apply$ as we needs to interpret the syntax and deal with their arguments. |#
      
          ;; display { } as they are.
          ;;(print-mpair-curly-braces #f)

          ;; we parse arguments at posteriori of SRFI 105 parser
          (with-syntax

              ((parsed-args

                (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
                                                                            (list (cons '* #'fx*)
                                                                                  (cons '- #'fx-)
                                                                                  (cons '+ #'fx+)
                                                                                  (cons '/ #'fxquotient)))))) ; end definitions

	    ;(display "<- : #'parsed-args=") (display #'parsed-args) (newline)
       
	   ;; we parse arguments at posteriori
	   ;; putting code here optimise run-time
	   (case (length
		  (cdr
		   (syntax->datum #'parsed-args)))


	     ;; 0 argument in []
	     ;; T[]
	     ;; {v[] <- #(1 2 3)}
	     ;; > v
	     ;;'#(1 2 3)
	     ((0)
	      ;;#'(begin
	      ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
	      #'(assignment-argument-0 container expr));)  ; possible to have NO index

	     ;; 1 argument in [ ]
	     ;; T[index]
	     ((1)
	      ;;(begin
	      ;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
	      ;; written inline for an index (not a slice) , as the access in bracket-apply.rkt
	      #;#'(assignment-argument-1-vector container
                                                (first parsed-args)
                                                expr)
	      (with-syntax ((index (cadr (syntax->list #'parsed-args))))
		#'(let ((c container) (i index) (e expr))
		    (if (fixnum? i)
			(vector-set! c (if (fx< i 0) (fx+ (vector-length c) i) i) e) ; a negative index counts from the end as in Python
			(assignment-argument-1-vector c i e)))));)

	     ;; 2 arguments in [ ]
	     ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
	     ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
	     ;; '#(3 4 5)
	     ((2)
	      ;; #'(begin
	      ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
	      ;; M[i j] of a vector of vectors written inline , each negative index counts from the end of its own vector
	      #;#'(assignment-argument-2-vector container
                                                (first parsed-args)
                                                (second parsed-args)
                                                expr
                                                #t)
	      (with-syntax (((index1 index2) (cdr (syntax->list #'parsed-args))))
		#'(let ((c container) (i index1) (j index2) (e expr))
		    (if (and (fixnum? i) (fixnum? j)) ; two indexes , not slices
			(let ((row (vector-ref c (if (fx< i 0) (fx+ (vector-length c) i) i))))
			  (vector-set! row (if (fx< j 0) (fx+ (vector-length row) j) j) e))
			(assignment-argument-2-vector c i j e #t)))));)

	     ;; 3 arguments in [ ]
	     ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
	     ((3)
	      ;; #'(begin
	      ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
	      ;;#'parsed-args);)
	      #'(assignment-argument-3-vector container
                                              (first parsed-args)
                                              (second parsed-args)
                                              (third parsed-args)
                                              expr
                                              #t));)

	     ;; 4 arguments in [ ]
	     ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
	     ((4) #'(assignment-argument-4-vector container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  expr
                                                  #t))

	     ;; 5 arguments in [ ]
	     ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
	     ((5) #'(assignment-argument-5-vector container
                                                  (first parsed-args)
                                                  (second parsed-args)
                                                  (third parsed-args)
                                                  (fourth parsed-args)
                                                  (fifth parsed-args)
                                                  expr
                                                  #t))

	     ;; more than 5 arguments in [ ]
	     ;; T[i1 i2 i3 i4 i5 i6 ...]
	     (else ; case
	      #'(assignment-argument-6-and-more-vector container parsed-args expr)))))

       
    ;;(<- x 5)
    ((_ var expr)
     
     ;; (begin
     ;;   (display "<- : variable set!") (newline)
     ;;   (display "var =") (display var) (newline)
     ;;   (display "expr =") (display expr) (newline)
     ;;#`(set! var expr))
       ;; (display "after set! : var =") (display var) (newline)))
     ;;var))

     #'(if-defined var
		   (set! var expr)
		   (define var expr)))

    ;; Warning: not sure if code below should also be kept in specialized macros
    
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

    ;; > (declare a b c d)
    ;; > {(a b) <- (c d) <- (values 5 7)}
    ;; > a
    ;; 5
    ;; > b
    ;; 7
    ;; > c
    ;; 5
    ;; > d
    ;; 7

    ;; without declare:
    ;; > {(a b) <- (c d) <- (values 5 7)}
    ;; > (list a b c d)
    ;; '(5 7 5 7)
    ((_ var var1 ... expr) ; i suppose varN are of the type (x y z ...)
     
     ;;(<- var (<- var1 ... expr))) ; probably this is commented because the syntax is not allowed

     #'(begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result
	 ;;(display "<- : case (_ var var1 ... expr)") (newline)
	 ;;(<- var expr)
	 ;; create-return-values computed again expr for each var
	 ;;(define return-values-of-expr (create-return-values expr))
	 (define values-of-expr (call-with-values (lambda () expr) list)) ; expr is computed only once
	 ;;(<-vector-vector* var (return-values-of-expr))
	 (<-vector-vector* var (apply values values-of-expr))
	 ;;(display "<- : case : passed (<- var expr)") (newline)
	 ;;(display "<- : case : var=") (display var) (newline)
	 ;;(<- var1 var)
	 ;;(<-vector-vector* var1 (return-values-of-expr))
	 (<-vector-vector* var1 (apply values values-of-expr))
	 ...)))))





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

;; > {T[3] -> T[7 2 4]}

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



(define-syntax :=

  (syntax-rules ()

    ((_ var ...) (<- var ...))))


(define-syntax =:

  (syntax-rules ()

    ((_ expr ...) (-> expr ...))))



;; the walrus operator of Python : assigns and returns the value
;; > (declare x y)
;; > {y := {x :=> 5} + 1}
;; > (list x y)
;; '(5 6)
;; Guile : := is an assignment only , the variables must exist before (declare , <+ or define) : Guile reports an
;; unbound variable otherwise (Racket checks it at expansion time with identifier-binding)
;; Guile : the pattern (== slice) of racket/match does not exist in (ice-9 match) : (? slice?) ; and the pattern
;; (list p ...) of racket/match is written (p ...) in (ice-9 match)
(define (slice? x) (equal? x slice))


(define-syntax :=>
  (syntax-rules ()
    ((_ place ... expr)
     (let ((v expr)) ; expr computed once
       (:= place ... v) ; any place of := : a variable , T[i] , a chain {a :=> b :=> 3}
       v))))



;; (declare x y z)
;;  {(x y z) <v (values 2 4 5)}
;; > (list x y z)
;; '(2 4 5)
;; > (declare u v w)
;; > {(x y z) <v (u v w) <v (values 2 4 5)}

;; > (list x y z u v w)
;; '(2 4 5 2 4 5)
;; > (declare a b c)
;; > {(x y z) <v (u v w) <v (a b c) <v (values 2 4 5)}

;; > (list x y z u v w a b c)
;; '(2 4 5 2 4 5 2 4 5)
;;
;; {(x y z) <v (u v w) <v (a b c) <v (values 5 6 7)}
;; >  (list x y z u v w a b c)
;; '(5 6 7 5 6 7 5 6 7)
;; (define T (make-vector 5))
;; {(x {T[4]} z) <v (values 1 2 3)}

;; {T[4]}
;; 2

;; > (declare u v w a b c)
;; > {(a b c) <v (x {T[4]} z) <v (u v w) <v (values 1 2 3)}

;; > (list a b c x {T[4]} z u v w)
;; '(1 2 3 1 2 3 1 2 3)

;; > {(x {T[4]} z) <v (u v w) <v (a b c) <v (values 1 2 3)}

;; > (list a b c x {T[4]} z u v w)
;; '(1 2 3 1 2 3 1 2 3)
;; > {(a b c)  <v (u v w) <v (x {T[4]} z) <v (values 1 2 3)}

;; > (list a b c x {T[4]} z u v w)
;; '(1 2 3 1 2 3 1 2 3)

(define-syntax <v
  
  (syntax-rules ()
    
    ((_ (var1 ...) expr) ;;(begin
			   (set!-values-plus (var1 ...) expr)
			   ;;(values var1 ...)))
			   )

    ((_ (var10 ...) (var11 ...) ... expr)
     
     ;;(<v (var10 ...) (<v (var11 ...) ... expr)))

     (begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result 
       (<v (var10 ...) expr)
       (let ((return-values (lambda () (values var10 ...)))) ;; to skip recomputation of expr
	 (<v (var11 ...) (return-values))
	 ...)))
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





; macro save lines of code
(define-syntax check-step

  (syntax-rules ()

    ((_ step)  (when (= step 0)
		     (error "assignment : slice step cannot be zero")))))



;; TODO assignment must be optimized also with expr (not only container) this force to have a lot of different functions such as copy-slice-with-negative-step etc... adapted for expression and container , this comment seems to concern only slice : ,sliced expressions.

;; copy-slice-with-negative-step
(define (copy-slice-with-negative-step container-eval expr-eval i1 i2 step)
  (for (($> (<+ k i1) (<+ i 0)) (> k i2) (<- k (+ k step)))
       (assignment-argument-1-index container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

(define (copy-slice-with-negative-step-vector container-eval expr-eval i1 i2 step)
  (for (($> (<+ k i1) (<+ i 0)) (> k i2) (<- k (+ k step)))
       (assignment-argument-1-index-vector container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copy-slice-starting-at-zero-with-negative-step
(define (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval i1 step)
  (for (($> (<+ k i1) (<+ i 0)) (>= k 0) (<- k (+ k step)))
       (assignment-argument-1-index container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

(define (copy-slice-starting-at-zero-with-negative-step-vector container-eval expr-eval i1 step)
  (for (($> (<+ k i1) (<+ i 0)) (>= k 0) (<- k (+ k step)))
       (assignment-argument-1-index-vector container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; copy-slice-with-positive-step
(define (copy-slice-with-positive-step container-eval expr-eval i1 i2 step)
  (for (($> (<+ k i1) (<+ i 0)) (< k i2) (<- k (+ k step)))
       (assignment-argument-1-index container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

(define (copy-slice-with-positive-step-vector container-eval expr-eval i1 i2 step) ; container vector
  (for (($> (<+ k i1) (<+ i 0)) (< k i2) (<- k (+ k step)))
       (assignment-argument-1-index-vector container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

;; fast path used only when expr is statically known (via $bracket-apply-vector$ syntax
;; in the <- macro) to be a vector : vector-copy! is a bulk memory copy, much faster than
;; the element-by-element loop above, but it does not support a step different than 1
(define (copy-slice-with-positive-step-vector-vector container-eval expr-eval i1 i2 step)
  (if (= step 1)
      (vector-copy! container-eval i1 expr-eval 0 (- i2 i1))
      (copy-slice-with-positive-step-vector container-eval expr-eval i1 i2 step)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; copy-slice-starting-at-zero-with-positive-step
(define (copy-slice-starting-at-zero-with-positive-step container-eval expr-eval i2 step)
  ;;(display "copy-slice-starting-at-zero-with-positive-step : container-eval=") (display container-eval) (newline)
  (for (($> (<+ k 0) (<+ i 0)) (< k i2) (<- k (+ k step)))
       (<+ bkt (apply-square-brackets-argument-1 expr-eval i))
       ;;(display "bkt=") (display bkt) (newline)
       (assignment-argument-1-index container-eval k bkt)
       (<- i (+ i 1))))


(define (copy-slice-starting-at-zero-with-positive-step-vector container-eval expr-eval i2 step) ; container vector
  ;;(display "copy-slice-starting-at-zero-with-positive-step-vector : container-eval=") (display container-eval) (newline)
  (for (($> (<+ k 0) (<+ i 0)) (< k i2) (<- k (+ k step)))
        (<+ bkt (apply-square-brackets-argument-1 expr-eval i))
        (assignment-argument-1-index-vector container-eval k bkt)
        (<- i (+ i 1))))

;; fast path : see copy-slice-with-positive-step-vector-vector
(define (copy-slice-starting-at-zero-with-positive-step-vector-vector container-eval expr-eval i2 step)
  (if (= step 1)
      (vector-copy! container-eval 0 expr-eval 0 i2)
      (copy-slice-starting-at-zero-with-positive-step-vector container-eval expr-eval i2 step)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; copy-stepped-slice
(define (copy-stepped-slice container-eval expr-eval i1 i2 step)

  (check-step step)

  (if (< step 0) ;; with negative step we start at end of vector (like in Python)
      
      (copy-slice-with-negative-step container-eval expr-eval i1 i2 step)
         
      (copy-slice-with-positive-step container-eval expr-eval i1 i2 step)))


(define (copy-stepped-slice-vector container-eval expr-eval i1 i2 step)

  (check-step step)

  (if (< step 0) ;; with negative step we start at end of vector (like in Python)

      (copy-slice-with-negative-step-vector container-eval expr-eval i1 i2 step)

      (copy-slice-with-positive-step-vector container-eval expr-eval i1 i2 step)))

;; fast path : see copy-slice-with-positive-step-vector-vector ; there is no fast path for
;; a negative step (vector-copy! only copies forward), so that branch is unchanged
(define (copy-stepped-slice-vector-vector container-eval expr-eval i1 i2 step)

  (check-step step)

  (if (< step 0)

      (copy-slice-with-negative-step-vector container-eval expr-eval i1 i2 step)

      (copy-slice-with-positive-step-vector-vector container-eval expr-eval i1 i2 step)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; macro save lines of code
(define-syntax negative->positive-index-when-not-slice
  
  (syntax-rules ()

    ((_ i container-length container-eval) 
     ;; transform the negative indexes in positive ones when not slices
     (when (and (not (eq? i slice))  (< i 0))
	   (<- i (+ (container-length container-eval) i))))))


;; T[:]
(define (assignment-argument-1-slice container-eval expr-eval)

  (cond ((vector? container-eval)
	 (vector-copy! container-eval
		       0
		       expr-eval))
	
	((hash-table? container-eval) (error "slicing not permitted with hash table"))
	
	((string? container-eval)
	 (string-copy! container-eval
		       0
		       expr-eval))
	(else (error "slicing not allowed with this container"))))


;; T[:]
(define (assignment-argument-1-slice-vector container-eval expr-eval)

  (vector-copy! container-eval
                0
                expr-eval))


;; normal case
;; {T[3] <- T[7 2 4]}
      
(define (assignment-argument-1-index container-eval index-eval expr-eval)

  ;;(display "assignment-argument-1-index  : container-eval=") (display container-eval) (newline)

  (cond ((vector? container-eval)
	 (when (< index-eval 0) ;; deal with negative index
           (<- index-eval (+ (vector-length container-eval) index-eval)))
	 (vector-set! container-eval index-eval expr-eval))
	
	((hash-table? container-eval)
	 (hash-table-set! container-eval index-eval expr-eval))
	
	((string? container-eval)
	 (when (< index-eval 0) ;; deal with negative index
	       (<- index-eval (+ (string-length container-eval) index-eval)))
	 (string-set! container-eval index-eval expr-eval))

	#;((flomat? container-eval)
	 (error "row setting not allowed with flomat"))
	
	((array? container-eval)
	 (array-set! container-eval index-eval expr-eval))

	(else ;; overloaded
	 (begin-def
	  (define args-lst (list container-eval index-eval))
	  (define setter! (find-setter-for-overloaded-square-brackets args-lst))
	  (setter! container-eval index-eval expr-eval)))))


;; {T[3] <- T[7 2 4]}     
(define (assignment-argument-1-index-vector container-eval index-eval expr-eval)

  ;;(display "assignment-argument-1-index-vector  : container-eval=") (display container-eval) (newline)

  (when (< index-eval 0) ;; deal with negative index
    (<- index-eval (+ (vector-length container-eval) index-eval)))
  (vector-set! container-eval index-eval expr-eval))


(define (assignment-argument-1-vector container-eval index-eval expr-eval)

  ;;(display "assignment.rkt : assignment-argument-1-vector") (newline)
  
  (if (eq? index-eval slice)  ;; T[:]
      
      (assignment-argument-1-slice-vector container-eval expr-eval)

      ;; normal case
      ;; {T[3] <- T[7 2 4]}
      
      (assignment-argument-1-index-vector container-eval index-eval expr-eval)))



(define (assignment-argument-2-vector container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval expr-is-vector?)

  (<+ index1-or-keyword-eval-pos  index1-or-keyword-eval) ;; pos for positive
  (<+ index2-or-keyword-eval-pos  index2-or-keyword-eval)

  (declare container-length container-copy! expr-length
	   copy-slice-with-positive-step copy-slice-starting-at-zero-with-positive-step)


  (<- container-length vector-length)
  (<- container-copy! vector-copy!) ;; unfortunately vector-copy! does not support step

  ;; expr-is-vector? is known at macro-expansion time (see the <- macro : the RHS syntax
  ;; was itself $bracket-apply-vector$), no runtime vector? check needed here to pick the
  ;; fast vector-copy!-based helpers below
  (if expr-is-vector?
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-vector-vector)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-vector-vector))
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-vector)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-vector)))

  (if (vector? expr-eval) ; still dependent on expression (will be specialized later when dealing with <-)
      (<- expr-length vector-length)
      (<- expr-length string-length))


  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)

  ;; > (require srfi/25)
  ;; > (define a (make-array (shape 0 5 0 3) 0))
  ;; > {a[1 2]}
  ;; 0
  ;; > {a[1 2] <- 7}
  ;; > {a[1 2]}
  ;; 7

  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos)

	 ;;  {a <+ (make-vector 7 0)}
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: :] <- #(1 2 3)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)

	 ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	 ;; v[: :]= 'abcd'
	 ;; v
	 ;; ['a', 'b', 'c', 'd']

	 ;; > {a <+ (make-vector 7 0)}
	 ;; > {a[: :] <- "abcd"}
	 ;; > a
	 ;; '#(#\a #\b #\c #\d 0 0 0)

	 (((? slice?) (? slice?))
	  ;;  make it work between vector and string
	  (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                 expr-eval
                                                                 (expr-length expr-eval)
                                                                 1))

	  ;; (container-copy! container-eval
	  ;; 		   0
	  ;; 		   expr-eval))

	 ;;  {s <+ (string-append "abcdefgh")}
	 ;; s
	 ;; "abcdefgh"
	 ;; > {s[3 :] <- "zob"}
	 ;; > s
	 ;; "abczobgh"
	 ;; >

	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[3 :] <- "zob"}
	 ;; > a
	 ;; '#(0 0 0 #\z #\o #\b 0)
	 ((i1 (? slice?))
	  ;; make it work between vector and string
	  (copy-slice-with-positive-step container-eval
                                                expr-eval
                                                i1
                                                (+ i1 (expr-length expr-eval))
                                                1))

	  ;; (container-copy! container-eval
	  ;; 		   i1
	  ;; 		   expr-eval))


	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: 3] <- (vector 1 2 3 4 5)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)
	 (((? slice?) i2)
	  ;; make it work between vector and string
	  (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                 expr-eval
                                                                 i2
                                                                 1))
	 ;; (container-copy! container-eval
	 ;; 		   0
	 ;; 		   expr-eval
	 ;; 		   0
	 ;; 		   i2))

	 ;; {a[1 2] <- 7}
	 ((i1 i2)
          ;; i2 is an index of the row M[i1] : its negative value counts from the end of the row (done by
          ;; function-array-n-dim-set!) , not of the vector of rows (i2 was made positive with the length of container-eval)
          #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2)))
          (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval))))
	
	 ) ;; end match

  )

(define (assignment-argument-3-vector container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval expr-is-vector?)

  ;(display "assignment-argument-3-vector : expr-is-vector? =") (display expr-is-vector?) (newline)

  (<+ index1-or-keyword-eval-pos index1-or-keyword-eval)
  (<+ index2-or-keyword-eval-pos index2-or-keyword-eval)
  (<+ index3-or-keyword-or-step-eval-pos  index3-or-keyword-or-step-eval)

  (declare container-length container-copy! expr-length
	   copy-slice-with-positive-step copy-slice-starting-at-zero-with-positive-step)

  (<- container-length vector-length)
  (<- container-copy! vector-copy!)

  ;; expr-is-vector? comes from the <- macro (RHS syntactically $bracket-apply-vector$) ;
  ;; no runtime vector? check needed to pick the fast vector-copy!-based helpers below,
  ;; which themselves only fall back to the element-by-element loop when step ≠ 1
  (if expr-is-vector?
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-vector-vector)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-vector-vector))
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-vector)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-vector)))

  (if (vector? expr-eval)
      (<- expr-length vector-length)
      (<- expr-length string-length))


  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-or-step-eval-pos container-length container-eval)

  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-or-step-eval-pos)


	 ;; T[: i2 :]
	 ;;  make it work between vector and string
	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: 3 :] <- (vector 1 2 3 4 5)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)
	 (((? slice?) i2 (? slice?))  (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                                                  expr-eval
                                                                                                  i2
                                                                                                  1))


	 ;; T[i1 : :]
	 ;; make it work between vector and string
	 ;; > {a <+ (make-vector 7 0)}
	 ;; > {a[3 : :] <- "zob"}
	 ;; > a
	 ;; '#(0 0 0 #\z #\o #\b 0)
	 ((i1 (? slice?) (? slice?))  (copy-slice-with-positive-step container-eval
                                                                                 expr-eval
                                                                                 i1
                                                                                 (+ i1 (expr-length expr-eval))
                                                                                 1))


	 
	 ;; T[: : s3]
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; '#(1 2 3 4 5 6 7 8 9)
	 ;; > {v[: : 2] <- (vector -1 -2 -3 -4 -5)}
	 ;;> v
	 ;;'#(-1 2 -2 4 -3 6 -4 8 -5)


	
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; >  {v[: : -2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(-5 2 -4 4 -3 6 -2 8 -1)

	 ;; Python:
	 ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	 ;; v[: : -2] = [-1, -2, -3, -4, -5]
	 ;; v
	 ;; [-5, 2, -4, 4, -3, 6, -2, 8, -1]

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: : -2] <- "abcde"}
	 ;; > v
	 ;; '#(#\e 2 #\d 4 #\c 6 #\b 8 #\a)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: : -2] <- (vector -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13)[: : 2]}
	 ;; > v
	 ;; '#(-9 2 -7 4 -5 6 -3 8 -1)

	 ;;> {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: : -2] <- "abcdefghijklmnop"[: : 2]}
	 ;; > v
	 ;; '#(#\i 2 #\g 4 #\e 6 #\c 8 #\a)
	 (((? slice?) (? slice?) step-not-used)
	  
          (begin-def
            (define size-input (vector-length container-eval))
            (check-step index3-or-keyword-or-step-eval)
            
            (if (< index3-or-keyword-or-step-eval 0) ;; with negative step we start at end of vector (like in Python)
                ;; the generic method allows compatibility between vectors and strings but is less fast
                (copy-slice-starting-at-zero-with-negative-step-vector container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
                (copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))))

	
	 


	 ;; T[i1 : i3]
	 ;; {s <+ (string-append "abcdefgh")}
	 ;; s
	 ;; "abcdefgh"
	 ;; > {s[2 : 4] <- "zob"}
	 ;; > s
	 ;; "abzoefgh"
	 ;; > {s[2 : 4] <- "zo"}
	 ;; > s
	 ;; "abzoefgh"


	 ;; > {v <+ (vector 1 2 3 4)}
	 ;; > {v[1 : 3] <- "abcdef"[2 : 4]}
	 ;; > v
	 ;; '#(1 #\c #\d 4)
	 
	 ;;  make it work between vector and string
	 ((i1 (? slice?) i3) (copy-slice-with-positive-step container-eval
                                                                        expr-eval
                                                                        i1
                                                                        i3
                                                                        1))



	 ;; T[i1 i2 i3]
	 ((i1 i2 i3) 
	  
	  ;; normal case
          ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
          ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
          #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3)))
          (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval index3-or-keyword-or-step-eval)))) ;;(array-n-dim-set! array value i1 i2)
	  	 
	 ) ;; end match

  )


(define (assignment-argument-4-vector container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval expr-is-vector?)

  (<+ index1-or-keyword-eval-pos index1-or-keyword-eval)
  (<+ index2-or-keyword-eval-pos index2-or-keyword-eval)
  (<+ index3-or-keyword-eval-pos index3-or-keyword-eval)
  (<+ index4-or-step-eval-pos  index4-or-step-eval)

  (declare container-length copy-slice-with-positive-step copy-slice-starting-at-zero-with-positive-step)

  (<- container-length  vector-length)

  ;; expr-is-vector? comes from the <- macro (RHS syntactically $bracket-apply-vector$) ;
  ;; no runtime vector? check needed to pick the fast vector-copy!-based helpers below,
  ;; which themselves only fall back to the element-by-element loop when step ≠ 1
  (<- copy-slice-with-positive-step
      (if expr-is-vector?
          copy-slice-with-positive-step-vector-vector
          copy-slice-with-positive-step-vector))
  (<- copy-slice-starting-at-zero-with-positive-step
      (if expr-is-vector?
          copy-slice-starting-at-zero-with-positive-step-vector-vector
          copy-slice-starting-at-zero-with-positive-step-vector))

  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-step-eval-pos container-length container-eval)


  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-eval-pos index4-or-step-eval-pos)

	 ;; T[i1 : i2 :]  i1 start , i2 end (redundant trailing slice, step is implicitly 1)
	 ((i1 (? slice?) i2 (? slice?)) (assignment-argument-3-vector container-eval i1 slice i2 expr-eval expr-is-vector?))

	 ;; T[: i2 : s3]
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; '#(1 2 3 4 5 6 7 8 9)
	 ;; > {v[: 6 : 2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(-1 2 -2 4 -3 6 7 8 9)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: 6 : -2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(1 2 3 4 5 6 7 8 -1)
	 (((? slice?) i2 (? slice?) step-not-used)

	  (<+ step index4-or-step-eval)

	  (check-step step)

	  ;;(display "assignment : assignment-argument-4-vector ") (newline)

	  (if (< step 0) ;; with negative index we start at end of vector (like in Python)

	      (copy-slice-with-negative-step-vector container-eval
                                                    expr-eval
                                                    (- (container-length container-eval) 1)
                                                    i2
                                                    step)


	      (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                     expr-eval
                                                                     i2
                                                                     step)))

	 	 
	 ;; T[i1 : : s3]
	 ((i1 (? slice?) (? slice?) step-not-used)

          (<+ step index4-or-step-eval)

	  ;; > {v <+ (vector 1 2 3 4 5 6 7 8)}
	  ;; > {v[3 : : 2] <- (vector -1 -2 -3 -4)}
	  ;; > v
	  ;; '#(1 2 3 -1 5 -2 7 -3)
	  
	  ;; > {v <+ (vector 1 2 3 4 5 6 7 8)}
	  ;; > {v[3 : : 2] <- (vector -1 -2 -3)}
	  ;; > v
	  ;; '#(1 2 3 -1 5 -2 7 -3)

	  ;; > {v <+ (vector 1 2 3 4 5 6 7 8)}
	  ;; > {v[5 : : -2] <- (vector -1 -2 -3)}
	  ;; > v
	  ;; '#(1 -3 3 -2 5 -1 7 8)

	  ;; Python check:
	  ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	  ;; v[5:  : -2] = [-1, -2,-3 ]
	  ;; v
	  ;; [1, -3, 3, -2, 5, -1, 7, 8, 9]
	  (check-step step)
		    
	  (if (< step 0) ;; with negative index we start at end of vector (like in Python)
	      (copy-slice-starting-at-zero-with-negative-step-vector container-eval expr-eval i1 step)
	
	      ($+>
	       (<+ size-input (container-length container-eval))
	       (copy-slice-with-positive-step container-eval expr-eval i1 size-input step))))

	 ;; T[i1 i2 i3 i4]
	 ((i1 i2 i3 i4) 
	  
	  ;; normal case
          ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
          ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
          #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4)))
          (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval)))) ;;(array-n-dim-set! array value i1 i2)
	 
	 ) ;; end match
  
  ) ; end define


(define (assignment-argument-5-vector container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval expr-is-vector?)

  ;; (display "assignment-argument-5-vector") (newline)
  ;; (display "container-eval=") (display container-eval) (newline)
  ;; (display "index1-eval=") (display index1-eval) (newline)
  ;; (display "index2-or-keyword-eval=") (display index2-or-keyword-eval) (newline)
  ;; (display "index3-eval=") (display index3-eval) (newline)
  ;; (display "index4-or-keyword-eval=") (display index4-or-keyword-eval) (newline)
  ;; (display "index5-or-step-eval=") (display index5-or-step-eval) (newline)
  ;; (display "expr-eval=") (display expr-eval) (newline)


  (<+ index1-eval-pos  index1-eval)
  (<+ index2-or-keyword-eval-pos  index2-or-keyword-eval)
  (<+ index3-eval-pos  index3-eval)
  (<+ index4-or-keyword-eval-pos  index4-or-keyword-eval)
  (<+ index5-or-step-eval-pos  index5-or-step-eval)

  (declare container-length copy-stepped-slice)

  (<- container-length  vector-length)

  ;; expr-is-vector? comes from the <- macro (RHS syntactically $bracket-apply-vector$) ;
  ;; no runtime vector? check needed to pick the fast vector-copy!-based helper below,
  ;; which itself only falls back to the element-by-element loop for a negative or ≠ 1 step
  (<- copy-stepped-slice
      (if expr-is-vector?
          copy-stepped-slice-vector-vector
          copy-stepped-slice-vector))
    
  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index5-or-step-eval-pos container-length container-eval)
  
  
  (match (list index1-eval-pos index2-or-keyword-eval-pos index3-eval-pos index4-or-keyword-eval-pos index5-or-step-eval-pos)


	 ;; from Python:
	 ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	 ;; v[7: 2 : -2] = [-1, -2, -3, -4, -5]
	 ;; Traceback (most recent call last):
	 ;;   File "<pyshell#26>", line 1, in <module>
	 ;;     v[7: 2 : -2] = [-1, -2, -3, -4, -5]
	 ;; ValueError: attempt to assign sequence of size 5 to extended slice of size 3
	 ;; v[7: 2 : -2] = [-1, -2, -3]
	 ;; v
	 ;; [1, 2, 3, -3, 5, -2, 7, -1, 9]
	 
	 ;; > {v <- (vector 1 2 3 4 5 6 7 8 9)}
	 ;; >  {v[7 : 2 : -2] <- (vector -1 -2 -3)}
	 ;; > v
	 ;; '#(1 2 3 -3 5 -2 7 -1 9)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[7 : 3 : -2] <- (vector -1 -2 -3)}
	 ;; > v
	 ;; '#(1 2 3 4 5 -2 7 -1 9)
	 ((i1 (? slice?) i2 (? slice?) step-not-used)
	  
	  (<+ step index5-or-step-eval)

	  ;; (display "container-eval=") (display container-eval) (newline)
	  ;; (display "i1=") (display i1) (newline)
	  ;; (display "i2=") (display i2) (newline)
	  ;; (display "step=") (display step) (newline)
	  ;; (display "expr-eval=") (display expr-eval) (newline)
	  
	  (copy-stepped-slice container-eval expr-eval i1 i2 step))
	 
	 
	 ;; T[i1 i2 i3 i4 i5]
	 ((i1 i2 i3 i4 i5) 
	  
	  ;; normal case
          ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
          ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
          #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4 i5)))
          (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval)))) ;;(array-n-dim-set! array expr-eval i1 i2)
	  
	 ) ;; end match
  
  ) ; end define



(define (assignment-argument-6-and-more-vector container expr args)

  (function-array-n-dim-set! container expr (reverse args))) ;; (array-n-dim-set! array value index1 index2 ...)



;; string procedures, built on the model of the vector ones

(define (copy-slice-with-negative-step-string container-eval expr-eval i1 i2 step)
  (for (($> (<+ k i1) (<+ i 0)) (> k i2) (<- k (+ k step)))
       (assignment-argument-1-index-string container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

(define (copy-slice-starting-at-zero-with-negative-step-string container-eval expr-eval i1 step)
  (for (($> (<+ k i1) (<+ i 0)) (>= k 0) (<- k (+ k step)))
       (assignment-argument-1-index-string container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

(define (copy-slice-with-positive-step-string container-eval expr-eval i1 i2 step) ; container string
  (for (($> (<+ k i1) (<+ i 0)) (< k i2) (<- k (+ k step)))
       (assignment-argument-1-index-string container-eval k (apply-square-brackets-argument-1 expr-eval i))
       (<- i (+ i 1))))

;; fast path used only when expr is statically known (via $bracket-apply-string$ syntax
;; in the <- macro) to be a string : string-copy! is a bulk memory copy, much faster than
;; the element-by-element loop above, but it does not support a step different than 1
(define (copy-slice-with-positive-step-string-string container-eval expr-eval i1 i2 step)
  (if (= step 1)
      (string-copy! container-eval i1 expr-eval 0 (- i2 i1))
      (copy-slice-with-positive-step-string container-eval expr-eval i1 i2 step)))

(define (copy-slice-starting-at-zero-with-positive-step-string container-eval expr-eval i2 step) ; container string
  (for (($> (<+ k 0) (<+ i 0)) (< k i2) (<- k (+ k step)))
        (<+ bkt (apply-square-brackets-argument-1 expr-eval i))
        (assignment-argument-1-index-string container-eval k bkt)
        (<- i (+ i 1))))

;; fast path : see copy-slice-with-positive-step-string-string
(define (copy-slice-starting-at-zero-with-positive-step-string-string container-eval expr-eval i2 step)
  (if (= step 1)
      (string-copy! container-eval 0 expr-eval 0 i2)
      (copy-slice-starting-at-zero-with-positive-step-string container-eval expr-eval i2 step)))

(define (copy-stepped-slice-string container-eval expr-eval i1 i2 step)

  (check-step step)

  (if (< step 0) ;; with negative step we start at end of string (like in Python)

      (copy-slice-with-negative-step-string container-eval expr-eval i1 i2 step)

      (copy-slice-with-positive-step-string container-eval expr-eval i1 i2 step)))

;; fast path : see copy-slice-with-positive-step-string-string ; there is no fast path for
;; a negative step (string-copy! only copies forward), so that branch is unchanged
(define (copy-stepped-slice-string-string container-eval expr-eval i1 i2 step)

  (check-step step)

  (if (< step 0)

      (copy-slice-with-negative-step-string container-eval expr-eval i1 i2 step)

      (copy-slice-with-positive-step-string-string container-eval expr-eval i1 i2 step)))


;; T[:]
(define (assignment-argument-1-slice-string container-eval expr-eval)

  (string-copy! container-eval
                0
                expr-eval))


;; {S[3] <- #\c}
(define (assignment-argument-1-index-string container-eval index-eval expr-eval)

  (when (< index-eval 0) ;; deal with negative index
        (<- index-eval (+ (string-length container-eval) index-eval)))
  (string-set! container-eval index-eval expr-eval))


(define (assignment-argument-1-string container-eval index-eval expr-eval)

  (if (eq? index-eval slice)  ;; T[:]

      (assignment-argument-1-slice-string container-eval expr-eval)

      ;; normal case
      ;; {S[3] <- #\c}

      (assignment-argument-1-index-string container-eval index-eval expr-eval)))


(define (assignment-argument-2-string container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval expr-is-string?)

  (<+ index1-or-keyword-eval-pos  index1-or-keyword-eval) ;; pos for positive
  (<+ index2-or-keyword-eval-pos  index2-or-keyword-eval)

  (declare container-length expr-length
	   copy-slice-with-positive-step copy-slice-starting-at-zero-with-positive-step)

  (<- container-length string-length)

  ;; expr-is-string? is known at macro-expansion time (see the <- macro : the RHS syntax
  ;; was itself $bracket-apply-string$), no runtime string? check needed here to pick the
  ;; fast string-copy!-based helpers below
  (if expr-is-string?
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-string-string)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-string-string))
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-string)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-string)))

  (if (vector? expr-eval)
      (<- expr-length vector-length)
      (<- expr-length string-length))


  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)

  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos)

	 ;; > {s <+ (make-string 7 #\0)}
	 ;; > {s[: :] <- "abc"}
	 ;; > s
	 ;; "abc0000"
	 (((? slice?) (? slice?))
	  (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                  expr-eval
                                                                  (expr-length expr-eval)
                                                                  1))

	 ;;  {s <+ (string-append "abcdefgh")}
	 ;; > {s[3 :] <- "zob"}
	 ;; > s
	 ;; "abczobgh"
	 ((i1 (? slice?))
	  (copy-slice-with-positive-step container-eval
                                                expr-eval
                                                i1
                                                (+ i1 (expr-length expr-eval))
                                                1))

	 ;; > {s <+ (make-string 7 #\0)}
	 ;; > {s[: 3] <- "abc"}
	 ;; > s
	 ;; "abc0000"
	 (((? slice?) i2)
	  (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                  expr-eval
                                                                  i2
                                                                  1))

	 ;; T[i1 i2] : not valid for strings (1-dimensional)
	 ((i1 i2)
	  (error "assignment-argument-2-string : bad arguments in string case,expecting : i2 or i1 :, provided :"
		 index1-or-keyword-eval index2-or-keyword-eval))

	 ) ;; end match

  )


(define (assignment-argument-3-string container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval expr-is-string?)

  (<+ index1-or-keyword-eval-pos index1-or-keyword-eval)
  (<+ index2-or-keyword-eval-pos index2-or-keyword-eval)
  (<+ index3-or-keyword-or-step-eval-pos  index3-or-keyword-or-step-eval)

  (declare container-length expr-length
	   copy-slice-with-positive-step copy-slice-starting-at-zero-with-positive-step)

  (<- container-length string-length)

  ;; expr-is-string? comes from the <- macro (RHS syntactically $bracket-apply-string$) ;
  ;; no runtime string? check needed to pick the fast string-copy!-based helpers below,
  ;; which themselves only fall back to the element-by-element loop when step ≠ 1
  (if expr-is-string?
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-string-string)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-string-string))
      ($>
       (<- copy-slice-with-positive-step copy-slice-with-positive-step-string)
       (<- copy-slice-starting-at-zero-with-positive-step copy-slice-starting-at-zero-with-positive-step-string)))

  (if (vector? expr-eval)
      (<- expr-length vector-length)
      (<- expr-length string-length))


  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-or-step-eval-pos container-length container-eval)

  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-or-step-eval-pos)


	 ;; T[: i2 :]
	 ;; > {s <+ (make-string 7 #\0)}
	 ;; > {s[: 3 :] <- "abc"}
	 ;; > s
	 ;; "abc0000"
	 (((? slice?) i2 (? slice?))  (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                                                    expr-eval
                                                                                                    i2
                                                                                                    1))

	 ;; T[i1 : :]
	 ;; > {s <+ (make-string 7 #\0)}
	 ;; > {s[3 : :] <- "zob"}
	 ;; > s
	 ;; "000zob0"
	 ((i1 (? slice?) (? slice?))  (copy-slice-with-positive-step container-eval
                                                                                   expr-eval
                                                                                   i1
                                                                                   (+ i1 (expr-length expr-eval))
                                                                                   1))

	 ;; T[: : s3]
	 ;; > {s <+ (string-append "abcdefgh")}
	 ;; > {s[: : 2] <- "ABCD"}
	 ;; > s
	 ;; "AbBdCfDh"
	 (((? slice?) (? slice?) step-not-used)

          (begin-def
            (define size-input (string-length container-eval))
            (check-step index3-or-keyword-or-step-eval)

            (if (< index3-or-keyword-or-step-eval 0) ;; with negative step we start at end of string (like in Python)
                (copy-slice-starting-at-zero-with-negative-step-string container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
                (copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))))


	 ;; T[i1 : i3]
	 ;; {s <+ (string-append "abcdefgh")}
	 ;; > {s[2 : 4] <- "zob"}
	 ;; > s
	 ;; "abzoefgh"
	 ((i1 (? slice?) i3) (copy-slice-with-positive-step container-eval
                                                                        expr-eval
                                                                        i1
                                                                        i3
                                                                        1))


	 ;; T[i1 i2 i3] : not valid for strings
	 ((i1 i2 i3)
	  (error "assignment-argument-3-string : in string case, provided too much arguments:"
		 index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval))

	 ) ;; end match

  ) ;; end define


(define (assignment-argument-4-string container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval expr-is-string?)

  (<+ index1-or-keyword-eval-pos index1-or-keyword-eval)
  (<+ index2-or-keyword-eval-pos index2-or-keyword-eval)
  (<+ index3-or-keyword-eval-pos index3-or-keyword-eval)
  (<+ index4-or-step-eval-pos  index4-or-step-eval)

  (declare container-length copy-slice-with-positive-step copy-slice-starting-at-zero-with-positive-step)

  (<- container-length  string-length)

  ;; expr-is-string? comes from the <- macro (RHS syntactically $bracket-apply-string$) ;
  ;; no runtime string? check needed to pick the fast string-copy!-based helpers below,
  ;; which themselves only fall back to the element-by-element loop when step ≠ 1
  (<- copy-slice-with-positive-step
      (if expr-is-string?
          copy-slice-with-positive-step-string-string
          copy-slice-with-positive-step-string))
  (<- copy-slice-starting-at-zero-with-positive-step
      (if expr-is-string?
          copy-slice-starting-at-zero-with-positive-step-string-string
          copy-slice-starting-at-zero-with-positive-step-string))

  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-step-eval-pos container-length container-eval)


  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-eval-pos index4-or-step-eval-pos)

	 ;; T[i1 : i2 :]  i1 start , i2 end
	 ((i1 (? slice?) i2 (? slice?)) (assignment-argument-3-string container-eval i1 slice i2 expr-eval expr-is-string?))

	 ;; T[: i2 : s3]
	 ;; > {s <+ (string-append "abcdefgh")}
	 ;; > {s[: 6 : 2] <- "ABCD"}
	 ;; > s
	 ;; "AbBdCfgh"
	 (((? slice?) i2 (? slice?) step-not-used)

	  (<+ step index4-or-step-eval)

	  (check-step step)

	  (if (< step 0) ;; with negative index we start at end of string (like in Python)

	      (copy-slice-with-negative-step-string container-eval
                                                    expr-eval
                                                    (- (container-length container-eval) 1)
                                                    i2
                                                    step)

	      (copy-slice-starting-at-zero-with-positive-step container-eval
                                                                     expr-eval
                                                                     i2
                                                                     step)))


	 ;; T[i1 : : s3]
	 ;; > {s <+ (string-append "abcdefgh")}
	 ;; > {s[3 : : 2] <- "0000"}
	 ;; > s
	 ;; "abc0e0g0"
	 ((i1 (? slice?) (? slice?) step-not-used)

          (<+ step index4-or-step-eval)

	  (check-step step)

	  (if (< step 0) ;; with negative index we start at end of string (like in Python)
	      (copy-slice-starting-at-zero-with-negative-step-string container-eval expr-eval i1 step)

	      ($+>
	       (<+ size-input (container-length container-eval))
	       (copy-slice-with-positive-step container-eval expr-eval i1 size-input step))))

	 ;; T[i1 i2 i3 i4] : not valid for strings
	 ((i1 i2 i3 i4)
	  (error "assignment-argument-4-string : in string case, provided too much arguments:"
		 index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval))

	 ) ;; end match

  ) ; end define


(define (assignment-argument-5-string container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval expr-is-string?)

  (<+ index1-eval-pos  index1-eval)
  (<+ index2-or-keyword-eval-pos  index2-or-keyword-eval)
  (<+ index3-eval-pos  index3-eval)
  (<+ index4-or-keyword-eval-pos  index4-or-keyword-eval)
  (<+ index5-or-step-eval-pos  index5-or-step-eval)

  (declare container-length copy-stepped-slice)

  (<- container-length  string-length)

  ;; expr-is-string? comes from the <- macro (RHS syntactically $bracket-apply-string$) ;
  ;; no runtime string? check needed to pick the fast string-copy!-based helper below,
  ;; which itself only falls back to the element-by-element loop for a negative or ≠ 1 step
  (<- copy-stepped-slice
      (if expr-is-string?
          copy-stepped-slice-string-string
          copy-stepped-slice-string))

  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index5-or-step-eval-pos container-length container-eval)


  (match (list index1-eval-pos index2-or-keyword-eval-pos index3-eval-pos index4-or-keyword-eval-pos index5-or-step-eval-pos)

	 ;; T[i1 : i2 : step]
	 ;; > {s <+ (string-append "abcdefgh")}
	 ;; > {s[2 : 7 : 2] <- "1234"}
	 ;; > s
	 ;; "ab1d2f3h"
	 ((i1 (? slice?) i2 (? slice?) step-not-used)

	  (<+ step index5-or-step-eval)

	  (copy-stepped-slice container-eval expr-eval i1 i2 step))

	 ;; T[i1 i2 i3 i4 i5] : not valid for strings
	 ((i1 i2 i3 i4 i5)
	  (error "assignment-argument-5-string : in string case, provided too much arguments:"
		 index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval))

	 ) ;; end match

  )


(define (assignment-argument-6-and-more-string container expr args)

  (error "assignment-argument-6-and-more-string : string used with [ ] and more than 5 arguments"))



;; hash table procedures, built on the model of the string/vector ones

;; {H[:]} is not permitted for hash tables
(define (assignment-argument-1-slice-hash-table container-eval expr-eval)

  (error "assignment-argument-1-slice-hash-table : slicing not permitted with hash table"))


;; {H[key] <- value}
(define (assignment-argument-1-index-hash-table container-eval index-eval expr-eval)

  (hash-table-set! container-eval index-eval expr-eval))


(define (assignment-argument-1-hash-table container-eval index-eval expr-eval)

  (if (eq? index-eval slice)  ;; H[:]

      (assignment-argument-1-slice-hash-table container-eval expr-eval)

      ;; normal case
      ;; {H[key] <- value}

      (assignment-argument-1-index-hash-table container-eval index-eval expr-eval)))


(define (assignment-argument-2-hash-table container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)

  (error "assignment-argument-2-hash-table : hash table used with [ ] and more than 1 argument"))


(define (assignment-argument-3-hash-table container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)

  (error "assignment-argument-3-hash-table : hash table used with [ ] and more than 1 argument"))


(define (assignment-argument-4-hash-table container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval)

  (error "assignment-argument-4-hash-table : hash table used with [ ] and more than 1 argument"))


(define (assignment-argument-5-hash-table container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval)

  (error "assignment-argument-5-hash-table : hash table used with [ ] and more than 1 argument"))


(define (assignment-argument-6-and-more-hash-table container expr args)

  (error "assignment-argument-6-and-more-hash-table : hash table used with [ ] and more than 1 argument"))



;; array procedures, built on the model of the vector/hash-table ones (SRFI 25)
;; note : i do not use negative indexes or slices for array because they could be not starting at zero

;; {A[i1] <- v} ,  1 dimension array
(define (assignment-argument-1-array container-eval index-eval expr-eval)

  (array-set! container-eval index-eval expr-eval))


;; {A[i1 i2] <- v} ,  2 dimension array
(define (assignment-argument-2-array container-eval index1-eval index2-eval expr-eval)

  (array-set! container-eval index1-eval index2-eval expr-eval))


;; {A[i1 i2 i3] <- v} ,  3 dimension array
(define (assignment-argument-3-array container-eval index1-eval index2-eval index3-eval expr-eval)

  (array-set! container-eval index1-eval index2-eval index3-eval expr-eval))


;; {A[i1 i2 i3 i4] <- v} ,  4 dimension array
(define (assignment-argument-4-array container-eval index1-eval index2-eval index3-eval index4-eval expr-eval)

  (array-set! container-eval index1-eval index2-eval index3-eval index4-eval expr-eval))


;; {A[i1 i2 i3 i4 i5] <- v} ,  5 dimension array
(define (assignment-argument-5-array container-eval index1-eval index2-eval index3-eval index4-eval index5-eval expr-eval)

  (array-set! container-eval index1-eval index2-eval index3-eval index4-eval index5-eval expr-eval))


;; {A[i1 i2 i3 i4 i5 i6 ...] <- v} , n dimension array
;; note: called as (assignment-argument-6-and-more-array container parsed-args expr) from the <- macro,
;; so the 2nd parameter is the list of indices and the 3rd is the value to assign.
(define (assignment-argument-6-and-more-array container args expr)

  (array-set! container (list->vector args) expr))




;; functions and macro based on number of arguments in [ ]

;;  0 argument case (i do not know if it is a good idea, will see later possible conflict with other special syntax)
;; (define (assignment-argument-0 container-eval expr-eval)
;;   ;; (display "assignment-argument-0 : container-eval =")
;;   ;; (display container-eval)
;;   ;; (newline)
;;   (<- container-eval expr-eval))

;; > {v <+ (vector 1 2 3 4)}
;; > {v[] <- #(1 2 5)}
;; > v
;; '#(1 2 5)
(define-syntax assignment-argument-0
  (syntax-rules ()

    ((_ container expr)
     ;; (display "assignment-argument-0 : container-eval =") ;; note: no more eval as a macro now (to be renamed if used)
     ;; (display container-eval)
     ;; (newline)
     (<- container expr))))


(define (assignment-argument-1 container-eval index-eval expr-eval)

  ;;(display "assignment.sls : assignment-argument-1") (newline)
  
  (if (eq? index-eval slice)  ;; T[:]
      
      (assignment-argument-1-slice container-eval expr-eval)

      ;; normal case
      ;; {T[3] <- T[7 2 4]}
      
      (assignment-argument-1-index container-eval index-eval expr-eval)))

      
      


(define (assignment-argument-2 container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)
  
  (<+ index1-or-keyword-eval-pos  index1-or-keyword-eval) ;; pos for positive
  (<+ index2-or-keyword-eval-pos  index2-or-keyword-eval)

  (declare container-length container-copy! expr-length)
  
  (if (vector? container-eval)
      ($>
       (<- container-length vector-length)
       (<- container-copy! vector-copy!)) ;; unfortunately vector-copy! does not support step
      ($>  ;; a string
       (<- container-length string-length)
       (<- container-copy! string-copy!)))

  (if (vector? expr-eval)
      (<- expr-length vector-length)
      (<- expr-length string-length))
      
  
  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)

  (when (hash-table? container-eval)
    (error "assignment forbiden with hash table with 2 arguments : hash table =" container-eval))    
  
  ;; > (require srfi/25)
  ;; > (define a (make-array (shape 0 5 0 3) 0))
  ;; > {a[1 2]}
  ;; 0
  ;; > {a[1 2] <- 7}
  ;; > {a[1 2]}
  ;; 7
  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos)
	 
	 ;;  {a <+ (make-vector 7 0)}
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: :] <- #(1 2 3)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)

	 ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	 ;; v[: :]= 'abcd'
	 ;; v
	 ;; ['a', 'b', 'c', 'd']

	 ;; > {a <+ (make-vector 7 0)}
	 ;; > {a[: :] <- "abcd"}
	 ;; > a
	 ;; '#(#\a #\b #\c #\d 0 0 0)

	 (((? slice?) (? slice?))
	  ;;  make it work between vector and string
	  (copy-slice-starting-at-zero-with-positive-step container-eval
							  expr-eval
							  (expr-length expr-eval)
							  1))
	  
	  ;; (container-copy! container-eval
	  ;; 		   0
	  ;; 		   expr-eval))
	 
	 ;;  {s <+ (string-append "abcdefgh")}
	 ;; s
	 ;; "abcdefgh"
	 ;; > {s[3 :] <- "zob"}
	 ;; > s
	 ;; "abczobgh"
	 ;; >

	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[3 :] <- "zob"}
	 ;; > a
	 ;; '#(0 0 0 #\z #\o #\b 0)
	 ((i1 (? slice?))
	  ;; make it work between vector and string
	  (copy-slice-with-positive-step container-eval
					 expr-eval
					 i1
					 (+ i1 (expr-length expr-eval))
					 1))
	 
	  ;; (container-copy! container-eval
	  ;; 		   i1
	  ;; 		   expr-eval))


	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: 3] <- (vector 1 2 3 4 5)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)
	 (((? slice?) i2)
	  ;; make it work between vector and string
	  (copy-slice-starting-at-zero-with-positive-step container-eval
							  expr-eval
							  i2
							  1))
	 ;; (container-copy! container-eval
	 ;; 		   0
	 ;; 		   expr-eval
	 ;; 		   0
	 ;; 		   i2))

	 ;; {a[1 2] <- 7}
	 ((i1 i2)
	  (cond ((vector? container-eval)  ;; normal case
		 ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
		 ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
		 #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2)))
		 (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval))))
		((array? container-eval)
		 ;;(display "assignment.* : 2 args ,array case : container-eval = ") (display container-eval) (newline)
		 (array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval))
		
		#;((flomat? container-eval) ; flomat
		 (mset! container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval))

		(else ;; overloaded
		 (begin-def
		  (define args-lst (list container-eval i1 i2))
		  (define setter! (find-setter-for-overloaded-square-brackets args-lst))
		  (setter! container-eval i1 i2 expr-eval))))) 

	 ) ;; end match

  )



;; flomat procedures, built on the model of the hash-table/array ones (removed : sci requires libgfortran)
;; note : flomat only supports element (row,col) assignment ; row setting, slicing,
;; and more than 2 indices are not supported (matches apply-square-brackets-argument-*-flomat)

;; {M[i] <- v} : not supported
#;(define (assignment-argument-1-flomat container-eval index-eval expr-eval)

  (error "assignment-argument-1-flomat : row setting not allowed with flomat"))


;; {M[i j] <- v}
#;(define (assignment-argument-2-flomat container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)

  (if (or (eq? slice index1-or-keyword-eval)
	  (eq? slice index2-or-keyword-eval))
      (error "assignment-argument-2-flomat : slice not allowed with flomat")
      ;; {a[1 2] <- 7}
      (mset! container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)))


#;(define (assignment-argument-3-flomat container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)

  (error "assignment-argument-3-flomat : flomat used with 3 arguments"))


#;(define (assignment-argument-4-flomat container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval)

  (error "assignment-argument-4-flomat : flomat used with 4 arguments"))


#;(define (assignment-argument-5-flomat container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval)

  (error "assignment-argument-5-flomat : flomat used with 5 arguments"))


;; note: called as (assignment-argument-6-and-more-flomat container parsed-args expr) from the <- macro,
;; so the 2nd parameter is the list of indices and the 3rd is the value to assign.
#;(define (assignment-argument-6-and-more-flomat container args expr)

  (error "assignment-argument-6-and-more-flomat : flomat used with more than 5 arguments"))







(define (assignment-argument-3 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)

  
  (<+ index1-or-keyword-eval-pos index1-or-keyword-eval)
  (<+ index2-or-keyword-eval-pos index2-or-keyword-eval)
  (<+ index3-or-keyword-or-step-eval-pos  index3-or-keyword-or-step-eval)

  (declare container-length container-copy! expr-length)

  (when (not (or (vector? container-eval)  (string? container-eval)  (array? container-eval)))
	     (error "assignment-argument-3: container type not compatible : " container-eval))
 
  
  (if (vector? container-eval)
      ($>
       (<- container-length vector-length)
       (<- container-copy! vector-copy!))
      ($>  ;; a string
       (<- container-length string-length)
       (<- container-copy! string-copy!)))


  (if (vector? expr-eval)
      (<- expr-length vector-length)
      (<- expr-length string-length))
      

  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-or-step-eval-pos container-length container-eval)

  #;(when (or (hash-table? container-eval)
	    (flomat? container-eval)
	    (array? container-eval))
    (error "assignment forbiden with hash table or flomat or array with 3 arguments : container =" container-eval))
  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-or-step-eval-pos)
	 

	 ;; T[: i2 :]
	 ;;  make it work between vector and string
	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: 3 :] <- (vector 1 2 3 4 5)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)
	 (((? slice?) i2 (? slice?))  (copy-slice-starting-at-zero-with-positive-step container-eval
											   expr-eval
											   i2
											   1))


	 ;; (container-copy! container-eval
	 ;; 		  0
	 ;; 		  expr-eval
	 ;; 		  0
	 ;; 		  i2))

	 
	 ;; T[i1 : :]
	 ;; make it work between vector and string
	 ;; > {a <+ (make-vector 7 0)}
	 ;; > {a[3 : :] <- "zob"}
	 ;; > a
	 ;; '#(0 0 0 #\z #\o #\b 0)
	 ((i1 (? slice?) (? slice?))  (copy-slice-with-positive-step container-eval
									  expr-eval
									  i1
									  (+ i1 (expr-length expr-eval))
									  1))


	  ;; (container-copy! container-eval
	  ;; 		   i1
	  ;; 		   expr-eval))
	 
	 
	 ;; T[: : s3]
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; '#(1 2 3 4 5 6 7 8 9)
	 ;; > {v[: : 2] <- (vector -1 -2 -3 -4 -5)}
	 ;;> v
	 ;;'#(-1 2 -2 4 -3 6 -4 8 -5)


	
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; >  {v[: : -2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(-5 2 -4 4 -3 6 -2 8 -1)

	 ;; Python:
	 ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	 ;; v[: : -2] = [-1, -2, -3, -4, -5]
	 ;; v
	 ;; [-5, 2, -4, 4, -3, 6, -2, 8, -1]

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: : -2] <- "abcde"}
	 ;; > v
	 ;; '#(#\e 2 #\d 4 #\c 6 #\b 8 #\a)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: : -2] <- (vector -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13)[: : 2]}
	 ;; > v
	 ;; '#(-9 2 -7 4 -5 6 -3 8 -1)

	 ;;> {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: : -2] <- "abcdefghijklmnop"[: : 2]}
	 ;; > v
	 ;; '#(#\i 2 #\g 4 #\e 6 #\c 8 #\a)
	 (((? slice?) (? slice?) step-not-used)
	  
	  (cond ((vector? container-eval)

		 (begin-def
		  (define size-input (vector-length container-eval))
		  (check-step index3-or-keyword-or-step-eval)
		 		   
		  (if (< index3-or-keyword-or-step-eval 0) ;; with negative step we start at end of vector (like in Python)
		      ;;(vector-copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		      ;; the generic method allows compatibility between vectors and strings but is less fast
		      (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		      ;;(vector-copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))
		      (copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))))

		;; > {s <+ (string-append "abcdefgh")}
		;; "abcdefgh"
		;;> {s[: : 2] <- "ABCD"}
		;;> s
		;;"AbBdCfDh"
		((string? container-eval)

		 (begin-def
		  (check-step index3-or-keyword-or-step-eval)
		  
		  (define size-input (string-length container-eval))
		  
		  (if (< index3-or-keyword-or-step-eval 0) ;; with negative step we start at end of string (like in Python)
		      ;;(string-copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		      ;; the generic method allows compatibility between vectors and strings but is less fast
		      (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		      ;;(string-copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))
		      (copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))))
		 
		 (else (error "Slicing only for vector and string."))))
	 
	 


	 ;; T[i1 : i3]
	 ;; {s <+ (string-append "abcdefgh")}
	 ;; s
	 ;; "abcdefgh"
	 ;; > {s[2 : 4] <- "zob"}
	 ;; > s
	 ;; "abzoefgh"
	 ;; > {s[2 : 4] <- "zo"}
	 ;; > s
	 ;; "abzoefgh"


	 ;; > {v <+ (vector 1 2 3 4)}
	 ;; > {v[1 : 3] <- "abcdef"[2 : 4]}
	 ;; > v
	 ;; '#(1 #\c #\d 4)
	 
	 ;;  make it work between vector and string
	 ((i1 (? slice?) i3) (copy-slice-with-positive-step container-eval
								 expr-eval
								 i1
								 i3
								 1))


	  ;; (container-copy! container-eval
	  ;; 		   i1
	  ;; 		   expr-eval
	  ;; 		   0
	  ;; 		   (- i3 i1)))
	 

	 ;; T[i1 i2 i3]
	 ((i1 i2 i3) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
	      ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
	      #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3)))
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval index3-or-keyword-or-step-eval))) ;;(array-n-dim-set! array value i1 i2)
	      (array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)))  
	 
	 ) ;; end match

  ) ;; end define






(define (assignment-argument-4 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval)
  
  (<+ index1-or-keyword-eval-pos index1-or-keyword-eval)
  (<+ index2-or-keyword-eval-pos index2-or-keyword-eval)
  (<+ index3-or-keyword-eval-pos index3-or-keyword-eval)
  (<+ index4-or-step-eval-pos  index4-or-step-eval)
  
  (declare container-length); container-copy!)

  (when (not (or (vector? container-eval)  (string? container-eval)  (array? container-eval)))
	     (error "assignment-argument-4 : container type not compatible : " container-eval))
  
  (if (vector? container-eval)
      ;;($>
       (<- container-length  vector-length)
       ;;(<- container-copy!  vector-copy!))
      ;;($>  ;; a string
       (<- container-length  string-length)
       ;;(<- container-copy!  string-copy!))
       )
  
  
  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-step-eval-pos container-length container-eval)
 
  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-eval-pos index4-or-step-eval-pos)

	 ;; T[i1 : i2 :]  i1 start , i2 end
	 ((i1 (? slice?) i2 (? slice?)) (assignment-argument-3 container-eval i1 slice i2 expr-eval))
	 
	 ;; T[: i2 : s3]
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; '#(1 2 3 4 5 6 7 8 9)
	 ;; > {v[: 6 : 2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(-1 2 -2 4 -3 6 7 8 9)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[: 6 : -2] <- (vector -1 -2 -3 -4 -5)}
	 ;; > v
	 ;; '#(1 2 3 4 5 6 7 8 -1)
	 (((? slice?) i2 (? slice?) step-not-used)

	  (<+ step index4-or-step-eval)

	  (check-step step)
	  
	  ;;(<+ i 0)

	  ;;(display "assignment : assignment-argument-4 ") (newline)
	  
	  (if (< step 0) ;; with negative index we start at end of vector (like in Python)
	         
	      (copy-slice-with-negative-step container-eval
	      				     expr-eval
	      				     (- (container-length container-eval) 1)
	      				     i2
	      				     step)

	      
	      (copy-slice-starting-at-zero-with-positive-step container-eval
	      						      expr-eval
	      						      i2
	      						      step)))

	 
	 
	 
	 ;; T[i1 : : s3]
	 ((i1 (? slice?) (? slice?) step-not-used)

	  ;;(display "critical zone") (newline)

	  (<+ step index4-or-step-eval)

	  ;; > {s <+ (string-append "abcdefgh")}
	  ;; "abcdefgh"
	  ;; {s[3 : : 2] <- "0000"}
	  ;; > s
	  ;; "abc0e0g0"

	  ;; {s[5 : : -2] <- "0000"}
	  ;; s
	  ;; "a0c0e0gh"

	  ;; > {v <+ (vector 1 2 3 4 5 6 7 8)}
	  ;; > {v[3 : : 2] <- (vector -1 -2 -3 -4)}
	  ;; > v
	  ;; '#(1 2 3 -1 5 -2 7 -3)
	  
	  ;; > {v <+ (vector 1 2 3 4 5 6 7 8)}
	  ;; > {v[3 : : 2] <- (vector -1 -2 -3)}
	  ;; > v
	  ;; '#(1 2 3 -1 5 -2 7 -3)

	  ;; > {v <+ (vector 1 2 3 4 5 6 7 8)}
	  ;; > {v[5 : : -2] <- (vector -1 -2 -3)}
	  ;; > v
	  ;; '#(1 -3 3 -2 5 -1 7 8)

	  ;; Python check:
	  ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	  ;; v[5:  : -2] = [-1, -2,-3 ]
	  ;; v
	  ;; [1, -3, 3, -2, 5, -1, 7, 8, 9]
	  (check-step step)
		    
	  (if (< step 0) ;; with negative index we start at end of vector (like in Python)
	      (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval i1 step)

	      ($+>
	       (<+ size-input (container-length container-eval))
	       (copy-slice-with-positive-step container-eval expr-eval i1 size-input step))))
	 
	 
	 
	 ;; T[i1 i2 i3 i4]
	 ((i1 i2 i3 i4) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
	      ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
	      #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4)))
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval))) ;;(array-n-dim-set! array value i1 i2)
	      (array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval)))  
	 
	 ) ;; end match
  
  )





(define (assignment-argument-5 container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval)

  ;; (display "assignment-argument-5") (newline)
  ;; (display "container-eval=") (display container-eval) (newline)
  ;; (display "index1-eval=") (display index1-eval) (newline)
  ;; (display "index2-or-keyword-eval=") (display index2-or-keyword-eval) (newline)
  ;; (display "index3-eval=") (display index3-eval) (newline)
  ;; (display "index4-or-keyword-eval=") (display index4-or-keyword-eval) (newline)
  ;; (display "index5-or-step-eval=") (display index5-or-step-eval) (newline)
  ;; (display "expr-eval=") (display expr-eval) (newline)
  
  
  (<+ index1-eval-pos  index1-eval)
  (<+ index2-or-keyword-eval-pos  index2-or-keyword-eval)
  (<+ index3-eval-pos  index3-eval)
  (<+ index4-or-keyword-eval-pos  index4-or-keyword-eval)
  (<+ index5-or-step-eval-pos  index5-or-step-eval)

  (declare container-length)

  (when (not (or (vector? container-eval)  (string? container-eval)  (array? container-eval)))
	     (error "assignment-argument-5 : container type not compatible : " container-eval))
 
  
  (if (vector? container-eval)
      (<- container-length  vector-length)
      ;; a string
      (<- container-length  string-length))
      
  
  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index5-or-step-eval-pos container-length container-eval)
  
  
  (match (list index1-eval-pos index2-or-keyword-eval-pos index3-eval-pos index4-or-keyword-eval-pos index5-or-step-eval-pos)

	 ;; T[i1 : i2 : step]	 	 
	 ;; > {s <+ (string-append "abcdefgh")}
	 ;; > {s[2 : 7 : 2] <- "1234"}
	 ;; > s
	 ;; "ab1d2f3h"

	 ;; from Python:
	 ;; v=[1, 2, 3, 4, 5, 6, 7, 8,9]
	 ;; v[7: 2 : -2] = [-1, -2, -3, -4, -5]
	 ;; Traceback (most recent call last):
	 ;;   File "<pyshell#26>", line 1, in <module>
	 ;;     v[7: 2 : -2] = [-1, -2, -3, -4, -5]
	 ;; ValueError: attempt to assign sequence of size 5 to extended slice of size 3
	 ;; v[7: 2 : -2] = [-1, -2, -3]
	 ;; v
	 ;; [1, 2, 3, -3, 5, -2, 7, -1, 9]
	 
	 ;; > {v <- (vector 1 2 3 4 5 6 7 8 9)}
	 ;; >  {v[7 : 2 : -2] <- (vector -1 -2 -3)}
	 ;; > v
	 ;; '#(1 2 3 -3 5 -2 7 -1 9)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[7 : 3 : -2] <- (vector -1 -2 -3)}
	 ;; > v
	 ;; '#(1 2 3 4 5 -2 7 -1 9)
	 ((i1 (? slice?) i2 (? slice?) step-not-used)
	  
	  (<+ step index5-or-step-eval)

	  ;; (display "container-eval=") (display container-eval) (newline)
	  ;; (display "i1=") (display i1) (newline)
	  ;; (display "i2=") (display i2) (newline)
	  ;; (display "step=") (display step) (newline)
	  ;; (display "expr-eval=") (display expr-eval) (newline)
	  
	  (copy-stepped-slice container-eval expr-eval i1 i2 step))
	 
	 
	 ;; T[i1 i2 i3 i4 i5]
	 ((i1 i2 i3 i4 i5) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      ;; the indexes after the first one are indexes of the sub-vectors : their negative values count from the end
	      ;; of the sub-vector (done by function-array-n-dim-set!) , not of container-eval
	      #;(function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4 i5)))
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval))) ;;(array-n-dim-set! array expr-eval i1 i2)
	      (array-set! container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval)))  

	 ) ;; end match
  
  ) 





(define (assignment-argument-6-and-more container expr args)

  (when (not (or (vector? container)  (array? container)))
	     (error "assignment-argument-6-and-more : container type not compatible : " container))
    
  (if (vector? container)
      (function-array-n-dim-set! container expr (reverse args)) ;; (array-n-dim-set! array value index1 index2 ...)
      (array-set! container (list->vector args) expr))) 





;; this version can set values for arrays,hash tables,etc
;; it uses the Scheme+R6RS assignment operator: <-
(define-syntax set!-values-plus
  (syntax-rules ()
    ((_ (var var* ...) exp)
     (call-with-values
       (lambda () exp)
       (lambda (value . rest)
         (<- var value) ;; instead of set! i use the Scheme+R6RS assignment operator
         (set!-values-plus (var* ...) (apply values rest)))))
    ((_ () exp)
     (call-with-values
       (lambda () exp)
       (lambda () (values)))))) ; nothing to do!

;; (define x)
;; (define y)
;; (define z)
;; (set!-values (x y z) (values 0 1 2))
;; (pk x y z)


;; this define a variable or if it already exists set it to '()
;; no more used : the variables were set to '() before the computation of the values of the right hand side ,
;; so {(a b) <- (values b {a + b})} gave an error
#;(define-syntax define-or-clear-values
  (syntax-rules ()
    ((_ var ...) (begin
		    (<- var '()) ; in racket <- can possibly define the value (if not already defined)
		    ...))))

;; define a variable only if it is not already defined (its value is set afterwards by set!-values-plus) ,
;; an already defined variable keeps its value until the assignment : the right hand side can use it
;; > (define a 0)
;; > (define b 1)
;; > {(a b) <- (values b {a + b})}
;; > (list a b)
;; '(1 1)
(define-syntax define-if-not-defined
  (syntax-rules ()
    ((_ (brket-aply container index ...)) (void)) ; T[i] : the container exists already
    ((_ var) (if-defined var
			 (void) ; already defined : nothing to do
			 (define var '())))))

;; define or/and set! values
#;(define-syntax define-or/and-set!-values
  (syntax-rules ()
    ((_ (var ...) expr)
     (begin
       (define-or-clear-values var ...)
       (set!-values-plus (var ...) expr)))))

(define-syntax define-or/and-set!-values
  (syntax-rules ()
    ((_ (var ...) expr)
     (begin
       (define-if-not-defined var)
       ...
       (set!-values-plus (var ...) expr)))))


;; examples:

;; {(a b c d e) <- (values 1 2 3 4 5)}
;; id=.#<syntax a>
;; if-defined : where=#f

;; id=.#<syntax b>
;; if-defined : where=#f

;; id=.#<syntax c>
;; if-defined : where=#f

;; id=.#<syntax d>
;; if-defined : where=#f

;; id=.#<syntax e>
;; if-defined : where=#f

;; id=.#<syntax a>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a 0 0 0)

;; id=.#<syntax b>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> b #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> b 0 0 0)

;; id=.#<syntax c>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c 0 0 0)

;; id=.#<syntax d>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d 0 0 0)

;; id=.#<syntax e>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e 0 0 0)

;; > (list a b c d e)
;; '(1 2 3 4 5)



;; (define T (make-vector 5))
;; {(a T[3] c d e) <- (values 1 -2 3 4 5)}
;; id=.#<syntax a>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a 0 0 0)

;; id=.#<syntax c>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c 0 0 0)

;; id=.#<syntax d>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d 0 0 0)

;; id=.#<syntax e>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e 0 0 0)

;; id=.#<syntax a>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a 0 0 0)

;; id=.#<syntax c>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c 0 0 0)

;; id=.#<syntax d>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d 0 0 0)

;; id=.#<syntax e>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e 0 0 0)

;; > {list(a T[3] c d e)}
;; '(1 -2 3 4 5)
;; > T
;; '#(0 0 0 -2 0)
;; > 



;; > (return-values (values 1 2 3))
;; 1
;; 2
;; 3
;; > (return-values (values 1))
;; 1
;; > (return-values 1)
;; 1
;; > (return-values (sin 0.7))
;; 0.644217687237691
;; no more used : the chained assignments compute the values only once
#;(define-syntax return-values
    (syntax-rules ()
      ((_ expr) (call-with-values (lambda () expr)
                                  values))))

;; > ((create-return-values 3))
;; 3
;; > ((create-return-values (values 1 2 3)))
;; 1
;; 2
;; 3
;; no more used : the procedure created computed again expr at each call
#;(define-syntax create-return-values
    (syntax-rules ()
      ((_ expr) (lambda () (return-values expr)))))



;; note: code below is AI generated
;; Specialized string assignment macro
(define-syntax <-string-string*

  (lambda (stx)
    
    (syntax-case stx ()

      
      ;; example: {a[4] <- 7}
      ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
      ((_ (brket-aply container index ...) expr)  ; possible to have NO index :
					; minimal form is (_ (brket-aply container) expr)

        #| We will let the second $bracket-apply$ be executed and use the return value
           but do not execute the first $bracket-apply$ as we needs to interpret the syntax and deal with their arguments. |#
        
        ;; display { } as they are.
        ;;(print-mpair-curly-braces #f)

        ;; we parse arguments at posteriori of SRFI 105 parser
        (with-syntax

            ((parsed-args

              (cons #'list (parse-square-brackets-arguments-lister-syntax (syntax->list #'(index ...))
                                                                          (list (cons '* #'fx*)
                                                                                (cons '- #'fx-)
                                                                                (cons '+ #'fx+)
                                                                                (cons '/ #'fxquotient)))))) ; end definitions

	  ;(display "<- : #'parsed-args=") (display #'parsed-args) (newline)
        
	 ;; we parse arguments at posteriori
	 ;; putting code here optimise run-time
	 (case (length
		(cdr
		 (syntax->datum #'parsed-args)))

	   
	   ;; 0 argument in []
	   ;; T[]
	   ;; {v[] <- #(1 2 3)}
	   ;; > v
	   ;;'#(1 2 3)
	   ((0)
	    ;;#'(begin
	    ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
	    #'(assignment-argument-0 container expr));)  ; possible to have NO index

	   ;; 1 argument in [ ]
	   ;; T[index]
	   ((1)
	    ;;(begin
	    ;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
	    ;; written inline as for the vectors : an index (not a slice) , negative from the end as in Python
	    #;#'(assignment-argument-1-string container
                                              (first parsed-args)
                                              expr)
	    (with-syntax ((index (cadr (syntax->list #'parsed-args))))
	      #'(let ((c container) (i index) (e expr))
		  (if (fixnum? i)
		      (string-set! c (if (fx< i 0) (fx+ (string-length c) i) i) e)
		      (assignment-argument-1-string c i e)))));)

	   ;; 2 arguments in [ ]
	   ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
	   ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
	   ;; '#(3 4 5)
	   ((2)
	    ;; #'(begin
	    ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
	    #'(assignment-argument-2-string container
                                            (first parsed-args)
                                            (second parsed-args)
                                            expr
                                            #t));)

	   ;; 3 arguments in [ ]
	   ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
	   ((3)
	    ;; #'(begin
	    ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
	    ;;#'parsed-args);)
	    #'(assignment-argument-3-string container
                                            (first parsed-args)
                                            (second parsed-args)
                                            (third parsed-args)
                                            expr
                                            #t));)

	   ;; 4 arguments in [ ]
	   ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
	   ((4) #'(assignment-argument-4-string container
                                                 (first parsed-args)
                                                 (second parsed-args)
                                                 (third parsed-args)
                                                 (fourth parsed-args)
                                                 expr
                                                 #t))

	   ;; 5 arguments in [ ]
	   ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
	   ((5) #'(assignment-argument-5-string container
                                                 (first parsed-args)
                                                 (second parsed-args)
                                                 (third parsed-args)
                                                 (fourth parsed-args)
                                                 (fifth parsed-args)
                                                 expr
                                                 #t))

	   ;; more than 5 arguments in [ ]
	   ;; T[i1 i2 i3 i4 i5 i6 ...]
	   (else ; case
	    #'(assignment-argument-6-and-more-string container parsed-args expr)))))
      
      ;;(<- x 5)
      ((_ var expr)
       
       ;; (begin
       ;;   (display "<- : variable set!") (newline)
       ;;   (display "var =") (display var) (newline)
       ;;   (display "expr =") (display expr) (newline)
       ;;#`(set! var expr))
       ;; (display "after set! : var =") (display var) (newline)))
       ;;var))

       #'(if-defined var
		    (set! var expr)
		    (define var expr)))

      ;; Warning: not sure if code below should also be kept in specialized macros
      
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

      ;; > (declare a b c d)
      ;; > {(a b) <- (c d) <- (values 5 7)}
      ;; > a
      ;; 5
      ;; > b
      ;; 7
      ;; > c
      ;; 5
      ;; > d
      ;; 7

      ;; without declare:
      ;; > {(a b) <- (c d) <- (values 5 7)}
      ;; > (list a b c d)
      ;; '(5 7 5 7)
      ((_ var var1 ... expr) ; i suppose varN are of the type (x y z ...)
       
       ;;(<- var (<- var1 ... expr))) ; probably this is commented because the syntax is not allowed

       #'(begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result
	   ;;(display "<- : case (_ var var1 ... expr)") (newline)
	   ;;(<- var expr)
	   ;; create-return-values computed again expr for each var
	   ;;(define return-values-of-expr (create-return-values expr))
	   (define values-of-expr (call-with-values (lambda () expr) list)) ; expr is computed only once
	   ;;(<-string-string* var (return-values-of-expr))
	   (<-string-string* var (apply values values-of-expr))
	   ;;(display "<- : case : passed (<- var expr)") (newline)
	   ;;(display "<- : case : var=") (display var) (newline)
	   ;;(<- var1 var)
	   ;;(<-string-string* var1 (return-values-of-expr))
	   (<-string-string* var1 (apply values values-of-expr))
	   ...)))))



;;(print-mpair-curly-braces #f)






(export
	<-
	->
	<-vector-vector*
	<-string-string*
	←
	→
	:=
	=:
	:=>
	<v
	v>
	⇜
	⇝)
 ; end library

