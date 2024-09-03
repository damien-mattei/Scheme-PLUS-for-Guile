;; assignment version Guile (support also growable vectors,arrays,string,hash tables)

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

;; note that slicing separator is now : , no more $ (see slice.scm)


;; (use-modules (assignment))

(define-module (assignment)
  
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-69)
  #:use-module (ice-9 match) ;; for assignment
  #:use-module (parse-square-brackets)
  #:use-module (for_next_step)
  #:use-module (array)
  #:use-module (slice)
  #:use-module (declare)
  #:use-module (block)
  #:use-module (def)
  #:use-module (bracket-apply)
  #:use-module (overload)
  #:use-module (growable-vector)
  #:use-module (set-values-plus)

  #:export (<- ->
	    ← →
	    :=  =:
	    <v v>
	    ⇜ ⇝))
  

;; scheme@(guile-user)> {v <+ (vector 1 2 3 4 5 6 7 8 9)}
;; scheme@(guile-user)> {v[7 : 3 : -2] <- (vector -1 -2 -3)}
;; <- : #'(index ...) = (#<syntax:unknown file:7:3 7> #<syntax:unknown file:7:5 :> #<syntax:unknown file:7:7 3> #<syntax:unknown file:7:9 :> #<syntax:unknown file:7:11 -2>)
;; <- : #'parsed-args=(#<syntax:assignment.scm:148:32 list> #<syntax:unknown file:7:3 7> #<syntax:unknown file:7:5 :> #<syntax:unknown file:7:7 3> #<syntax:unknown file:7:9 :> #<syntax:unknown file:7:11 -2>)
;; scheme@(guile-user)> v
;; $1 = #(1 2 3 4 5 -2 7 -1 9)

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

       #'(set!-values-plus (var) expr))
      
    

      ;; example: {a[4] <- 7}
      ;; $bracket-apply$ is from SRFI 105  $bracket-apply$ is an argument of the macro <- here
      
      ((_ (brket-applynext container index ...) expr)  ; possible to have NO index :
					; minimal form is (_ (brket-applynext container) expr)

       ;; We will let the RHS $bracket-apply$ be executed and forbid the execution of LHS $bracket-apply$. (Right/Left Hand S-expr)
       (cond ;; ((equal? (quote $bracket-apply$next) (syntax->datum #'brket-applynext))  ;;  $bracket-apply$next , already parsed and optimised by parser

	     ;;  #'(assignmentnext container expr index  ...)) ; possible to have NO index



	     ;; $bracket-apply$
	     ;; integrated curly-infix of guile (no parsing) at REPL
	     ;; (define T (make-vector 7))
	     ;; {T[2 + 1] <- -7}
	     ((equal? (quote $bracket-apply$) (syntax->datum #'brket-applynext)) ;; curly-infix

	      (display "<- : #'(index ...) = ") (display #'(index ...)) (newline)
	     	      
	      ;; we parse arguments at posteriori of SRFI 105 parser

	      (with-syntax ((parsed-args
			     
	      		     (cons #'list ;; otherwise:Wrong type to apply: 0 ,list will be interpreted as code !
				   (parse-square-brackets-arguments-lister-syntax #'(index ...))))
			     
			     ;;#`(list #,@(parse-square-brackets-arguments-lister-syntax #'(index ...))))

			    ) ; end definitions
			   
			   (display "<- : #'parsed-args=") (display #'parsed-args) (newline)

			   #'(assignment-next4list-args container parsed-args expr))) 
			  
			   ;; (case (length (cdr #'parsed-args)) ; putting code here optimise run-time
			  			       
			   ;;   ;; 0 argument in []
			   ;;   ;; T[]
			   ;;   ;; {v[] <- #(1 2 3)}
			   ;;   ;; > v
			   ;;   ;;'#(1 2 3)
			   ;;   ((0) 
			   ;;    ;;#'(begin
			   ;;    ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
			   ;; 	  #'(assignment-argument-0 container expr));)  ; possible to have NO index

			   ;;   ;; 1 argument in [ ]
			   ;;   ;; T[index]
			   ;;   ((1)
			   ;;    ;;#'(begin
			   ;;     	;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
			   ;; 	  #'(assignment-argument-1 container
			   ;; 				   (first parsed-args)
			   ;; 				   expr));)

			   ;;   ;; 2 arguments in [ ]
			   ;;   ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]   
			   ;;   ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
			   ;;   ;; '#(3 4 5)
			   ;;   ((2)
			   ;;    ;; #'(begin
			   ;;    ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
			   ;; 	  #'(assignment-argument-2 container
			   ;; 				   (first parsed-args)
			   ;; 				   (second parsed-args)
			   ;; 				   expr));)

			   ;;   ;; 3 arguments in [ ]
			   ;;   ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
			   ;;   ((3)
			   ;;    ;; #'(begin
			   ;;    ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
			   ;; 	  ;;#'parsed-args);)
			   ;; 	  #'(assignment-argument-3 container					  
			   ;; 	  			 (first parsed-args)
			   ;; 	  			 (second parsed-args)
			   ;; 	  			 (third parsed-args)
			   ;; 	  			 expr));)
			     
			   ;;   ;; 4 arguments in [ ]
			   ;;   ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
			   ;;   ((4)
			   ;;    #'(assignment-argument-4 container
			   ;; 			       (first parsed-args)
			   ;; 			       (second parsed-args)
			   ;; 			       (third parsed-args)
			   ;; 			       (fourth parsed-args)
			   ;; 			       expr))

			   ;;   ;; 5 arguments in [ ]
			   ;;   ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
			   ;;   ((5)
			   ;;    #'(assignment-argument-5 container
			   ;; 			       (first parsed-args)
			   ;; 			       (second parsed-args)
			   ;; 			       (third parsed-args)
			   ;; 			       (fourth parsed-args)
			   ;; 			       (fifth parsed-args)
			   ;; 			       expr))

			   ;;   ;; more than 5 arguments in [ ]
			   ;;   ;; T[i1 i2 i3 i4 i5 i6 ...]
			   ;;   (else ; case
			   ;;    #'(assignment-argument-6-and-more container parsed-args expr)))))

	     (else ; cond
	      #'(set!-values-plus (brket-applynext container index ...) expr)))) ; warning: the argument's names does not match the use
      
      
      
      ;;(<- x 5)
      ((_ var expr)
       
       #'(set! var expr))
      
      
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
      ((_ var var1 ... expr)
       
       
       #'(begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result
	   ;;(display "<- : case (_ var var1 ... expr)") (newline)
	   
	   (define return-values-of-expr (create-return-values expr))
	   (<- var (return-values-of-expr))
	   ;;(display "<- : case : passed (<- var expr)") (newline)
	   ;;(display "<- : case : var=") (display var) (newline) 
	   
	   (<- var1 (return-values-of-expr))
	   ...))
      
     
      )))


(define (assignment-next4list-args container parsed-args expr) 

    (case (length parsed-args) ; putting code here optimise run-time
			  			       
			     ;; 0 argument in []
			     ;; T[]
			     ;; {v[] <- #(1 2 3)}
			     ;; > v
			     ;;'#(1 2 3)
			     ((0) 
			      ;;(begin
			      ;; 	  (display "<- case 0 : parsed-args=") (display parsed-args) (newline)
				  (assignment-argument-0 container expr));)  ; possible to have NO index

			     ;; 1 argument in [ ]
			     ;; T[index]
			     ((1)
			      ;;(begin
			       	;;  (display "<- case 1 : parsed-args=") (display parsed-args) (newline)
				  (assignment-argument-1 container
							   (first parsed-args)
							   expr));)

			     ;; 2 arguments in [ ]
			     ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]   
			     ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
			     ;; '#(3 4 5)
			     ((2)
			      ;; (begin
			      ;; 	  (display "<- case 2 : parsed-args=") (display parsed-args) (newline)
				  (assignment-argument-2 container
							   (first parsed-args)
							   (second parsed-args)
							   expr));)

			     ;; 3 arguments in [ ]
			     ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
			     ((3)
			      ;; (begin
			      ;; 	  (display  "<- case 3 : 'parsed-args=") (display 'parsed-args) (newline)
				  ;;#'parsed-args);)
				  (assignment-argument-3 container					  
				  			 (first parsed-args)
				  			 (second parsed-args)
				  			 (third parsed-args)
				  			 expr));)
			     
			     ;; 4 arguments in [ ]
			     ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
			     ((4)
			      (assignment-argument-4 container
						       (first parsed-args)
						       (second parsed-args)
						       (third parsed-args)
						       (fourth parsed-args)
						       expr))

			     ;; 5 arguments in [ ]
			     ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
			     ((5)
			      (assignment-argument-5 container
						       (first parsed-args)
						       (second parsed-args)
						       (third parsed-args)
						       (fourth parsed-args)
						       (fifth parsed-args)
						       expr))

			     ;; more than 5 arguments in [ ]
			     ;; T[i1 i2 i3 i4 i5 i6 ...]
			     (else ; case
			      (assignment-argument-6-and-more container parsed-args expr))))




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



(define-syntax ←
  
  (syntax-rules ()
    ((← . args)
     (<- . args))))




;; scheme@(guile-user)> {x :+ 7}
;; scheme@(guile-user)> x
;; $1 = 7
;; scheme@(guile-user)> {x := "hello"}
;; scheme@(guile-user)> x
;; $2 = "hello"

(define-syntax :=

  (syntax-rules ()

    ((_ var ...) (<- var ...))))


(define-syntax =:

  (syntax-rules () 

    ((_ expr ...) (-> expr ...))))




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
    
    ((_ (var1 ...) expr) ;;(begin
			   (set!-values-plus (var1 ...) expr)
			   ;; (values var1 ...)))
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
     


(define-syntax check-step

  (syntax-rules ()

    ((_ step)  (when (= step 0)
		     (error "assignment : slice step cannot be zero")))))




;; DEPRECATED
(define-syntax assignmentnext

  (syntax-rules ()

    ((_ container expr args)

     ;; (begin

     ;;   (display "assignmentnext") (newline) (newline)

     (case (length args)

       ;; 0 argument in []
       ;; T[]
       ((0)
	;;(display "assignmentnext : container =") (display container) (newline)
	(assignment-argument-0 container expr))
       
       ;; 1 argument in [ ]
       ;; T[index]
       ((1) (assignment-argument-1 container (first args) expr))
       
       ;; 2 arguments in [ ]
       ;; ex: T[i1 :] , T[: i2], T[i1 i2] , T[: :]
       
       ;; {#(1 2 3 4 5)[inexact->exact(floor(2.7)) :]}
       ;; '#(3 4 5)
       ((2) (assignment-argument-2 container
				   (first args)
				   (second args)
				   expr))

       ;; 3 arguments in [ ]
       ;; T[i1 : i2] , T[i1 i2 i3] , T[: : s]
       ((3) (assignment-argument-3 container
				   (first args)
				   (second args)
				   (third args)
				   expr))


       ;; 4 arguments in [ ]
       ;; T[: i2 : s] , T[i1 : : s] , T[i1 : i3 :] , T[i1 i2 i3 i4]
       ((4) (assignment-argument-4 container
				   (first args)
				   (second args)
				   (third args)
				   (fourth args)
				   expr))

       

       ;; 5 arguments in [ ]
       ;; T[i1 : i3 : s] , T[i1 i2 i3 i4 i5]
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
	(assignment-argument-6-and-more container expr args))))));); one more parenthesis for begin


  


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




(define (copy-slice-with-negative-step container-eval expr-eval i1 i2 step)
  (for (($> (<+ k i1) (<+ i 0)) (> k i2) (<- k (+ k step)))
       (assignment-argument-1-index container-eval k ($bracket-apply$next expr-eval i))
       (<- i (+ i 1))))

(define (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval i1 step)
  (for (($> (<+ k i1) (<+ i 0)) (>= k 0) (<- k (+ k step)))
       (assignment-argument-1-index container-eval k ($bracket-apply$next expr-eval i))
       (<- i (+ i 1))))


(define (copy-slice-with-positive-step container-eval expr-eval i1 i2 step)
  (for (($> (<+ k i1) (<+ i 0)) (< k i2) (<- k (+ k step)))
       (assignment-argument-1-index container-eval k ($bracket-apply$next expr-eval i))
       (<- i (+ i 1))))


(define (copy-slice-starting-at-zero-with-positive-step container-eval expr-eval i2 step)
  ;;(display "copy-slice-starting-at-zero-with-positive-step : container-eval=") (display container-eval) (newline)
  (for (($> (<+ k 0) (<+ i 0)) (< k i2) (<- k (+ k step)))
       (<+ bkt ($bracket-apply$next expr-eval i))
       ;;(display "bkt=") (display bkt) (newline)
       (assignment-argument-1-index container-eval k bkt)
       (<- i (+ i 1))))



       
   

(define (copy-stepped-slice container-eval expr-eval i1 i2 step)
  
  (check-step step)

  (if (< step 0) ;; with negative step we start at end of vector (like in Python)
      
      (copy-slice-with-negative-step container-eval expr-eval i1 i2 step)
         
      (copy-slice-with-positive-step container-eval expr-eval i1 i2 step)))



;; macro save lines of code
(define-syntax negative->positive-index-when-not-slice
  
  (syntax-rules ()

    ((_ i container-length container-eval) 
     ;; transform the negative indexes in positive ones when not slices
     (when (and (not (equal? i slice))  (< i 0))
	   (<- i (+ (container-length container-eval) i))))))

;; T[:]
(define (assignment-argument-1-slice container-eval expr-eval)

  (cond ((or (vector? container-eval) (growable-vector? container-eval))
	 (vector-copy! container-eval
		       0
		       expr-eval))
	
	((hash-table? container-eval) (error "slicing not permitted with hash table"))
	
	((string? container-eval)
	 (string-copy! container-eval
		       0
		       expr-eval))
	(else (error "slicing not allowed with this container"))))




;; normal case
;; {T[3] <- T[7 2 4]}
      
(define (assignment-argument-1-index container-eval index-eval expr-eval)

  ;;(display "assignment-argument-1-index  : container-eval=") (display container-eval) (newline)

  (cond ((or (vector? container-eval) (growable-vector? container-eval))
	 (when (< index-eval 0) ;; deal with negative index
	       (<- index-eval (+ (vector-length container-eval) index-eval)))
	 (vector-set! container-eval index-eval expr-eval))
	
	((hash-table? container-eval)
	 (hash-table-set! container-eval index-eval expr-eval))
	
	((string? container-eval)
	 (when (< index-eval 0) ;; deal with negative index
	       (<- index-eval (+ (string-length container-eval) index-eval)))
	 (string-set! container-eval index-eval expr-eval))
	
	((array? container-eval)
	 (srfi25-array-set! container-eval index-eval expr-eval))

	(else ;; overloaded
	 ($+>
	   (define args-lst (list container-eval index-eval))
	   (define setter! (find-setter-for-overloaded-square-brackets args-lst))
	   (setter! container-eval index-eval expr-eval)))))



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

    ((_ container-eval expr-eval)
     ;; (display "assignment-argument-0 : container-eval =")
     ;; (display container-eval)
     ;; (newline)
     (<- container-eval expr-eval))))




(define (assignment-argument-1 container-eval index-eval expr-eval)
  
  
  (if (equal? index-eval slice)  ;; T[:]
      
       (assignment-argument-1-slice container-eval expr-eval)

      ;; normal case
      ;; {T[3] <- T[7 2 4]}
      
      (assignment-argument-1-index container-eval index-eval expr-eval)))


  

(define (assignment-argument-2 container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)

  ;;(display "assignment-argument-2 : expr-eval=") (display expr-eval) (newline)
  
  {index1-or-keyword-eval-pos <+ index1-or-keyword-eval} ;; pos for positive
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}

  (declare container-length container-copy! expr-length)
  
  (if (vector? container-eval) 
      ($>
       {container-length <- vector-length}
       {container-copy! <- vector-copy!}) ;; unfortunately vector-copy! does not support step
      ($>  ;; a string
       {container-length <- string-length}
       {container-copy! <- string-copy!}))
  
  (if (vector? expr-eval)
      (<- expr-length vector-length)
      (<- expr-length string-length))
      
  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  
  ;;(display "assignment-argument-2 : index1-or-keyword-eval-pos =") (display index1-or-keyword-eval-pos) (newline)
  ;;(display "assignment-argument-2 : index2-or-keyword-eval-pos =") (display index2-or-keyword-eval-pos) (newline)
  
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
	 (((? equal-slice?) (? equal-slice?)) ;;  make it work between vector and string
	  (copy-slice-starting-at-zero-with-positive-step container-eval
							  expr-eval
							  (expr-length expr-eval)
							  1))
	 
	 ;;  {s <+ (string-append "abcdefgh")}
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
	 ((i1 (? equal-slice?))
	  ;; make it work between vector and string
	  (copy-slice-with-positive-step container-eval
					 expr-eval
					 i1
					 (+ i1 (expr-length expr-eval))
					 1))

	 ;; > {a <+ (make-vector 7 0)}
	 ;; > a
	 ;; '#(0 0 0 0 0 0 0)
	 ;; > {a[: 3] <- (vector 1 2 3 4 5)}
	 ;; > a
	 ;; '#(1 2 3 0 0 0 0)
	 (((? equal-slice?) i2) ;; make it work between vector and string
	  (copy-slice-starting-at-zero-with-positive-step container-eval
							  expr-eval
							  i2
							  1))


	 ;; {a[1 2] <- 7}
	 ((i1 i2)

	  (cond ((vector? container-eval)  ;; normal case
		 (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2))))
		
		((array? container-eval)
		 (srfi25-array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval expr-eval)) ;; no SRFI 25 in Guile
		
		(else ;; overloaded
		 ($+> ;; why let () here? because define forbidden in something up ?
		   (define args-lst (list container-eval i1 i2))
		   (define setter! (find-setter-for-overloaded-square-brackets args-lst))
		   ;;(display "assignment-argument-2 : setter! =") (display setter!) (newline)
		   (setter! container-eval i1 i2 expr-eval))))) ; end match case

	 ) ;; end match

  

  )




(define (assignment-argument-3 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)

  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
  
  {index1-or-keyword-eval-pos <+ index1-or-keyword-eval}
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}
  {index3-or-keyword-or-step-eval-pos <+ index3-or-keyword-or-step-eval}
 

  (declare container-length container-copy! expr-length)
  
  (if (vector? container-eval)
      ($>
       {container-length <- vector-length}
       {container-copy! <- vector-copy!})
      ($>  ;; a string
       {container-length <- string-length}
       {container-copy! <- string-copy!}))


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
	 (( (? equal-slice?) i2 (? equal-slice?) )
	  
	  (copy-slice-starting-at-zero-with-positive-step container-eval
							  expr-eval
							  i2
							  1))

	 
	 ;; T[i1 : :]
	 ;; make it work between vector and string
	 ;; > {a <+ (make-vector 7 0)}
	 ;; > {a[3 : :] <- "zob"}
	 ;; > a
	 ;; '#(0 0 0 #\z #\o #\b 0)
	 ((i1 (? equal-slice?) (? equal-slice?))

	  (copy-slice-with-positive-step container-eval
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
	 (((? equal-slice?) (? equal-slice?) step-not-used)
	  
	  (cond ((vector? container-eval)
		 
		 (check-step index3-or-keyword-or-step-eval)
		 (let ((size-input (vector-length container-eval)))
		   
		  (if (< index3-or-keyword-or-step-eval 0) ;; with negative step we start at end of vector (like in Python)
		      ;; the generic method allows compatibility between vectors and strings but is less fast
		     (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		     (copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))))

		;; > {s <+ (string-append "abcdefgh")}
		;; "abcdefgh"
		;;> {s[: : 2] <- "ABCD"}
		;;> s
		;;"AbBdCfDh"
		((string? container-eval)
		 
		 (check-step index3-or-keyword-or-step-eval)
		 
		 (let ((size-input (string-length container-eval)))
		   
		   (if (< index3-or-keyword-or-step-eval 0) ;; with negative step we start at end of string (like in Python)
		     ;;(string-copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		     ;; the generic method allows compatibility between vectors and strings but is less fast
		     (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval (- size-input 1) index3-or-keyword-or-step-eval)
		     ;;(string-copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))
		     (copy-slice-with-positive-step container-eval expr-eval 0  size-input  index3-or-keyword-or-step-eval))))

		(else (error "Slicing only for vector and string."))))
	 
	 


	 ;; T[i1 : i3]
	 ;; {s <+ (string-append "abcdefgh")}
	 ;; "abcdefgh"
	 ;; > {s[2 : 4] <- "zob"}
	 ;; > s
	 ;; "abzoefgh"
	 ;; > {s[2 : 4] <- "zo"}
	 ;; > s
	 ;; "abzoefgh"

	 ;; 	 > {v <+ (vector 1 2 3 4)}
	 ;; > {v[1 : 3] <- "abcdef"[2 : 4]}
	 ;; > v
	 ;; '#(1 #\c #\d 4)

	 ;; scheme@(guile-user)> {v <+ (vector 1 2 3 4)}
	 ;; scheme@(guile-user)> v
	 ;; $1 = #(1 2 3 4)
	 ;; scheme@(guile-user)> {v[1 : 3] <- "abcdef"[2 : 4]}
	 ;; scheme@(guile-user)> v
	 ;; $2 = #(1 #\c #\d 4)

	 ;; {v[1 : 3] <- #(-1 -2 -3 -4 -5 -6 -7)[2 : 4]}
	 
	 ;;  make it work between vector and string
	 ((i1 (? equal-slice?) i3) (copy-slice-with-positive-step container-eval
								 expr-eval
								 i1
								 i3
								 1))
	 

	 ;; T[i1 i2 i3]
	 ((i1 i2 i3) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3))) ;;(array-n-dim-set! array value i1 i2)
	      (srfi25-array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-or-step-eval expr-eval)))
	 
	 ) ;; end match
  
  )  ;; end define


;; this portion of Scheme+ is written in... Scheme+ !!!

(define (assignment-argument-4 container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval)

  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
  
  {index1-or-keyword-eval-pos <+ index1-or-keyword-eval}
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}
  {index3-or-keyword-eval-pos <+ index3-or-keyword-eval}
  {index4-or-step-eval-pos <+ index4-or-step-eval}
  
  (declare container-length)
  
  (if (vector? container-eval)
      {container-length <- vector-length}
      ;; a string
      {container-length <- string-length})
      
  
  ;; transform the negative indexes in positive ones when not slices
  (negative->positive-index-when-not-slice index1-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index2-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index3-or-keyword-eval-pos container-length container-eval)
  (negative->positive-index-when-not-slice index4-or-step-eval-pos container-length container-eval)
  
  
  (match (list index1-or-keyword-eval-pos index2-or-keyword-eval-pos index3-or-keyword-eval-pos index4-or-step-eval-pos)

	 ;; T[i1 : i2 :]
	 ((i1 (? equal-slice?) i2 (? equal-slice?)) {container-eval[i1 slice i2] <- expr-eval})
	 
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
	 (((? equal-slice?) i2 (? equal-slice?) step-not-used)

	  ($+>
	   {step <+ index4-or-step-eval}

	   (check-step step)
	   
	   {i <+ 0}
	   
	   (if {step < 0} ;; with negative index we start at end of vector (like in Python)
	       (copy-slice-with-negative-step container-eval
	      				     expr-eval
	      				     (- (container-length container-eval) 1)
	      				     i2
	      				     step)

	      
	      (copy-slice-starting-at-zero-with-positive-step container-eval
	      						      expr-eval
	      						      i2
	      						      step))

	   ))
	 
	 
	 ;; T[i1 : : s3]
	 ((i1 (? equal-slice?) (? equal-slice?) step-not-used)

	  ($+>
	   {step <+ index4-or-step-eval}

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

	   ;; scheme@(guile-user)> {v <+ (vector 1 2 3 4 5 6 7 8)}
	   ;; scheme@(guile-user)> {v[5 : : -2] <- (vector -1 -2 -3)}
	   ;; scheme@(guile-user)> v
	   ;; $1 = #(1 -3 3 -2 5 -1 7 8)
	   (check-step step)
	   
	   (if (< step 0) ;; with negative index we start at end of vector (like in Python)
	       (copy-slice-starting-at-zero-with-negative-step container-eval expr-eval i1 step)
	       ($+>
		{size-input <+ (container-length container-eval)}
		(copy-slice-with-positive-step container-eval expr-eval i1 size-input step)))))
	 
	 
	 
	 ;; T[i1 i2 i3 i4]
	 ((i1 i2 i3 i4) 
	  
	  ;; normal case
	  (if (vector? container-eval)
	      (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4))) ;;(array-n-dim-set! array value i1 i2)
	      (srfi25-array-set! container-eval index1-or-keyword-eval index2-or-keyword-eval index3-or-keyword-eval index4-or-step-eval expr-eval))

	  )
	 
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
  
  (when (not {(vector? container-eval) or (string? container-eval) or
	      (array? container-eval) or (growable-vector? container-eval)})
	(error "assignment : container type not compatible : " container-eval))
 
  {index1-eval-pos <+ index1-eval}
  {index2-or-keyword-eval-pos <+ index2-or-keyword-eval}
  {index3-eval-pos <+ index3-eval}
  {index4-or-keyword-eval-pos <+ index4-or-keyword-eval}
  {index5-or-step-eval-pos <+ index5-or-step-eval}

  (declare container-length)
  
  (if (vector? container-eval)
      {container-length <- vector-length}
      ;; a string
      {container-length <- string-length})
  
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
	 
	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; >  {v[7 : 2 : -2] <- (vector -1 -2 -3)}
	 ;; > v
	 ;; '#(1 2 3 -3 5 -2 7 -1 9)

	 ;; > {v <+ (vector 1 2 3 4 5 6 7 8 9)}
	 ;; > {v[7 : 3 : -2] <- (vector -1 -2 -3)}
	 ;; > v
	 ;; '#(1 2 3 4 5 -2 7 -1 9)

	 ;; {v[7 : 3 : -2] <- (vector -1 -2 -3)}
	 ((i1 (? equal-slice?) i2 (? equal-slice?) step-not-used)

	  ($+>
	   {step <+ index5-or-step-eval}

	   (copy-stepped-slice container-eval expr-eval i1 i2 step)))
	  
	  
	  ;; T[i1 i2 i3 i4 i5]
	  ;;((list i1 i2 i3 i4 i5)
	  ((i1 i2 i3 i4 i5) 
	   
	   ;; normal case
	   (if (vector? container-eval)
	       (function-array-n-dim-set! container-eval expr-eval (reverse (list i1 i2 i3 i4 i5))) ;;(array-n-dim-set! array expr-eval i1 i2)
	       (srfi25-array-set! container-eval index1-eval index2-or-keyword-eval index3-eval index4-or-keyword-eval index5-or-step-eval expr-eval)))

	 ) ;; match

  )


;; scheme@(guile-user)> (declare T)
;; scheme@(guile-user)> {T[1 2 3 4 5 6 7] <- 7}
;; assignment-argument-6-and-more:
;; note: not tested (require a multi dimensional array more than 5 in dimension)
(define (assignment-argument-6-and-more container expr args)

  ;;(display "assignment-argument-6-and-more: expr=") (display expr) (newline) 

  (when (not {(vector? container) or (array? container)})
	     (error "assignment : container type not compatible : " container))
    
  (if (vector? container)
      (function-array-n-dim-set! container expr (reverse args)) ;; (array-n-dim-set! array value index1 index2 ...)
      (srfi25-array-set! container (list->vector args) expr)))  
