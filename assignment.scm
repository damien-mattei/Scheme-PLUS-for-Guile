;; assignment version Guile (support my growable vectors)

;; This file is part of Scheme+

;; Copyright 2021-2022 Damien MATTEI

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
;; scheme@(guile-user)> {T[2] <- 4}
;; <- : vector or array set!
;; $2 = 4

;; scheme@(guile-user)> {T[3] <- T[2]}
;; $bracket-apply$
;; <- : vector or array set!
;; $4 = 4
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $5 = 4

;; scheme@(guile-user)> '{x <- y <- 7}
;; $1 = (<- x y 7)
(define-syntax <-
  
  (syntax-rules ()
    ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    
    ;; one dimension array, example: {a[4] <- 7}
    ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
    ((_ (bracket-apply container index) expr)
     
     (begin

       ;; add a checking

       ;; scheme@(guile-user)> (use-modules (Scheme+))
       ;; scheme@(guile-user)> {a <+ (make-vector 10)}
       ;; #(#<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified>)
       ;; scheme@(guile-user)> {a[5] <- 7}
       ;; 7
       ;; scheme@(guile-user)> '{a[5] <- 7}
       ;; (<- ($bracket-apply$ a 5) 7)
       ;; scheme@(guile-user)> (<- (gaga a 5) 7)
       ;; (Guile version) Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index) , first argument : gaga is not $bracket-apply
       (unless (equal? (quote $bracket-apply$) (quote bracket-apply))
	       (error "Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index) , first argument "
		      (quote bracket-apply)
		      " is not $bracket-apply$."))
		       
       
       (let ((value expr)) ;; to avoid compute it twice
	 
	 ;; normal case
	 ;; {T[2] <- 4}
	 ;; {T[3] <- T[2]}
	 ;;(begin
	 ;;(display "<- : vector or array set! or hash-table set!") (newline)
	 (cond ({(vector? container) or (growable-vector? container)} (vector-set! container index value))
	       ((hash-table? container) (hash-table-set! container index value))
	       ((string? container) (string-set! container index value))
	       (else (array-set! container value index)));)
	 
	 value)))


    ;; multi dimensions array :  {a[2 4] <- 7}
    ;; $bracket-apply$ is from SRFI 105  bracket-apply is an argument of the macro
    ((_ (bracket-apply array index1 index2 ...) expr)
     
     (begin

       ;; add a checking
       (unless (equal? (quote $bracket-apply$) (quote bracket-apply)) 
	       (error "Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ array index1 index2 ...) , first argument "
		      (quote bracket-apply)
		      " is not $bracket-apply$."))
       
       (let ((value expr)) ;; to avoid compute it twice
	 
	 ;; normal case
	 ;;(begin
	 ;;(display "<- : multidimensional vector or array set!") (newline)
	 (if (vector? array)
	     (array-n-dim-set! array value index1 index2 ...)
	     (array-set! array value index1 index2 ...));) ;; Warning syntax is not the same as SRFI 25 (Racket)
	 
	 value)))

    ;; compact form but will display a warning: possibly wrong number of arguments to `vector-set!'
    ;; and if i remove ellipsis it is a severe error
    ;; ((_ (funct-or-macro array index ...) expr) (let ((value expr)
    ;; 						     (var (funct-or-macro array index ...)))
    ;; 						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    ;; 						     ;; normal case
    ;; 						     (if {(vector? array) or (growable-vector? array)}
    ;; 							 (vector-set! array index ... value)
    ;; 							 (array-set! array value index ...))
						     
    ;; 						     ;; rare case (to prevent any error)
    ;; 						     (set! var value)) ;; Wrong! this will only set the local 'var of (let (...
					     
    ;; 						 value))


    ;; possibly better version (TODO: test it) :
    ;; ((_ (funct-or-macro array index ...) expr) (let ((value expr)) ;; to avoid compute it twice
    ;; 				   
    ;; 						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    ;; 						     ;; normal case
    ;; 						     (if {(vector? array) or (growable-vector? array)}
    ;; 							 (vector-set! array index ... value)
    ;; 							 (array-set! array value index ...))
						     
    ;; 						     ;; rare case (to prevent any error)
    ;; 						     (set! (funct-or-macro array index ...) value)) ;; Wrong! set! only accept an identifier
					     
    ;; 						 value))

    ;; not sure this case will be usefull
    ;; ((_ (funct-or-macro arg ...) expr)
    ;;  (let ((var (funct-or-macro arg ...))
    ;; 	   (value expr)) ;; to avoid compute it twice
    ;;    (set! var value) ;; Wrong! this will only set the local 'var of (let (...
    ;;    var))

    
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

    ;; (declare I)
    ;; {I <- (make-array 0 4 4)}
    ;; #2((0 0 0 0)
    ;;    (0 0 0 0)
    ;;    (0 0 0 0)
    ;;    (0 0 0 0))
    ;;
    ;; {I[0 0] <- I[1 1] <- I[2 2] <- I[3 3] <- 1}
    ;; 1
    ;; 
    ;; I
    ;; #2((1 0 0 0)
    ;;    (0 1 0 0)
    ;;    (0 0 1 0)
    ;;    (0 0 0 1))

    ;; ((_ var var1 var2 ...)
    ;;  (<- var (<- var1 var2 ...)))

    ((_ var var1 ... expr) 
     (<- var (<- var1 ... expr))) 
     
    ))







;; (define-syntax ->
;;   (syntax-rules ()
;;     ;;  special form like : (-> ($bracket-apply$ T 3) ($bracket-apply$ T 4))
;;     ;; changé en (<- expr (funct-or-macro container index))
;;     ;;((_ expr (funct-or-macro container index)) {container[index] <- expr}  )
;;     ;; ((_ expr (funct-or-macro container index)) (<- (funct-or-macro container index) expr))
    
;;     ;; ;;((_ expr (funct-or-macro array index ...)) {array[index ...] <- expr} )
;;     ;; ((_ expr (funct-or-macro array index ...)) (<- (funct-or-macro array index ...) expr))
    
;;     ;; (-> 5 x)
;;     ;; note: it is short and infix but seems to work in all case indeed!
;;     ((_ expr var) {var <- expr})


;;     ;; (declare I)
;;     ;; {I <- (make-array 0 4 4)}
;;     ;; #2((0 0 0 0)
;;     ;;    (0 0 0 0)
;;     ;;    (0 0 0 0)
;;     ;;    (0 0 0 0))
;;     ;; {1 -> I[0 0] -> I[1 1] -> I[2 2] -> I[3 3]}
;;     ;; 1
;;     ;;
;;     ;; I
;;     ;; #2((1 0 0 0)
;;     ;;    (0 1 0 0)
;;     ;;    (0 0 1 0)
;;     ;;    (0 0 0 1))

;;     ((_ expr var var1 ...)
;;      (-> (-> expr var) var1 ...))

;;     ))





;; (define-syntax → ;; under Linux this symbol can be typed with the
;;   ;; combination of keys: Ctrl-Shift-u 2192 where 2192 is the unicode of right arrow

;;   (syntax-rules () 

;;     ;; note: it is short and infix but seems to work in all case indeed!
;;     ((_ expr var) {var <- expr})

;;     ((_ expr var var1 ...)
;;      (-> (-> expr var) var1 ...))
    
;;     ))


;; ;; Mac OS use CTRL+CMD+space to bring up the characters popover, then type in u + unicode and hit Enter to get it)

;; ;; (declare I)
;; ;; {I <- (make-array 0 2 2)}
;; ;; #2((0 0)
;; ;;    (0 0))
;; ;;
;; ;; {I[0 0] ← I[1 1] ← 1}
;; ;; 1
;; ;;
;; ;; I
;; ;; #2((1 0)
;; ;;    (0 1))

;; (define-syntax ← ;; under Linux this symbol can be typed with the
;;   ;; combination of keys: Ctrl-Shift-u 2190 where 2190 is the unicode of left arrow

;;   (syntax-rules ()
   
;;     ;; note: it is short and infix but seems to work in all case indeed!
;;     ((_ var expr) {var <- expr})

;;     ((_ var var1 var2 ...)
;;      (<- var (<- var1 var2 ...)))

;; ))


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
;; > {T[3] -> T[2]}
;; 7
;; > {T[2]}
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


     
   
