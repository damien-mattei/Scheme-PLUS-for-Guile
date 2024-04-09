;; Sub Set Sum problem
;; Dynamic solution
;; Guile compatible

;; Copyright 2021-2024 Damien MATTEI

;; e-mail: damien.mattei@gmail.com

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


;;  for curly infix notation  put in your .guile:
;; (read-enable 'curly-infix)

;; ne marche pas:
;; export GUILE_AUTO_COMPILE=0

;; touch file.scm works if you change included files but not source file.scm

;; ../curly-infix2prefix4guile.scm   SssDyna+.scm > SssDyna.scm

;; (load "SssDyna.scm")


;; files below can be retrieved here: https://github.com/damien-mattei/library-FunctProg


(include "../rest.scm")

(use-modules (srfi srfi-1)) ;; for 'first' procedure

(use-modules (Scheme+))

(define (one? n)
  (= n 1))

;;(define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;;(define t-init 19836)


;; {L-init <+ '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
;; {t-init <+ 35267}
;; {ls <+ (length L-init)}
;; {dyna <+ (make-array 0 {ls + 1} {t-init + 1})}
;; {cpt <+ 0} ;;(define cpt 0)

(declare L-init t-init ls dyna cpt)

{L-init <- '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init <- 35267}
;;{t-init <- 21}

{ls <- (length L-init)}
{dyna <- (make-array 0 {ls + 1} {t-init + 1})}

(define (tf->12 b)
  (if b 1 2))

{cpt <- 0}



;; scheme@(guile-user)> (ssigma-dyna L-init t-init)
;; $2 = #t
;; scheme@(guile-user)> cpt
;; $3 = 147801
;; (define (ssigma-dyna L t)

;;   {cpt <- {cpt + 1}}
  
;;   ;;(display L) (display " ") (display t) (newline)
  
;;   (let*  [(ls (length L))
;; 	  (dyn (array-ref dyna ls t))]
    
;;     ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
;;     (one?
;;      (if (not (zero? dyn))
	
;; 	dyn
	
;; 	(array-ref-set! dyna ;; set the array but return the variable
;; 			(tf->12
;; 			 (if (null? L)
;; 			     #f
;; 			     (let [ (c (first L))
;; 				    (R (rest L)) ]
;; 			       (cond [ {c = t} #t ] ;; c is the solution
;; 				     [ {c > t} (ssigma-dyna R t) ] ;; c is to big to be a solution but can be an approximation
;; 				     ;; c < t at this point
;; 				     ;; c is part of the solution or his approximation
;; 				     ;; or c is not part of solution or his approximation
;; 				     [ else {(ssigma-dyna R {t - c}) or (ssigma-dyna R t)} ] ))))
;; 			ls
;; 			t  )))))


;; scheme@(guile-user)> (ssigma-dyna-local L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
;; (define (ssigma-dyna-local L t)

;;   {cpt ← {cpt + 1}} ;; cpt is defined at toplevel
  
;;   ;;(display L) (display " ") (display t) (newline)
  
;;   (local  [ ls (length L)
;; 	    dyn {dyna[ls t]} ]
    
;;     ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
;;     (one?
;;      (if (not (zero? dyn))
	
;; 	dyn

;; 	;; set the array but return the variable
;; 	{ dyna[ls t] ← (tf->12
;; 			  (if (null? L)
;; 			      #f
;; 			      (local [ c (first L)
;; 				       R (rest L) ]
;; 				     (cond [ {c = t} #t ] ;; c is the solution
;; 					   [ {c > t} (ssigma-dyna-local R t) ] ;; c is to big to be a solution but can be an approximation
;; 					   ;; c < t at this point
;; 					   ;; c is part of the solution or his approximation
;; 					   ;; or c is not part of solution or his approximation
;; 					   [ else {(ssigma-dyna-local R {t - c}) or (ssigma-dyna-local R t)} ] )))) } ))))

;; scheme@(guile-user)> (ssigma-dyna-define-anywhere L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-dyna-define-anywhere L t)
  
  {cpt <- cpt + 1} ;; cpt is defined at toplevel
  
  ;;(display L) (display " ") (display t) (newline)

  ;;(def ls (length L))
  ;;(def dyn {dyna[ls t]})

  {ls <+ (length L)}

  ;;(display "ls=") (display ls) (display " ") (display "t=") (display t) (newline)
  
  {dyn <+ dyna[ls t]}
  
  (def c)
  (def R)

  
  
  ;; TODO: write this code simplier
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  (one?
    (if (not (zero? dyn))
	
	dyn
	
	;; set the array but return the variable
	($>
	 { dyna[ls t] <- (tf->12
			  (if (null? L)
			      #f
			      ($> ;;(display "assignment") (newline)
				{c <- (first L)}
				{R <- (rest L)}
				(cond [ {c = t} #t ] ;; c is the solution
				      [ {c > t} (ssigma-dyna-define-anywhere R t) ] ;; c is to big to be a solution but can be an approximation
				      ;; c < t at this point
				      ;; c is part of the solution or his approximation
				      ;; or c is not part of solution or his approximation
				      [ else {(ssigma-dyna-define-anywhere R {t - c}) or (ssigma-dyna-define-anywhere R t)} ] )))) }
	 { dyna[ls t] })
	 )))



;; scheme@(guile-user)> (ssigma-dyna-def L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-dyna-def L t)

  (display L) (newline)
  (display t) (newline)
  (newline)
 
  
  {cpt <- cpt + 1} ;; cpt is defined at toplevel
  
  (def (ls dyn)) ;; declare multiple variables 
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  ;; declare one variable at a time
  (def c)
  (def R)

  ;; TODO: write this code simplier
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  (one? ; 1: solution found ?
   (if (not (zero? dyn)) ; we already know the solution
       
       dyn ; 1: solution found, 2: no solution

       ;; 0: unknown solution
       ;; set the array but return the variable
       ($>
	{ dyna[ls t] <- (tf->12
			 (if (null? L)
			    #f
			    ;; TODO: rename $ which is already used by SRFI-9 record utiliser  § ou | (option shift L sur mac)
			    ($> ;;(display "assignment") (newline)
			     {c <- (first L)}
			     {R <- (rest L)}
			     (cond [ {c = t} #t ] ;; c is the solution
				   [ {c > t} (ssigma-dyna-def R t) ] ;; c is to big to be a solution but can be an approximation
				   ;; c < t at this point
				   ;; c is part of the solution or his approximation
				   ;; or c is not part of solution or his approximation
				   [ else {(ssigma-dyna-def R {t - c}) or (ssigma-dyna-def R t)} ] )))) }
	{ dyna[ls t] }))))







;; scheme@(guile-user)> (ssigma-proto L-init t-init)
;;  = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-proto L t)

  (set! cpt {cpt + 1})
 
  (define ls (length L))
  (define dyn (array-ref dyna ls t))
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution

  (cond [(not (zero? dyn)) (one? dyn)]
	[(null? L) (array-set! dyna 2 ls t) #f] ;; return #f
	
	[else (let [(c (first L))]
		
		(if {c = t} ;; c is the solution
		      
		    (begin
		      (array-set! dyna 1 ls t)
		      #t)  ;; return #t

		    ;; else
		    (let [(R (rest L))]
		      
		      (if {c > t}   ;; continue searching a solution in the rest
			    
			  (let [(s (ssigma-proto R t))]
			    (array-set! dyna
					(tf->12 s)
					ls t)
			      
			    s) ;; return s
			
			  ;; else
			  ;; c < t at this point
			  ;; c is part of the solution or his approximation
			  ;; or c is not part of solution
			  (let [(s {(ssigma-proto R {t - c}) or (ssigma-proto R t)})]
			    (array-set! dyna (tf->12 s)
					ls t)
			    s)))))
	      ] ))

;; (ssigma-proto-condx  L-init t-init)
;; ... 
;;  = #t
(define (ssigma-proto-condx L t)

  (set! cpt {cpt + 1})

  (define ls (length L))
  (define dyn (array-ref dyna ls t))


  (display L) (newline)
  (display t) (newline)
 
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [(not (zero? dyn)) (one? dyn)]
	 [(null? L) (array-set! dyna 2 ls t) #f] ;; return #f
	 
	 [exec (define c (first L))]
	 
	 ;; c is the solution
	 [{c = t} (array-set! dyna 1 ls t) #t]  ;; return #t
	 
	 [exec (define R (rest L))]
	 
	 ;; continue searching a solution in the rest
	 [{c > t} (define s (ssigma-proto-condx R t))
	  (array-set! dyna
		      (tf->12 s)
		      ls t)
	  s] ;; return s
			
	 ;; else :
	 ;; c < t at this point
	 ;; c is part of the solution or his approximation
	 ;; or c is not part of solution
	 [else (define s {(ssigma-proto-condx R {t - c}) or (ssigma-proto-condx R t)})
	       (array-set! dyna (tf->12 s)
			   ls t)
	       s]))





;; (subset-sum-dyna  L-init t-init)
;; #t ;; there exist a solution

(def (subset-sum-dyna L t)

  (declare ls dyn) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  (if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)
      (return (one? dyn)))

  (when (null? L)
    {dyna[ls t] <- 2}
    (return #f))

  {c <+ (first L)}

  (when {c = t}  ;; c is the solution
    {dyna[ls t] <- 1}
    (return #t))

  {R <+ (rest L)} ;; continue searching a solution in the rest

  (declare s)
  (if {c > t}  ;; c is to big to be a solution
    {s <- (subset-sum-dyna R t)}
    ;; c is part of the solution or c is not part of solution
    {s <- (subset-sum-dyna R {t - c}) or (subset-sum-dyna R t)})

  {dyna[ls t] <- (tf->12 s)}
  s) ;; return boolean value


;; scheme@(guile-user)> (subset-sum-dynamic L-init t-init)
;; $1 = #t
(def (subset-sum-dynamic L t)

  (declare ls dyn c R s) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]} ;; dyna is a toplevel defined array

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  (if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)
      (return (one? dyn)))

  (when (null? L)
    {dyna[ls t] <- 2}
    (return #f))

  {c <- (first L)}

  (when {c = t}  ;; c is the solution
    {dyna[ls t] <- 1}
    (return #t))

  {R <- (rest L)} ;; continue searching a solution in the rest

  (if {c > t}  ;; c is to big to be a solution
    {s <- (subset-sum-dynamic R t)}
    ;; c is part of the solution or c is not part of solution
    {s <- (subset-sum-dynamic R {t - c}) or (subset-sum-dynamic R t)})

  {dyna[ls t] <- (tf->12 s)}
  s) ;; return boolean value
  


;; scheme@(guile-user)> (subset-sum-dynamic-new-syntax L-init t-init)
;; $1 = #t
(def (subset-sum-dynamic-new-syntax L t)

  (declare ls dyn c R s) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]} ;; dyna is a toplevel defined array

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  (if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)
      (return (one? dyn)))

  (if (null? L) then
      {dyna[ls t] <- 2}
      (return #f))

  {c <- (first L)}

  (if {c = t} then ;; c is the solution
      {dyna[ls t] <- 1}
      (return #t))

  {R <- (rest L)} ;; continue searching a solution in the rest

  (if {c > t}  then ;; c is to big to be a solution
      {s <- (subset-sum-dynamic-new-syntax R t)}
   else
      ;; c is part of the solution or c is not part of solution
      {s <- (subset-sum-dynamic-new-syntax R {t - c}) or (subset-sum-dynamic-new-syntax R t)})

  {dyna[ls t] <- (tf->12 s)}
  s) ;; return boolean value


;;(subset-sum-condx L-init t-init)
;;$1 = #t

(define (subset-sum-condx L t)

  (declare ls dyn) ;; declare multiple variables or use <+ instead of <- below

  {ls <- (length L)}
  {dyn <- dyna[ls t]}
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <+ (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <+ (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <+ (subset-sum-condx R t)}
	          {dyna[ls t] <- (tf->12 s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <+ (subset-sum-condx R {t - c}) or (subset-sum-condx R t)}
	       {dyna[ls t] <- (tf->12 s)}
	       s])) ;; return boolean value


;;  (subset-sum L-init t-init)
;;$1 = #t

(define (subset-sum L t)

  {ls <+ (length L)}
  {dyn <+ dyna[ls t]}

  {cpt <- cpt + 1} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <+ (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <+ (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <+ (subset-sum R t)}
	          {dyna[ls t] <- (tf->12 s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <+ (subset-sum R {t - c}) or (subset-sum R t)}
	       {dyna[ls t] <- (tf->12 s)}
	       s])) ;; return boolean value



;; scheme@(guile-user)> (subset-sum-dec L-init t-init)
;;$1 = #t
(define (subset-sum-dec L t) ; declaration on top

  (declare ls dyn c R s)
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  {cpt <- cpt + 1} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-dec R t)}
	          {dyna[ls t] <- (tf->12 s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <- (subset-sum-dec R {t - c}) or (subset-sum-dec R t)}
	       {dyna[ls t] <- (tf->12 s)}
	       s])) ;; return boolean value


;; > (subset-sum-value L-init t-init)
;; '(1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)
;; > t-init
;; 35267
;; > (+ 1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)
;; 35267

;; > (subset-sum-value  '(17 24 45 64 197 256 323 540 723 889 915 1040 1041 1093 1111 1284 1344 1520 2027 2500 2734 3000 3267 4285 5027) t-init)
;; #f
(define (subset-sum-value L t)

  ;; declaration on top
  (declare ls dyn c R s) ; c: current/first number of list, R: rest of list,s: solution if found
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  {cpt <- cpt + 1} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, true (example: list): solution found, false: #f: no solution
  
  (condx [(not (equal? dyn 0)) dyn] ; the solution if found (already computed)
	 [(null? L) {dyna[ls t] <- #f}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {s <- dyna[ls t] <- (list c)} s]  ;; return a true value ( the list containing the solution)
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-value R t)}
	          {dyna[ls t] <- s}
		  s] ;; return boolean value which is the solution when true/found.
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 
	 [exec {s <- (subset-sum-value R {t - c})}]
	 [s {s <- dyna[ls t] <- (cons c s)} s] ; case c is part of a solution
	       
	 [exec {s <- (subset-sum-value R t)}] ; at this point c is not part of a solution
	 [else {dyna[ls t] <- s} s])) ;; return boolean value which is the solution when true/found.
