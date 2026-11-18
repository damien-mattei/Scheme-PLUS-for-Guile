;; Translated from Scheme+ for Racket (nfx.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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


(define-module (Scheme+ nfx)
  #:use-module (Scheme+ racket-compat))


	

  (use-modules
	(Scheme+ infix-with-precedence-to-prefix)
	(Scheme+ infix-with-precedence-to-prefix)
	(Scheme+ exponentiation)
	(Scheme+ multiply)
	(Scheme+ def-function))

  
  
;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > ($nfx$ 3 * 5 + 2)
;; $nfx$ : parsed-args={.#<syntax:15-interactions from an unsaved editor:3:15 +> {.#<syntax:15-interactions from an unsaved editor:3:11 *> .#<syntax:15-interactions from an unsaved editor:3:9 3> .#<syntax:15-interactions from an unsaved editor:3:13 5>} .#<syntax:15-interactions from an unsaved editor:3:17 2>}
;; 17

;; partly DEPRECATED : used only with syntax transformers only mode
(define-syntax $nfx$

  (lambda (stx)
    
    (syntax-case stx ()


      ;; ($nfx$ 3)
      ;; 3

      ;; ($nfx$ (3 + 2))
      ;; 5

      (($nfx$ expr) 

       (with-syntax
			 
	   ((parsed-args

	     (begin
	       
	       ;;(display "$nfx$: #'(expr)=") (display #'(expr)) (newline)
	       ;;(display "$nfx$: (syntax->list #'(expr))=") (display (syntax->list #'(expr))) (newline)

	       ;; the one below is currently in use with the code
	       ;;(display "$nfx$: #'expr=") (display #'expr) (newline)
	       ;; (newline)

	       ;;(car ;  probably because the result will be encapsuled in a list !
		;; apply operator precedence rules
		(!*prec-generic-infix-parser ;; (list->mlist
		 ;;(syntax->list ;; no need in R6RS ???
		 ;;#'(expr));))
		 #'expr
		 ;;) ; end syntax-list
		 (lambda (op a b) (list op a b))
		 )
		;;) ; car

	       ;; should work
	       ;;(recall-infix-parser #'expr
				    ;;;;infix-operators-lst-for-parser-syntax
				    ;;(lambda (op a b) (list op a b)))
		) ; begin
	     ))

	 ;;(display "$nfx$ expr : parsed-args=") (display #'parsed-args) (newline)
	 ;; (newline)
	 #'parsed-args))

      

      ;; (define x  - 4)
      ;; x
      ;; -4
      (($nfx$ op1 e1) ; deprecated note : with 2 arg !*-prec-generic-infix-parser could not work , so we use recall-infix-parser

       (with-syntax
			 
	   ((parsed-args

	     ;(car ;  probably because the result will be encapsuled in a list !
	     
	      ;; (recall-infix-parser (superscript-operator-loop (begin-operators+-
	      ;; 						       (syntax->list
	      ;; 							#'(op1 e1)
	      ;; 						       	)
	      ;; 						       )) ; can not set (list op1 e1) ... see error , try:
	      ;; 			  ;;(list #'op1 #'e1))) ; works too 
	     ;; 			   (lambda (op a b) (list op a b)))
	      ;;) ;  end car

	      ;;(car ;  probably because the result will be encapsuled in a list !
	     (!*prec-generic-infix-parser (list #'op1 #'e1)
					;(syntax->list #'(op1 e1))
					  (lambda (op a b) (list op a b))
					  )
	       ;) ; close car

	      ))
	 ;;(display "$nfx$ op1 e1 : parsed-args=") (display #'parsed-args) (newline)
	 #'parsed-args))

      
       
      ;; (($nfx$ e1 op1 e2) #'(op1 e1 e2))
      ;; (($nfx$ e1 op1 e2 op2)

      ;;  (error "$nfx$ : called with 4 args (should be 3 or 5 or more) :" #'e1 #'op1 #'e2 #'op2))

      ;; note that ,in the normal case,to have $nfx$ called you need at minimum to have 2 different operator causing an operator precedence question
      ;; and then at least those 2 operators must be between operands each, so there is a need for 3 operand
      ;; the syntax then looks like this : e1 op1 e2 op2 e3 ..., example : 3 * 4 + 2
      ;;(($nfx$ e1 op1 e2 op2 e3 op ...) ; note: i add op because in scheme op ... could be non existent
      (($nfx$ e1 op1 e2 op ...)
       
	 (with-syntax ;; let
			 
	     ((parsed-args

	      ; (begin
		 ;;(display "$nfx$: #'(e1 op1 e2 op ...)=") (display #'(e1 op1 e2 op ...)) (newline)
		 ;;(display "$nfx$: (syntax->list #'(e1 op1 e2 op ...))=") (display (syntax->list #'(e1 op1 e2 op ...))) (newline)
    	 		
		 ;; ;; pre-check we have an infix expression because parser can not do it
		 ;; (when (not (infix-simple? ;;(list #'e1 #'op1 #'e2 #'op2 #'e3 #'op ...) ; ellipsis impossible
		 ;; 		    ;;#'(e1 op1 e2 op2 e3 op ...)
		 ;; 	            ;;(syntax->list #'(e1 op1 e2 op2 e3 op ...))
		 ;; 	     (syntax->list #'(e1 op1 e2 op ...))
		 ;; 	     operators-lst-syntax))
		 ;;       ;; (error "$nfx$ : arguments do not form an infix expression : here is #'(e1 op1 e2 op2 e3 op ...) and (syntax->list #'(e1 op1 e2 op2 e3 op ...)) for debug:"
		 ;;       (error "$nfx$ : arguments do not form an infix expression : here is #'(e1 op1 e2 op ...) and (syntax->list #'(e1 op1 e2 op ...)) for debug:"
		 ;; 	  ;;#'(e1 op1 e2 op2 e3 op ...)
		 ;; 	  #'(e1 op1 e2 op ...)
		 ;; 	  ;;(syntax->list #'(e1 op1 e2 op2 e3 op ...))))
		 ;; 	  (syntax->list #'(e1 op1 e2 op ...))))

		 
	       (!*prec-generic-infix-parser (syntax->list #'(e1 op1 e2 op ...))
					    (lambda (op a b) (list op a b)))
			    

	       ;; ) ; end begin
	       ))
	      
	   ;;(display "$nfx$ : parsed-args=") (display #'parsed-args) (newline)
	   #'parsed-args)))))






;; this version is mainly call from SRFI-105
(def (nfx f . r)
    
  ;; 1 argument

  ;; {list(1 2 3)}
  ;; nfx.rkt : nfx : r null , f = (list 1 2 3)
  ;; (list 1 2 3)
  ;; '(1 2 3)

  ;; {(list 1 2 3)}
  ;; nfx.rkt : nfx : r null , f = (list 1 2 3)
  ;; (list 1 2 3)
  ;; '(1 2 3)
  ;; #<eof>

  ;; {#(1 2 3 4)[2]}
  ;; nfx.rkt : nfx : r null , f = ($bracket-apply$ #(1 2 3 4) 2)
  ;; ($bracket-apply$ #(1 2 3 4) 2)
  ;; parse-square-brackets-arguments : args-brackets=(.#<syntax 2>)
  ;; 3
  ;; #<eof>
  (when (null? r)
    ;;(display "nfx.rkt : nfx : r null , f = ") (display f) (newline)
    (return (!*prec-generic-infix-parser-rec-prepare  #;!*prec-generic-infix-parser-rec f ; usefull also for neoteric expressions , see above example
					     (lambda (op a b) (list op a b)))))

  ;; 2 arguments or more
  ;;(display "nfx.rkt : nfx : (cons f r) = ") (display (cons f r)) (newline)

  ;; changed this one for runtime too
  (!*prec-generic-infix-parser-prepare-runtime (cons f r)
					       (lambda (op a b) (list op a b))))





;; this is a version of $nfx$ that call the recursive version of infix/prefix parser of expressions, this is usefull for define+/define
(define-syntax $nfx$-rec

  (lambda (stx)
    
    (syntax-case stx ()

      (($nfx$-rec expr) 

       (with-syntax
			 
	   ((parsed-args

	     (begin
	       
	       ;;(display "$nfx$-rec: #'(expr)=") (display #'(expr)) (newline)
	       ;;(display "$nfx$-rec: (syntax->list #'(expr))=") (display (syntax->list #'(expr))) (newline)
	       ;; (display "$nfx$-rec: #'expr=") (display #'expr) (newline)
	       ;; (newline)

	       ;;(car ;  probably because the result will be encapsuled in a list !
		;; apply operator precedence rules
		(!*prec-generic-infix-parser-rec-prepare ;; (list->mlist
		 ;;(syntax->list ;; no need in R6RS ???
		 ;;#'(expr));))
		 #'expr
		 ;;) ; end syntax-list
		 (lambda (op a b) (list op a b))
		 )
		;;) ; car
	      
		) ; begin
	     ))

	 ;; (display "$nfx$-rec expr : parsed-args=") (display #'parsed-args) (newline)
	 ;; (newline)
	 #'parsed-args))

      

      ;; (define x  - 4)
      ;; x
      ;; -4
      (($nfx$-rec op1 e1)

       (with-syntax
			 
	   ((parsed-args

	      ;;(car ;  probably because the result will be encapsuled in a list !
	     (!*prec-generic-infix-parser-rec-prepare ;(list #'op1 #'e1)
                                        ; TODO continue checking this: (instead of above)
					  (syntax->list #'(op1 e1))
					  (lambda (op a b) (list op a b))
					  )
	       ;) ; close car

	      ))
	 ;;(display "$nfx$-rec op1 e1 : parsed-args=") (display #'parsed-args) (newline)
	 #'parsed-args))

          
      (($nfx$-rec e1 op1 e2 op ...)
       
	 (with-syntax ;; let
			 
	     ((parsed-args

	      ; (begin
		 ;;(display "$nfx$-rec: #'(e1 op1 e2 op ...)=") (display #'(e1 op1 e2 op ...)) (newline)
		 ;;(display "$nfx$-rec: (syntax->list #'(e1 op1 e2 op ...))=") (display (syntax->list #'(e1 op1 e2 op ...))) (newline)
    	 		
		 ;; 	  #'(e1 op1 e2 op ...)
		 ;; 	  ;;(syntax->list #'(e1 op1 e2 op2 e3 op ...))))
		 ;; 	  (syntax->list #'(e1 op1 e2 op ...))))

		 
	       (!*prec-generic-infix-parser-rec-prepare (syntax->list #'(e1 op1 e2 op ...))
						(lambda (op a b) (list op a b))
						)
			    

	       ;; ) ; end begin
	       ))
	      
	   ;;(display "$nfx$-rec : parsed-args=") (display #'parsed-args) (newline)
	   #'parsed-args)))))






(export
	$nfx$
	nfx
	$nfx$-rec)
 ; end module


;; (define (foo) 7)

;; (foo)
;; 7

;; (define x  - (foo))

;; x
;; -7

;; (define (bar) -)

;; (bar)
;; #<procedure:->

;; (define z  (bar) (foo))
;; z
;; -7


;; from https://course.dyalog.com/dfns-and-assignment/ :

;; > {2 (lambda (alpha omega) alpha) 3}


;; ($nfx$ 2 (lambda (alpha omega) alpha) 3)
;; 2


;; #<eof>
;; > {2 (lambda (alpha omega) omega) 3}


;; ($nfx$ 2 (lambda (alpha omega) omega) 3)
;; 3


;; > {(lambda (alpha omega) omega) 3}


;; ((lambda (alpha omega) omega) 3)
;; . . ../../../../Applications/Racket v8.14/collects/racket/private/kw.rkt:1260:25: arity mismatch;
;;  the expected number of arguments does not match the given number
;;   expected: 2
;;   given: 1

;; > {2 (lambda (alpha omega) omega)}


;; (2 (lambda (alpha omega) omega))
;; . . ../../../../Applications/Racket v8.14/collects/racket/private/kw.rkt:1260:25: application: not a procedure;
;;  expected a procedure that can be applied to arguments
;;   given: 2
;; >

;; > {2 (lambda (alpha omega) omega) "apl"}


;; ($nfx$ 2 (lambda (alpha omega) omega) "apl")
;; "apl"


;; #<eof>
;; > 



;; https://course.dyalog.com/basic-syntax-and-arithmetic/
;; > (define-overload-existing-operator -)


;; (define-overload-existing-operator -)


;; #<eof>
;; > (define (negate-list L) (map - L))


;; (define (negate-list L) (map - L))


;; #<eof>
;; > (negate-list '(1 -2 3))


;; (negate-list '(1 -2 3))
;; '(-1 2 -3)


;; #<eof>
;; > 	(overload-existing-operator - negate-list (list?))


;; (overload-existing-operator - negate-list (list?))


;; #<eof>
;; > (- 3)


;; (- 3)
;; -3


;; #<eof>
;; > (- '(1 -2 3))


;; (- '(1 -2 3))
;; '(-1 2 -3)


;; #<eof>

;;(define n 7)
;; {1 * (- 2 · (n - 4))}
;; -6


;;{- 2 * (- 2 · (n - 4))}
;; 12

;; {- 2 * (- 2 · (- n 4))}
;; 12
