;; This file is part of Scheme+

;; Copyright 2024-2025 Damien MATTEI

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

;; Guile version


;; some optimizer procedures that parse square brackets arguments

;; was optimise-infix-slice.scm

;; version 17


(define-module (Scheme+ parse-square-brackets)
  
  #:use-module (Scheme+ def-function)
  #:use-module (Scheme+ declare)
  #:use-module (Scheme+ block)
  #:use-module (Scheme+ syntax)
  #:use-module (Scheme+ slice)
  #:use-module (Scheme+ infix-with-precedence-to-prefix)
  #:use-module (Scheme+ insert)
  #:use-module (Scheme+ infix-prefix)
   
  #:export (parse-square-brackets-arguments-lister-syntax)) ; end module header

  ;; split the expression using slice as separator
(def (parse-square-brackets-arguments args-brackets creator)

  ;;(display "parse-square-brackets-arguments : args-brackets=") (display args-brackets) (newline)

  (when (null? args-brackets) ; empty case
	(return args-brackets))

  (declare result partial-result) ; '() at beginning

  ;; note: example that needs to be between { } : {#(1 2 3 4)[{cinque minus 3}]}
 
  (def (psba args) ;; parse square brackets arguments ,note: it is a tail-recursive function (see end)

       ;; (display "psba : args=") (display args) (newline)
       ;; (display "psba : partial-result =") (display partial-result) (newline)

       
       (when (null? args) ; end case : we return the result after appending the last partial result
  	 ;;(display "before !*prec") (newline)
	 ;;(display "null args") (newline)

	 (when (not (null? partial-result)) ;; check infix expression exist really
	       ;;(display "not null") (newline)

	       ;;(display "psba 1 : partial-result =") (display partial-result) (newline)
	       (if (infix? partial-result) ; we test but we do not need to check all the syntax

		   ($> ; then
		    ;;(display "psba 1 : infix detected ") (newline)
		    
		    (insert-tail-set!  result (!*prec-generic-infix-parser partial-result
									   creator
									   ))) 
		   (begin
		     ;;(display "NO infix detected") (newline)
		     ;; append because the singleton (atom) needs to be exited from the list ,and append make it exited
		     (append-tail-set! result partial-result)))  ; not infix
	       
	       ;; (display "after !*prec") (newline)
	       )
	 
  	 ;; (display "psba when null args : result =") (display result) (newline)
  	 ;; (display "return-rec") (newline)
	 
  	 (return-rec result)) ;; return from all recursive calls, as it is tail recursive


       
       
       (define fst (car args)) ; get the first token in the infix expression

       ;;(display "fst=") (display fst) (newline)

       ;; test here for ':' ??? for multi-dim arrays , that will remove the use of { } in [ ]
       (if (datum=? slice fst) ; separator , end of infix expression

	   ;; we have some job to do at the end of an infix expression
  	   ($> ; then
  	    ;;(display "slice detected") (newline)
  	    ;;(display "psba : partial-result =") (display partial-result) (newline)
	    
  	    (when (not (null? partial-result)) ;; check infix expression exist really
  	      ;;(display "not null") (newline)
	      
	      ;;(display "psba : result =") (display result) (newline)
	      ;;(display "psba 2 : partial-result =") (display partial-result) (newline)
	      
	      ;; check it is in infix, not already prefixed (we are in scheme...) we test but we do not need to check all the syntax
  	      (if (infix? partial-result) ;;  operateurs quotés ou syntaxés !
		  
  		      (begin ; yes
  			;;(display "psba 2 : infix detected ") (newline)
			
  			(insert-tail-set! result (!*prec-generic-infix-parser partial-result
												 creator
		       									    	 ))) ;; convert to prefix and store the expression
		      ;; no
		      (begin
			;;(display "NO infix detected") (newline)
			;; append because the singleton (atom) needs to be exited from the list ,and append make it exited
  			(append-tail-set! result partial-result))) ; partial-result already atom, already prefix

	      ;;(display "psba : result =") (display result) (newline)
  	      (set! partial-result '())) ;; empty for the next possible portion between slice operator
	    
  	    (insert-tail-set! result fst)) ;; append the slice operator

	   
	   ;; else : not slice
	   ;; construct the list of the infix expression
  	   (insert-tail-set! partial-result fst)) ;; not a slice operator but append it


       
       ;; (display "psba : result=") (display result) (newline)
       ;; (display "psba 2 : partial-result=") (display partial-result) (newline)
       
       (psba (cdr args))) ;; end def psba , recurse (tail recursive) , continue with the rest of the infix token list

  

  ;;(display "parse-square-brackets-arguments : args-brackets=") (display args-brackets) (newline)

  (define rs  (psba args-brackets))

  ;;(display "parse-square-brackets-arguments : rs=") (display rs) (newline)

  ;;(cons #'list rs)
  ;; (cons 'list rs)
  rs
  ) ;; initial call




(define (parse-square-brackets-arguments-lister-syntax args-brackets)
  ;;(newline) (display "parse-square-brackets-arguments-lister-syntax : args-brackets=") (display args-brackets) (newline)
  (when (not (list? args-brackets))
    (display "parse-square-brackets-arguments-lister-syntax : WARNING , args-brackets is not a list, perheaps expander is not psyntax (Portable Syntax)") (newline)
    (display "parse-square-brackets-arguments-lister-syntax : args-brackets=") (display args-brackets) (newline))

  (parse-square-brackets-arguments args-brackets ;; generic procedure
				   (lambda (op a b) (list op a b)))) 
				
	
