;; Translated from Scheme+ for Racket (plus-minus-parser.rkt) for Guile.

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



(define-module (Scheme+ plus-minus-parser)
  #:use-module (Scheme+ racket-compat))

	

	(use-modules
	(Scheme+ def-function)
	(Scheme+ operators)
	(Scheme+ condx))


(define previous-operator '-)

;; {5 -  - 2}
;; !*prec-generic-infix-parser : rv : deep-terms:(.#<syntax 5> .#<syntax -> (- .#<syntax 2>))
;; $nfx$ : parsed-args=.#<syntax (- 5 (- 2))>
;; 7

;;  {3 * 5 -  - 2}
;; 17

;; {5 - - - - + - - 2}
;; 7

;;(define a 7)
;;(define b 3)

;; {a * - b}
;; -21

;;{- 2 · 3}
;; -6


;; main entry of parsing +- (see parser+-.odg or jpeg image for the schema of automaton)
;; we want to parse infix expression like: 3 + - 4 and transform it 3 + (- 4)
;; like other languages, like Python do it for 3+-4 or others, bad but valid syntax as 3+---4 ,etc ....
(def (begin-operators+- lst acc)
  ;; (display "begin-operators+- : lst=") (display lst) (newline)
  ;; (display "begin-operators+- : acc=") (display acc) (newline)

  (when (null? lst)
	(return lst))
  
  (define elem (car lst))
  
  (cond ((ADD-op? elem)  ; we drop it from the resulting list
	 (begin-operators+- (cdr lst) acc))
	((MINUS-op? elem) ; we drop it from the resulting infix list (but will be possibly integrated in a prefixed sub sexpr)
	 (set! previous-operator '-)
	 (state-1-loop-over+- (cdr lst) acc)) ; go to another automaton state
	((NO-OP? elem) ; should be a general sexpr
	  ; we keep it in the resulting list as this is the first sexpr
	 (state-3-parse-operators (cdr lst)
				  (append acc (list elem))))
	(else
	 (display "begin-operators+- : elem=") (display elem) (newline)
	 (display "Error in begin-operators+- : ")
	 (error+- lst))))


	
(def (state-3-parse-operators lst acc)
  ;; (display "state-3-parse-operators : lst=") (display lst) (newline)
  ;; (display "state-3-parse-operators : acc=") (display acc) (newline)

  (when (null? lst)
	(if (null? (cdr acc))
		   (return (car acc))
		   (return acc)))
  
  (define elem (car lst))
  (if (operator-symbol-or-syntax? elem)
      (begin
	(set! previous-operator elem)
	;; we keep it in the resulting list
	(state-2-signs (cdr lst)
		       (append acc (list elem)))) ; and change to another automaton state
      ;;(error "Error parsing +- : state-3-parse-operators : found something else than an operator ,see element and list :" elem lst)))

      ;; happens for example with: chaos+.rkt : ((cos (x + y)) * (sin (x - y)))
      ;; we keep it in the resulting list
      (state-3-parse-operators (cdr lst)
			       (append acc (list elem))))) ; and stay in the same automaton state



(define (error+- lst)
  (error "Error parsing +- : the sequence of operators has no mathematic signification, in the provided list : " lst))




;; we have find an operator or more (signs) and we check possibly another one following (signs)
(def (state-2-signs lst acc)

  ;; (display "state-2-signs : lst=") (display lst) (newline)
  ;; (display "state-2-signs : acc=") (display acc) (newline)
  
  (when (null? lst)
    (return acc))

  (define elem (car lst))
  
  (cond ((ADD-op? elem) ; we drop it from the resulting list
	 (state-2-signs (cdr lst) acc)) ; and stay in the same automaton state
	
	((NO-OP? elem) ; should be a general sexpr
	 ;; we keep it in the resulting list
	 (state-3-parse-operators (cdr lst)
				  (append acc (list elem))))
	
	((MINUS-op? elem) ; we drop it from the resulting infix list (but will be possibly integrated in a prefixed sub sexpr)
	 (set! previous-operator '-) ; TODO check it (begin-operators+- '(x := - 2 * 3 + 5) '()) ---> '(x := (- (2 * 3)) + 5)
	 (state-1-loop-over+- (cdr lst) acc)) ; go to another automaton state
	
	(else
	 (display "state-2-signs : elem=") (display elem) (newline)
	 (display "Error in state-2-signs : ")
	 (error+- lst))))



;; loop over the + - operators to find the final sub sexpr to create in the infix sequence
(def (state-1-loop-over+- lst acc)

  ;; (display "state-1-loop-over+- : lst=") (display lst) (newline)
  ;; (display "state-1-loop-over+- : acc=") (display acc) (newline)
  
  (when (null? lst)
	(return acc))
  
  (define elem (car lst))
  (cond ((MINUS-op? elem) ; we drop it because we have - - resulting in + which also can be dropped
	 (state-2-signs (cdr lst) acc)) ; and we go back to the previous state of the automaton
	
	((ADD-op? elem) ; we drop it from the resulting list
	 (state-1-loop-over+- (cdr lst) acc)) ; and stay in the same automaton state
	
	((NO-OP? elem) ; should be a general sexpr or more elements , so here we change his sign
	 ;;(display -) (newline)

	 ;; (display "state-1-loop-over+- : calling minus-expression with : ") (newline)
	 ;; (display (list elem)) (display "   ") (display (cdr lst)) (newline)
	 
	 (define-values (minus-lst rest-lst) (minus-expression (list elem) (cdr lst))) ; accumulator is (elem ...)
	 
	 ;(display "state-1-loop-over+- : minus-lst =") (display minus-lst) (newline)
	 ;(display "state-1-loop-over+- : rest-lst =") (display rest-lst) (newline)
	 
	 ;; (cons
	 ;;  (list '- ;(syntax -) ; '- : possible bug if we manipulate syntax object this should be (syntax -) and in R6RS could be also different
	 ;; 	elem)
	 ;;  (state-3-parse-operators (cdr lst)))) ; continue parsing with the inital automaton state

	 (if (null? rest-lst)
	     ;; example: (cons 'a (cons '* (list (list '- '(b + c)))))
	     ;;          '(a * (- (b + c)))

	     ;; it is the end of all recursions here
	     (if (null? acc)
		 (list '- minus-lst)
		 (append acc
			 (list
			  (list '- minus-lst))))

	     ;; in other case recursion continue
	     (state-3-parse-operators rest-lst
				      (append acc
					      (list
					       (list '- minus-lst)))))) ; continue parsing with the inital automaton state
	 
	(else
	 (display "Error in state-1-loop-over+- : ")
	 (error+- lst))))


;; find the minus expression and the rest
;; as we evaluate from left to right we go from beginning of the list to the end
(define (minus-expression minus-lst rest-lst)
  ;;(display "minus-expression : minus-lst =") (display minus-lst) (newline)
  ;;(display "minus-expression : rest-lst =") (display rest-lst) (newline)
  
  (condx ((null? rest-lst)
	  (if (null? (cdr minus-lst)) ; a single element list (sexpr)
	      (values (car minus-lst) rest-lst)
	      (values minus-lst rest-lst)))
	 
	 ;; after this point rest of minus-lst is not null
	 (exec
	  (define elem (car rest-lst)))
	 ((or (NO-OP? elem) 
	      ;;(strict-precedence-over-minus? elem))
              ; (operator-precedence>? elem previous-operator)
	      ; above commented because of fail:
              ; {x := 2}
              ; {y := 3}

              ;; > {- x * - y}
              ;; REPL Curly Infix:

              ;; (* (- x) (- y))

              ;; Parsed annotations. :
              ;; (* (- x) (- y))

              ;; 6

              ;; > {- 2 ** 4}
              ;; REPL Curly Infix:

              ;; (- (** 2 4))

              ;; Parsed annotations. :
              ;; (- (** 2 4))

              ;; -16
              ;; > {- 3 % 2}
              ;; REPL Curly Infix:

              ;; (% (- 3) 2)

              ;; Parsed annotations. :
              ;; (% (- 3) 2)

              ;; 1
              ;; > {- 3 & 2}
              ;; REPL Curly Infix:

              ;; (& (- 3) 2)

              ;; Parsed annotations. :
              ;; (& (- 3) 2)

              ;; 0
             
              (EXPONENTIATION-op? elem)
	      #;(and (EXPONENTIATION-op? previous-operator)
		   (EXPONENTIATION-op? elem))) ; preserve that exponentiation is right associative
	  ;;(display "minus-expression : elem =") (display elem) (newline)
	  ;;(display "minus-expression : (NO-OP? elem) =") (display (NO-OP? elem) ) (newline)
	  ;;(display "minus-expression : append") (newline)
	  (minus-expression (append minus-lst
				    (list elem)) ; put the element at the end of the list
			    (cdr rest-lst)))
	 
	 (else
	  ;;(display "minus-expression : else") (newline)
	  (if (null? (cdr minus-lst))
	      (values (car minus-lst) rest-lst)
	      (values minus-lst rest-lst)))))






(export
	begin-operators+-)
 ; end module


;; > {3 + 2 ** - 0.5 * 3}
;; state-1-loop-over+- : minus-lst =0.5
;; state-1-loop-over+- : rest-lst =(* 3)
;; REPL Curly Infix:

;; (+ 3 (* (** 2 (- 0.5)) 3))

;; Parsed annotations. :
;; (+ 3 (* (** 2 (- 0.5)) 3))

;; 5.121320343559643

;; {2 ** -0.5 * 2}
;; !*-generic-infix-parser : terms = ((.#<syntax *> (.#<syntax **> .#<syntax 2> .#<syntax -0.5>) .#<syntax 2>))
;; $nfx$ : parsed-args=.#<syntax (* (** 2 -0.5) 2)>
;; 1.4142135623730951

;; {2 ** (-0.5 * 2)}
;; 0.5


;; {- 2 + 3}
;; 1



;; > {4 2 + 3}
;; REPL Curly Infix:

;; (!*prec-generic-infix-parser-runtime (list 4 2 + 3) (lambda (op a b) (op a b)))

;; Parsed annotations. :
;; (!*prec-generic-infix-parser-runtime (list 4 2 + 3) (lambda (op a b) (op a b)))

;; . . Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/infix-with-precedence-to-prefix.rkt:236:4: infix-with-precedence-to-prefix.rkt : pre-check-!*-generic-infix-parser : not a canonical infix expression:  '(4 2 #<procedure:+> 3)
;; > 

;; {2 + - 3}
;; -1


;; {- - + - 3 + 2 * 5}
;; 7


;; {- + - - 2 ** 4}
;; -16

;; {- 2 ** 4}
;; -16

;; {2 + - - - 3 * 5 + 2}
;; -11


;; {- 2 ** 3}
;; -8

;; {- + - - 2 ** 4}
;; -16


;; {2 ** - 0.5 * 3}
;; state-1-loop-over+- : minus-lst =0.5
;; state-1-loop-over+- : rest-lst =(* 3)
;; REPL Curly Infix:

;; (* (** 2 (- 0.5)) 3)

;; Parsed annotations. :
;; (* (** 2 (- 0.5)) 3)

;; 2.121320343559643


;; {3 + 2 ** - 0.5 * 3}
;; 5.121320343559643

;; {2 ** - 0.5 * 2}
;; 1.4142135623730951

;; {3 * - 2 ** 4 + 1}
;; -47

;; {3 * - 2 ** 4 ** 2}
;; -196608

;; {3 * - (2 ** 4 + 1)}
;; -51

;; {(3 * - 2 ** 4) + 1}
;; -47

;; {2 * 3 ** - 2 ** 4}
;; 2/43046721 = 4.6461146250837546e-08

;; {3 * - 2 ** 4 ** 2 + 1}
;; -196607

;; {3 * - 2 ** 4 ** 2 / 2}
;; -98304

;; {3 * - 2 ** 4 ** 2 < 0}
;; #t


;; {1 + 3 * - 2 ** 4 ** 2 < 0}
;; #t

;; {- 4 < 0}
;; #t

;; {3 * - 2 ⁴ ** 2 + 1}
;; -196607

