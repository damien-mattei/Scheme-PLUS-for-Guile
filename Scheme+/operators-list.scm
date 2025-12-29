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

(define-module (Scheme+ operators-list)

  #:use-module (srfi srfi-1)
  #:use-module (Scheme+ syntax)
  #:use-module (Scheme+ map-nested)
  
  #:export (definition-operator
	    assignment-operator

	    exponential-operator
	    infix-operators-lst-for-parser
	    
	    definition-operator-syntax
	    assignment-operator-syntax

	    exponential-operator-syntax
	    arithmetic-operator-syntax
	    infix-operators-lst-for-parser-syntax
	    
	    operators-lst
	    operators-lst-syntaxarithmetic-operator-lst-syntax
	    strict-precedence-over-minus
	    in/equalities-operator-syntax))




;; (define definition-operator (list '<+ '+>
;; 				  '⥆ '⥅
;; 				  ':+ '+:))

;; (define assignment-operator (list '<- '->
;; 				  '← '→
;; 				  ':=  '=:
;; 				  '<v 'v>
;; 				  '⇜ '⇝))


;; (define infix-operators-lst-for-parser

;;   (list
    
;;    (list 'expt '**)
;;    (list '* '/ '%)
 
;;    (list '+ '-)
   
;;    (list '<< '>>)
   
;;    (list '&)
;;    (list '^)
;;    (list '∣)
   
;;    (list '< '> '= '≠ '<= '>= '<>)

;;    (list 'and)
   
;;    (list 'or)
    
;;    (append assignment-operator 
;; 	   definition-operator)
     
;;    )
  
;;   )

;;  from syntax we create the quoted operators symbols with syntax->datum, but we could have done the reverse (with datum->syntax)
	

(define definition-operator-syntax (list #'<+ #'+>
					 #'⥆ #'⥅
					 #':+ #'+:))

(define definition-operator (map syntax->datum definition-operator-syntax))


(define assignment-operator-syntax (list #'<- #'->
					 #'← #'→
					 #':= #'=:
					 #'<v #'v>
					 #'⇜ #'⇝))

(define assignment-operator (map syntax->datum assignment-operator-syntax))

(define exponential-operator-syntax (list #'expt #'**))

(define exponential-operator (map syntax->datum exponential-operator-syntax))


;; precedence lists


(define calculus-operator-syntax (list (list #'* #'/ #'% #'·)
				       (list #'⊕) ; symbolic Xor
				       (list #'- #'+)
				       (list #'<< #'>>)
				       (list #'&)
				       (list #'^) ; bitwise xor
				       (list #'|) ; TODO check in Guile with {2 & 3 | 4} should retgurn 6 in Racket ,buggy in Guile 9.7
				       ))

(define in/equalities-operator-syntax (list #'< #'> #'= #'≠ #'<= #'>= #'<> #'≤ #'≥ #'equal?))

(define arithmetic-operator-syntax (append (list exponential-operator-syntax)
					   calculus-operator-syntax
					   (list in/equalities-operator-syntax)
					   (list (list #'and))
					   (list (list #'or))))




#;(define infix-operators-lst-for-parser-syntax

  (list
    (list #'expt #'**)
    (list #'* #'/ #'%)
    (list #'+ #'-)
	
    (list #'<< #'>>)

    (list #'& #'∣)

    (list #'< #'> #'= #'≠ #'<= #'>= #'<>)

    (list #'and)

    (list #'or)

    assignment-operator-syntax
    definition-operator-syntax))


(define infix-operators-lst-for-parser-syntax

  `(,(list #'∘)
    ,@arithmetic-operator-syntax
    ;,(list #'~>) ; for Qi in Racket
    ,assignment-operator-syntax
    ,definition-operator-syntax))

;; (define (get-infix-operators-lst-for-parser-syntax)
;;   infix-operators-lst-for-parser-syntax)

(define infix-operators-lst-for-parser (map-nested syntax->datum infix-operators-lst-for-parser-syntax))

;; liste à plate des operateurs

(define arithmetic-operator-lst-syntax
  (apply append arithmetic-operator-syntax))

(define operators-lst
  (apply append infix-operators-lst-for-parser))

(define operators-lst-syntax
  (apply append infix-operators-lst-for-parser-syntax))
