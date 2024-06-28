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

(define-module (operators-list)

  
  #:export (definition-operator
	    assignment-operator
	    
	    infix-operators-lst-for-parser
	    
	    definition-operator-syntax
	    assignment-operator-syntax
	    
	    infix-operators-lst-for-parser-syntax
	    
	    operators-lst
	    operators-lst-syntax))




(define definition-operator (list '<+ '+>
				  '⥆ '⥅
				  ':+ '+:))

(define assignment-operator (list '<- '->
				  '← '→
				  ':=  '=:
				  '<v 'v>
				  '⇜ '⇝))


(define infix-operators-lst-for-parser

  (list
    
   (list 'expt '**)
   (list '* '/ '%)
 
   (list '+ '-)
   
   (list '<< '>>)
   
   (list '&)
   (list '^)
   (list '∣)
   
   (list '< '> '= '≠ '<= '>= '<>)

   (list 'and)
   
   (list 'or)
    
   (append assignment-operator 
	   definition-operator)
     
   )
  
  )




(define definition-operator-syntax (list #'<+ #'+>
					 #'⥆ #'⥅
					 #':+ #'+:))

(define assignment-operator-syntax (list #'<- #'->
					 #'← #'→
					 #':= #'=:
					 #'<v #'v>
					 #'⇜ #'⇝))



(define infix-operators-lst-for-parser-syntax

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


(define (get-infix-operators-lst-for-parser-syntax)
  infix-operators-lst-for-parser-syntax)



;; liste à plate des operateurs
(define operators-lst
  (apply append infix-operators-lst-for-parser))

(define operators-lst-syntax
  (apply append infix-operators-lst-for-parser-syntax))
