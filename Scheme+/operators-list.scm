;; Translated from Scheme+ for Racket (operators-list.rkt) for Guile.

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


;; code from Scheme+R6RS

  
(define-module (Scheme+ operators-list)
  #:use-module (Scheme+ racket-compat))

	

	(use-modules
	(srfi srfi-1)
	(Scheme+ syntax)
	(Scheme+ map-nested))

	

;;  from syntax we create the quoted operators symbols with syntax->datum, but we could have done the reverse (with datum->syntax)
	
;(define orig-+ +)
	

(define definition-operator-syntax (list #'<+ #'+>
					 #'⥆ #'⥅
					 #':+ #'+:
					 ))



(define definition-operator (map syntax->datum definition-operator-syntax))

;; ???????? a quoi ça devait servir ?
;;(define single-variable-assignment-operator-syntax (list #'<- #'->))


(define assignment-operator-syntax ;;(append single-variable-assignment-operator-syntax
					   (list #'<- #'->
					         #'← #'→
					         #':= #'=: 
					         #':=> ; the walrus operator
					         #'<v #'v>
					         #'⇜ #'⇝));)
  
(define assignment-operator (map syntax->datum assignment-operator-syntax))


(define exponentiation-operator-syntax (list #'expt #'**))


(define exponentiation-operator (map syntax->datum exponentiation-operator-syntax))


;; the operators that are right-associative for the Pratt parser (pratt-infix-parser.rkt) : today only
;; expt/** , aliased from exponentiation-operator-syntax so there is a single place to extend if another
;; right-associative operator is ever added.
(define right-associative-operators-syntax exponentiation-operator-syntax)
(define right-associative-operators exponentiation-operator)

(define (right-associative? op)
  (member-generic op right-associative-operators-syntax))




;; precedence lists


(define calculus-operator-syntax (list (list #'* #'/ #'% #'· #'×)
				       ;;(list #'·) ; symbolic logic And
				       (list #'⊕) ; symbolic Xor
				       (list #'- #'+)
				       (list #'<< #'>>)
				       (list #'&)
				       (list #'^) ; bitwise xor
				       (list #'∣) ; it is not the keyboard character because reserved in Racket !
				       ))

(define in/equalities-operator-syntax (list #'< #'> #'= #'≠ #'<= #'>= #'<> #'≤ #'≥ #'equal?))

(define arithmetic-operator-syntax (append (list exponentiation-operator-syntax)
					   calculus-operator-syntax
					   (list in/equalities-operator-syntax)
					   (list (list #'and))
					   (list (list #'or))))


(define infix-operators-lst-for-parser-syntax

  `(,(list #'∘)
    ,@arithmetic-operator-syntax
    ,(list #'~>) ; for Qi
    ,assignment-operator-syntax
    ,definition-operator-syntax))


(define infix-operators-lst-for-parser (map-nested syntax->datum infix-operators-lst-for-parser-syntax))



;; liste à plate des operateurs

(define arithmetic-operator-lst-syntax
  (apply append arithmetic-operator-syntax))


(define operators-lst
  (apply append infix-operators-lst-for-parser))




(define operators-lst-syntax
  (apply append infix-operators-lst-for-parser-syntax))

(define strict-precedence-over-minus (take-while (lambda (x) (not (datum=? x #'-)))
						 operators-lst-syntax))


;; (define infix-operators-lst-for-parser

;;   (list

;;    (list '∘) ; composition operator (for functions) Option + k  on MacOS
    
;;    exponentiation-operator
   
;;    (list '* '/ '% '·) ;; for:a ² + 2 · a · b + b ²      AltGr + Maj + 1 --> · on french keyboard ,Option + h on MacOS

;;    ;;(list '·) ; symbolic logic And   

;;    (list '⊕) ; symbolic logic Xor
 
;;    (list '- '+) ; note + could be used as symbolic logic Or, for this reason and operator precedence i put And and Xor above
   
;;    (list '<< '>>)
   
;;    (list '&)
;;    (list '^)
;;    (list '∣)
   
;;    (list '< '> '= '≠ '<= '>= '<> 'equal?)

;;    (list 'and)
   
;;    (list 'or)
    
;;    (append assignment-operator 
;; 	   definition-operator)
     
;;    )
  
;;   )


(define postfix-lst '(!))

(define (insert-in-postfix-lst elem) ; insert it only if not already inside
  (when (not (member elem postfix-lst))
    (set! postfix-lst (cons elem postfix-lst))))

(define (member-in-postfix-lst elem)
  (member elem postfix-lst))

(define (get-postfix-lst)
  postfix-lst)

(define (reset-postfix-lst!) ; each file read starts with only the factorial as postfix operator
  (set! postfix-lst '(!)))


;; the prefix unary operators binding as the unary minus : ~ , and those declared in the file with overload-unary-operator ,
;; require-overload-unary-operator or declare-prefix-operator (see parser-declarations.rkt) ; {a and ¬ b} is (and a (¬ b))
;; (see group-prefix-operators in infix-prefix-postfix.rkt , not has its own rule , the signs - + theirs)
(define prefix-unary-lst '(~))

(define (insert-in-prefix-unary-lst elem) ; insert it only if not already inside
  (when (not (member elem prefix-unary-lst))
    (set! prefix-unary-lst (cons elem prefix-unary-lst))))

(define (member-in-prefix-unary-lst elem) ; elem : a symbol or a syntax object
  (define d (if (syntax? elem) (syntax->datum elem) elem))
  (and (symbol? d)
       (member d prefix-unary-lst)
       #t))

(define (get-prefix-unary-lst)
  prefix-unary-lst)

(define (reset-prefix-unary-lst!) ; each file read starts with only ~
  (set! prefix-unary-lst '(~)))

;; the procedures and macros known by the infix parser, in addition to those of Racket (see known-program? in program-type.rkt) :
;; defined in the file, typed with a type of procedure, overloaded (see parser-declarations.rkt)
(define executables-lst '())

(define (insert-in-executables-lst elem) ; insert it only if not already inside
  (when (not (member elem executables-lst))
    (set! executables-lst (cons elem executables-lst))))

(define (member-in-executables-lst elem)
  (member elem executables-lst))

(define (get-executables-lst)
  executables-lst)

(define (reset-executables-lst!)
  (set! executables-lst '()))


;; the variables typed with a type which is not a procedure, example: (type flonum? x y)
;; an expression starting with such a variable can not be a call, so (x 3 +) is postfix (see infix-prefix-postfix.rkt)
(define data-lst '())

(define (insert-in-data-lst elem) ; insert it only if not already inside
  (when (not (member elem data-lst))
    (set! data-lst (cons elem data-lst))))

(define (member-in-data-lst elem)
  (member elem data-lst))

(define (get-data-lst)
  data-lst)

(define (reset-data-lst!)
  (set! data-lst '()))


#;(define (declare-postfix-operator op) 
  (insert-in-postfix-lst op))

;; now in parser-declarations.rkt : the operator is also stored at the compilation, for the parser called by def+ , define+ ...
#;(define-syntax declare-postfix-operator
  (syntax-rules ()
    ((_ op) 
     (insert-in-postfix-lst (quote op)))))


;; replaced by verify-if-declaration-for-parser-then-store-it in parser-declarations.rkt
#;(define (verify-if-declare-postfix-operator-then-store-it-for-proc datum)  ; return a boolean
  (if (and (list? datum)
           (not (null? datum))
           (eq? (car datum) 'declare-postfix-operator))
      (begin
        (insert-in-postfix-lst (cadr datum))
        #t)
      #f))

;; replaced by verify-if-declaration-for-parser-then-store-it in parser-declarations.rkt
#;(define (verify-if-declare-postfix-operator-then-store-it datum)  ; return a boolean
  (if (and (list? datum)
           (not (null? datum))
           (eq? (car datum) 'declare-postfix-operator))
      (begin
        (insert-in-postfix-lst (cadr datum))
        (display "Parser : main.rkt : Postfix operators list :") (display (get-postfix-lst)) (newline)
        #t)
      #f))


#;(define oper-equiv-syntax (list (cons #'* #'orig-*)
				(cons #'- #'orig--)
				(cons #'+ #'orig-+)
				(cons #'/ #'orig-/)))

;(define oper-equiv-symb (map-nested-pair syntax->datum oper-equiv-syntax))

;(define oper-equiv (append oper-equiv-syntax oper-equiv-symb))

;(displayln orig-+)

#;(define oper-equiv-symbol-syntax (list (cons '* #'orig-*)
				       (cons '- #'orig--)
				       (cons '+ #'orig-+)
				       (cons '/ #'orig-/)))



(export
	definition-operator
	assignment-operator
	exponentiation-operator
	infix-operators-lst-for-parser
	right-associative-operators
	right-associative-operators-syntax
	right-associative?
	definition-operator-syntax
	assignment-operator-syntax
	exponentiation-operator-syntax
	arithmetic-operator-syntax
	infix-operators-lst-for-parser-syntax
	operators-lst
	operators-lst-syntax
	arithmetic-operator-lst-syntax
	strict-precedence-over-minus
	in/equalities-operator-syntax
	insert-in-postfix-lst
	member-in-postfix-lst
	get-postfix-lst
	reset-postfix-lst!
	insert-in-prefix-unary-lst
	member-in-prefix-unary-lst
	get-prefix-unary-lst
	reset-prefix-unary-lst!
	insert-in-executables-lst
	member-in-executables-lst
	get-executables-lst
	reset-executables-lst!
	insert-in-data-lst
	member-in-data-lst
	get-data-lst
	reset-data-lst!)
 ; end module



;; > {1 & 1 ^ 0}


;; (^ (& 1 1) 0)
;; 1


;; > {1 & 1 ^ 1}


;; (^ (& 1 1) 1)
;; 0


;; > {1 & 1 ^ 1 ∣ 1}


;; (∣ (^ (& 1 1) 1) 1)
;; 1

