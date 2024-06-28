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

;; Guile version

(define-module (n-arity)
  
  #:use-module (operators-list)
  #:use-module (operators)
  
  #:export (n-arity))

  
;; initially written for logical expression of OR / AND
;; should works for every associative operator

;; n-arity function, this version will not show AND & OR case but collect them in one single line code
;; n-arity single function replacing n-arity-or and n-arity-and and that use the collect-leaves function 
;; with no match special form inside them and no operator show
;;
;;  (n-arity '(or a (or b c)))
;; '(or a b c)
;; > (n-arity '(or a (or b c d)))
;; '(or a b c d)
;; > (n-arity '(or a (and e f) (or b c d)))
;; '(or a (and e f) b c d)
;; > (n-arity '(or a (and e (or f g h i)) (or b c d)))
;; '(or a (and e (or f g h i)) b c d)
;; > (n-arity '(or a (and e (or f g h i)) (or b c d (or j k l))))
;; '(or a (and e (or f g h i)) b c d j k l)
;;
;; (n-arity '(or a (and e (or f g h i) (and m n)) (or b c d (or j k l))))
;; -> '(or a (and e (or f g h i) m n) b c d j k l)
;;
;; (n-arity '(not (or a (and e (or f g h i) (and m n)) (or b c d (or j k l)))))
;; '(not (or a (and e (or f g h i) m n) b c d j k l))
;; > (n-arity '(not (or a (and e (or f g (not h) i) (and m n)) (or b c d (or j k l)))))
;; '(not (or a (and e (or f g (not h) i) m n) b c d j k l))
;; > (n-arity '(not (or a (and e (or f g (not h) i) (and (not m) n)) (or (not b) c d (or j k l)))))
;; '(not (or a (and e (or f g (not h) i) (not m) n) (not b) c d j k l))

  


;;  > (n-arity '(or a (not b) (or (or (and c (and c2 c3)) d) e) (and (and (not f) g) h) (or i (and (not j) (and k (or l (or m (not n))))))) )
;;  '(or a (not b) (and c c2 c3) d e (and (not f) g h) i (and (not j) k (or l m (not n))))
;;  > 

  

;; (n-arity '(+ a (+ b c))) -> '(+ a b c)
;;
;; 


;; > (n-arity '(+ a (+ b c)))
;; '(+ a b c)
;; > (n-arity '(- a (- b c)))
;; '(- a (- b c))
;;  (n-arity '(- (- (- a b) c) d))
;; '(- a b c d) 
;; > (n-arity '(- a (+ b c)))
;; '(- a (+ b c))
;; > (n-arity '(+ (+ a (- b c)) d))
;; '(+ a (- b c) d)
;; > (n-arity '(+ (+ a (+ e (- b c))) d))
;; '(+ a e (- b c) d)
;; > (n-arity '(+ (+ a (+ e (- b (+ f (+ g c)))) d) h))
;; '(+ a e (- b (+ f g c)) d h)
;; >

;;  (n-arity '(<- a (<- b (<- c 7))))
;; '(<- a b c 7)
;; (n-arity '(<- x (<- a (<- b (- b c)))))
;; '(<- x a b (- b c))

;; warning: usualy give a false result if operator is not associative
;; could not work with exponentiation, expt , ** : is evaluation is from right to left (opposite normal evaluation and not associative! and not commutative!)
(define (n-arity expr)

  ;;(display "n-arity : expr =")(display expr) (newline)

  (define rv
    (cond
     ((not (list? expr)) expr) ;; symbol,number,boolean,vector,hash table....
     ((null? expr) expr)
     ((function-without-parameters? expr) expr)
     ((unary-operation? expr)
      (cons
       (operator expr)
       (list (n-arity (arg expr)))))
     ((isASSOCIATIVE? expr)
      (let ((opera (operator expr)))
	(cons opera
	      (apply
	       append
	       (map (make-collect-leaves-operator opera) (args expr))))))
     
     (else ;; safe else, will not touch non associative operators
      (let ((opera (operator expr)))
	(cons opera
	      (map n-arity (args expr)))))))

  ;;(display "n-arity : rv =") (display rv) (newline)
  ;;(newline)

  rv

  )


;; return a closure of collect-leaves function associated with an operator (AND or OR)
(define (make-collect-leaves-operator oper)

  ;;(display "make-collect-leaves-operator") (newline) 
  (let ((ourOperation?
	 
	 (cond
	  
	  ((AND-op? oper) isAND?)
	  ((OR-op? oper) isOR?)
	  ((ADD-op? oper) isADD?)
	  ((DEFINE-op? oper) isDEFINE?)
	  ((ASSIGNMENT-op? oper) isASSIGNMENT?)

	  (else ; fonction generique pour tous les operateurs ( non syntaxiques )
	   (lambda (expr) (and (pair? expr) (equal? (car expr) oper)))))))

    
    (letrec ((collect-leaves-operator

	      (lambda (expr)
		(cond
		 ((not (list? expr)) (list expr)) ;; symbol,number,boolean,vector,hash table....
		 ((null? expr) (list expr))
		 ((function-without-parameters? expr) (list expr))
		 ((unary-operation? expr)
		  (list
		   (cons
		    (operator expr)
		    (list (n-arity (arg expr))))))
		 ((ourOperation? expr) ;; #;(eqv? oper (operator expr))
		  (apply append (map collect-leaves-operator (args expr))))
		 (else (list (n-arity expr)))))))

        collect-leaves-operator)))


