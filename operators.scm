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

(define-module (operators)

  ;;#:use-module ((guile))
  #:use-module (syntax)
  #:use-module (srfi srfi-1) ; first
  #:use-module (operators-list)
  
  #:export ( operator?
	     operator-syntax?

	     operator
	     arg1
	     arg2
	     arg
	     args

	     function-without-parameters?
	     unary-operation?
	     binary-operation?

	     AND-op?
	     OR-op?
	     XOR-op?
	     NOT-op?
	     ADD-op?
	     IMPLIC-op?
	     EQUIV-op?
	     DEFINE-op?
	     ASSIGNMENT-op?
	     is-associative-operator?

	     ;; expression tests
	     isADD?
	     MULTIPLY-op?
	     isMULTIPLY?
	     isOR?
	     isAND?
	     isOR-AND?
	     isNOT?
	     isIMPLIC?
	     isEQUIV?
	     isXOR?
	     isDEFINE?
	     isASSIGNMENT?
	     isASSOCIATIVE? ))


(define rest cdr)


(define (operator? x)
  (member x operators-lst))

;; syntax version
(define (operator-syntax? x)
  (member-syntax x operators-lst-syntax));;operators-lst))



;; return the operator of an operation
(define (operator expr)
  (car expr))


;; return the first argument of a binary operation
(define (arg1 expr)
  (display "arg1") (newline)
  (first (rest expr)))

;; return the second argument of a binary operation
(define (arg2 expr)
  (display "arg2") (newline)
  (first (rest (rest expr))))

(define (arg expr)
  (arg1 expr))

;; return the arguments of an operation
(define (args expr)
  (cdr expr))

;; function without parameters
(define (function-without-parameters? expr)
  (null? (rest expr)))

;; (unary-operation? '(not a)) -> #t
(define (unary-operation? expr)
  (null? (rest (rest expr))))

;; (binary-operation? '(and a b)) -> #t
(define (binary-operation? expr)
  ;;(null? (rest (rest (rest expr)))))
  (and (pair? expr)
       (pair? (rest expr))
       (pair? (rest (rest expr)))
       (null? (rest (rest (rest expr))))))








;; operators test

;; test if an operator is AND
(define (AND-op? oper)
  (datum=? oper 'and))
  ;;(or (eqv? oper 'and) (check-syntax=? oper #'and)))
  ;;(or (eqv? oper 'and) (eqv? oper 'AND) (eqv? oper '·)))

;; test if an operator is OR
(define (OR-op? oper)
  (datum=? oper 'or))
  ;;(or (eqv? oper 'or) (check-syntax=? oper #'or)))
  ;;(or (eqv? oper 'or) (eqv? oper 'OR)  (eqv? oper '➕))) ;; middle dot

(define (XOR-op? oper) ;; note: logxor in Guile, xor in Racket
  (or (datum=? oper 'logxor)
      (datum=? oper 'xor)
      (datum=? oper '^)))

  ;;(or (eqv? oper 'xor) (check-syntax=? oper #'xor)))
  ;;(or (eqv? oper 'xor) (eqv? oper 'XOR)  (eqv? oper '⊕))) ;; ⨁


;; test if an operator is NOT
(define (NOT-op? oper)
  (datum=? oper 'not))
  ;;(or (eqv? oper 'not) (check-syntax=? oper #'not))) ; not sure it is usefull in syntax 
  ;;(or (eqv? oper 'not) (eqv? oper 'NOT)))

(define (ADD-op? oper)
  (or (eqv? oper +)
      (datum=? oper '+)))
  ;;(or (eqv? oper +) (eqv? oper '+) (check-syntax=? oper #'+)))

(define (IMPLIC-op? oper)
  (or (eqv? oper '⟹) (eqv? oper '=>)))

(define (EQUIV-op? oper)
  (or (eqv? oper '⟺) (eqv? oper '<=>)))


(define (DEFINE-op? oper)
  ;; (or (eqv? oper '<+) (eqv? oper '+>)
  ;;     (eqv? oper '←) (eqv? oper '+>)
  ;;     (eqv? oper '<-) (eqv? oper '->)
  ;;     (eqv? oper '←) (eqv? oper '+>)))

  (or (memv oper definition-operator)
      (member-syntax oper definition-operator-syntax)))


  
(define (ASSIGNMENT-op? oper)
  ;;(or (eqv? oper '<-) (eqv? oper '->)))
  (or (memv oper assignment-operator)
      (member-syntax oper assignment-operator-syntax)))





(define (is-associative-operator? op)
  (or (AND-op? op)
      (OR-op? op)
      (XOR-op? op)
      (EQUIV-op? op)
      (ADD-op? op)
      (MULTIPLY-op? op)
      (DEFINE-op? op)
      (ASSIGNMENT-op? op)))










;; expression tests

(define (isADD? expr)
  (and (pair? expr) (ADD-op? (car expr))))

(define (MULTIPLY-op? oper)
  (or (eqv? oper *)
      (datum=? oper '*)))

  ;;(or (eqv? oper *) (eqv? oper '*) (check-syntax=? oper #'*)))

(define (isMULTIPLY? expr)
  (and (pair? expr) (MULTIPLY-op? (car expr))))

;; test if an expression is a OR
(define (isOR? expr)
  ;;(and (pair? expr) (equal? (car expr) 'or)))
  (and (pair? expr) (OR-op? (car expr))))

;; test if an expression is a AND
(define (isAND? expr)
  ;;(and (pair? expr) (equal? (car expr) 'and)))
  (and (pair? expr) (AND-op? (car expr))))


;; is expression an (OR or AND) ?
(define (isOR-AND? expr)
  (or (isOR? expr)  (isAND? expr)))

(define (isNOT? expr)
  (and (pair? expr) (NOT-op? (car expr))))

(define (isIMPLIC? expr)
  (and (pair? expr) (IMPLIC-op? (operator expr))))

(define (isEQUIV? expr)
  (and (pair? expr) (EQUIV-op? (operator expr))))

(define (isXOR? expr)
  (and (pair? expr) (XOR-op?  (operator expr))))

(define (isDEFINE? expr)
  (and (pair? expr) (DEFINE-op?  (operator expr))))

(define (isASSIGNMENT? expr)
  (and (pair? expr) (ASSIGNMENT-op?  (operator expr))))


(define (isASSOCIATIVE? expr)
  ;;(display "isASSOCIATIVE?") (newline)
  (is-associative-operator? (first expr)))

