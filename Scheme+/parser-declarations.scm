;; Translated from Scheme+ for Racket (parser-declarations.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2025 Damien MATTEI

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


;; What the infix parser must know to choose between prefix, infix and postfix in an expression like (f - 1) or (x 3 +) :
;;
;; * the postfix operators : (declare-postfix-operator op ...) written by the user
;;
;; * the executables : the procedures and macros of the program, unknown by Racket ; found by the readers (SRFI-105, SRFI-110)
;;   in the definitions of procedures and macros, the types of procedures (type procedure? f), the overloadings
;;
;; * the data : the variables typed with a type which is not a procedure (type flonum? x) ; found by the readers
;;
;; The readers store them while reading, for the infix between { } converted at the reading.
;; The infix converted at the expansion of the macros (def+ , define+ , lambda-x , rec+ , (def x expr ...) ... which are
;; deprecated) knows only the procedures of Racket.

(define-module (Scheme+ parser-declarations)
  #:use-module (Scheme+ racket-compat))

  

  (use-modules
	(Scheme+ operators-list)
	((Scheme+ operators) #:select (operator?))
	((srfi srfi-13) #:select (string-prefix?)))


  ;; (declare-postfix-operator op ...) : then (x op) is (op x)
  ;; the reader stores the operators while reading (see verify-if-declaration-for-parser-then-store-it), the macro at the
  ;; execution for the parser of the runtime
  (define-syntax declare-postfix-operator
    (syntax-rules ()
      ((_ op ...)
       (begin (insert-in-postfix-lst (quote op)) ... (void)))))


  ;; (declare-prefix-operator op ...) : then {a and op b} is (and a (op b)) , op binding as the unary minus (as ~)
  ;; the reader stores the operators while reading (see verify-if-declaration-for-parser-then-store-it), the macro at the
  ;; execution for the parser of the runtime
  (define-syntax declare-prefix-operator
    (syntax-rules ()
      ((_ op ...)
       (begin (insert-in-prefix-unary-lst (quote op)) ... (void)))))


  ;; each file read starts with empty declarations (only the factorial ! as postfix operator , only ~ as prefix unary operator)
  (define (reset-declarations-for-parser!)
    (reset-prefix-unary-lst!)
    (reset-postfix-lst!)
    (reset-executables-lst!)
    (reset-data-lst!))


  ;; called by the readers on each expression read : stores what the infix parser must know
  ;; return #t only for a declaration of postfix operators (the reading loops of the top level of the file do not keep it)
  (define (verify-if-declaration-for-parser-then-store-it datum)
    (cond ((not (and (list? datum)
		     (not (null? datum))
		     (symbol? (car datum))))
	   #f)
	  ((eq? (car datum) 'declare-postfix-operator) ; (declare-postfix-operator op ...)
	   (for-each insert-in-postfix-lst (filter symbol? (cdr datum)))
	   #t)
	  ((eq? (car datum) 'declare-prefix-operator) ; (declare-prefix-operator op ...)
	   (for-each insert-in-prefix-unary-lst (filter symbol? (cdr datum)))
	   #f)
	  ((and (memq (car datum) '(overload-unary-operator require-overload-unary-operator)) ; a prefix unary operator
		(pair? (cdr datum))
		(symbol? (cadr datum)))
	   (insert-in-prefix-unary-lst (cadr datum))
	   (for-each insert-executable (filter symbol? (overloaded-names datum)))
	   #f)
	  ((eq? (car datum) 'type) ; (type T name ...)
	   (when (and (pair? (cdr datum))
		      (type-form? (cadr datum))) ; not an other form like (type {z[]} vector?)
	     (for-each (if (procedure-type? (cadr datum))
			   insert-executable
			   insert-in-data-lst)
		       (filter symbol? (cddr datum)))) ; not {z[]} : the type of the elements of a container
	   #f)
	  ((overload-form? (car datum)) ; (overload-procedure name funct (pred ...)) , (define-overload-procedure name) ...
	   (for-each insert-executable (filter symbol? (overloaded-names datum)))
	   #f)
	  (else
	   (let ((name (defined-procedure-or-macro datum)))
	     (when name
	       (insert-executable name)))
	   #f)))

  (define (insert-executable name)
    (unless (operator? name) ; an infix operator (+ , * , · ...) is overloaded but stays an operator for the parser
      (insert-in-executables-lst name)))

;; a type : a predicate (flonum?) , #(T) a vector of elements of type T , (T) a list of elements of type T ,
  ;; or a signature of procedure
  (define (type-form? t)
    (or (symbol? t)
	(and (vector? t)
	     (= (vector-length t) 1))
	(and (list? t)
	     (= (length t) 1))
	(procedure-type? t)))

  ;; the type of a procedure : procedure? , or a signature ((return types ...) procedure? (parameter types ...))
  (define (procedure-type? t)
    (or (memq t '(procedure? macro?))
	(and (list? t)
	     (= (length t) 3)
	     (memq (cadr t) '(procedure? macro?)))))

  (define (overload-form? head)
    (let ((str (symbol->string head)))
      (or (string-prefix? "overload-" str)
	  (string-prefix? "define-overload" str)
	  (string-prefix? "require-overload-" str))))

  ;; (overload-procedure name funct (pred ...)) , (overload-operator op (return-type ...) funct (pred ...)) : name and funct ,
  ;; (define-overload-procedure name [module]) : name
  (define (overloaded-names datum)
    (define args (cdr datum))
    (cond ((null? args) '())
	  ((string-prefix? "define-overload" (symbol->string (car datum)))
	   (list (car args)))
	  (else
	   args))) ; the lists of types and predicates are not symbols, the caller keeps only the symbols

  ;; the name of the procedure or macro defined by datum, #f otherwise :
  ;; (define (f x) body ...) , (def (f x) body ...) , def+ , define+ , defun , (rec+ (f x) body ...) , curried: (define ((f a) b) body ...)
  ;; (define f (lambda (x) body ...)) , (define-syntax f ...) , (define-syntax (f stx) ...) , (define-syntax-rule (f x) template)
  ;; but not (def+ (x y z)) which defines variables without values (no body)
  (define (defined-procedure-or-macro datum)
    (define head (car datum))
    (define name
      (cond ((not (and (pair? (cdr datum))
		       (pair? (cddr datum)))) ; no body or no value
	     #f)
	    ((memq head '(define def def+ define+ defun rec+ define-syntax-rule))
	     (let ((h (cadr datum)))
	       (cond ((pair? h) (header-name h))
		     ((lambda-form? (caddr datum)) h)
		     (else #f))))
	    ((eq? head 'define-syntax)
	     (header-name (cadr datum)))
	    (else #f)))
    (and (symbol? name)
	 name))

  (define (header-name h) ; (f x) -> f , curried ((f a) b) -> f
    (if (pair? h)
	(header-name (car h))
	h))

  (define (lambda-form? e)
    (and (pair? e)
	 #;(memq (car e) '(lambda λ case-lambda lambda+))
	 (memq (car e) '(lambda λ case-lambda lambda+ lambda-x))))

  

(export
	declare-postfix-operator
	declare-prefix-operator
	verify-if-declaration-for-parser-then-store-it
	reset-declarations-for-parser!)
 ; end module
