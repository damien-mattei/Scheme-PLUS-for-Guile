;; overload.scm

;; Guile Scheme version

;; Damien Mattei

;; This file is part of Scheme+

;; Copyright 2021-2026 Damien MATTEI

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


;; The overloading of operators and procedures , in Guile.
;;
;; The mechanism is the one of the previous versions of Scheme+ for Guile : the overloaded operator is a procedure
;; (a closure) that keeps the original operator , the program redefines the operator with it :
;;    (define + <the overloaded + , that falls back on the original +>)
;; There is no table indexed by the names of the operators and no definition introduced by a macro at the top level
;; (in Guile two expansions of a macro that introduce a top-level definition of the same name share one variable).
;; The overloads of an operator are kept with its closure : each overloaded operator has its own state (a record
;; found by the procedure itself , in a weak table).
;;
;; The interface is the one of Scheme+ for Racket (overload.rkt) :
;;   (overload-operator + (vector?) vector-append (vector? vector?)) ; 2 predicates : binary overload
;;   (overload-operator + string-append (string?))                   ; 1 predicate : n-ary overload
;;   (overload-unary-operator - vector-negate (vector?))              ; unary overload
;;   (overload-procedure length vector-length (vector?))             ; procedure : exact number of arguments
;;   (overload-operator proc (module name) funct (pred ...))         ; the original operator is taken from a module
;; The optional list of the return values ((vector?) above) is only read by the type annotation pass.
;; The first overload of a name defines the overloaded operator (its original is the current binding of the name ,
;; or the one of the given module) , the next ones add an overload.
;; A call tries , in order : the arithmetic operators (+ - * / · ×) with numbers only go straight to the original
;; operator ; the overloads with an exact number of arguments (binary , unary , procedures) , the most recent first ;
;; the n-ary overloads (all the arguments satisfy the predicate) ; for more than 2 arguments and an operator with
;; binary overloads , the left associativity : (op a b c) = (op (op a b) c) ; the original operator ; else an error.
;; The predicates must accept any value and answer #t or #f , never raise an error.
;;
;; The two steps macros of the old versions (define-overload-operator , overload-existing-operator ...) are kept for
;; compatibility.


(define-module (Scheme+ overload)

  #:use-module ((srfi srfi-1) #:select (every any first second))
  #:use-module (srfi srfi-9) ; records

  #:export (overload-operator
	    overload-unary-operator
	    overload-procedure
	    require-overload-operator
	    require-overload-unary-operator
	    require-overload-procedure

	    define-overload-operator
	    define-overloaded-operator
	    define-overload-existing-operator
	    define-overload-existing-operator-annot
	    overload-existing-operator
	    define-overload-n-ary-associative-operator
	    define-overload-existing-n-ary-associative-operator
	    define-overload-existing-n-ary-associative-operator-annot
	    overload-n-ary-operator
	    overload-existing-n-ary-operator

	    ;; the names of the previous versions of Scheme+ for Guile
	    define-overload-procedure
	    define-overload-existing-procedure
	    overload-existing-procedure
	    overload-function
	    define-overload-n-arity-operator
	    define-overload-existing-n-arity-operator
	    overload-n-arity-operator
	    overload-existing-n-arity-operator

	    $ovrld-square-brackets-lst$
	    overload-square-brackets
	    require-overload-square-brackets
	    find-getter-for-overloaded-square-brackets
	    find-setter-for-overloaded-square-brackets

	    overloaded?
	    check-arguments))


;; ---------- the state of an overloaded operator or procedure

(define-record-type <overloaded>
  (make-overloaded name kind original exact n-ary)
  overloaded-state?
  (name overloaded-name) ; the symbol , for the messages
  (kind overloaded-kind) ; operator or procedure
  (original overloaded-original) ; the original procedure , or #f for a new one
  (exact overloaded-exact set-overloaded-exact!) ; list of (funct . predicates) , exact number of arguments
  (n-ary overloaded-n-ary set-overloaded-n-ary!)) ; list of (funct . predicate) , any number of arguments

;; the overloaded procedure -> its state
(define states (make-weak-key-hash-table))

(define (overloaded? p) (and (procedure? p) (hashq-ref states p) #t))

;; the arithmetic operators keep a fast path for numbers
(define arithmetic-names '(+ - * / · ×))

;; (check-arguments (list number? string?) '(3 "a")) -> #t : as many arguments as predicates , each one satisfies its
;; predicate
(define (check-arguments pred-list args)
  (and (= (length pred-list) (length args))
       (every (lambda (pred arg) (pred arg)) pred-list args)))

;; create the overloaded procedure of name (a symbol) , with original (a procedure or #f)
(define (make-overloaded-procedure name kind original)
  (define state (make-overloaded name kind original '() '()))
  (define fast? (and original (eq? kind 'operator) (memq name arithmetic-names) #t))
  (define (overloaded . args)
    (cond
     ((and fast? (every number? args)) (apply original args))
     ((any (lambda (entry) (and (check-arguments (cdr entry) args) entry))
	   (overloaded-exact state))
      => (lambda (entry) (apply (car entry) args)))
     ((and (pair? args)
	   (any (lambda (entry) (and (every (cdr entry) args) entry))
		(overloaded-n-ary state)))
      => (lambda (entry) (apply (car entry) args)))
     ((and (eq? kind 'operator)
	   (> (length args) 2)
	   (pair? (overloaded-exact state)))
      ;; left associativity : (op a b c ...) = (op (op a b) c ...)
      (apply overloaded (cons (overloaded (first args) (second args)) (cddr args))))
     (original (apply original args))
     (else (error 'overload
		  (format #f "failed because procedure ~a can not be applied to arguments list ~a" name args)))))
  (hashq-set! states overloaded state)
  overloaded)

;; the overloaded procedure of name : current if it is already one , else a new one that falls back on current
;; (a procedure , or #f when the name was not bound)
(define (overloaded-procedure-of current name kind)
  (if (overloaded? current)
      current
      (make-overloaded-procedure name kind current)))

;; add an overload to the overloaded procedure of name , made from current if needed , and return it
;; kind of overload : exact (binary , unary , procedure) or n-ary
(define (overload-add! current name kind overload-kind funct preds)
  (define p (overloaded-procedure-of current name kind))
  (define state (hashq-ref states p))
  (case overload-kind
    ((exact) (set-overloaded-exact! state (cons (cons funct preds) (overloaded-exact state))))
    ((n-ary) (set-overloaded-n-ary! state (cons (cons funct (car preds)) (overloaded-n-ary state))))
    (else (error 'overload "unknown kind of overload" overload-kind)))
  p)


;; ---------- expansion time : the original of the name

;; the names already overloaded in each module , at expansion time : the next overloads of a name refer to the
;; overloaded procedure already defined in the module
(eval-when (expand load eval)
  (define overloaded-in-modules (make-hash-table))

  (define (note-overloaded! id)
    (hash-set! overloaded-in-modules (cons (module-name (current-module)) (syntax->datum id)) #t))

  (define (already-overloaded? id)
    (hash-ref overloaded-in-modules (cons (module-name (current-module)) (syntax->datum id)) #f))

  ;; is the identifier bound (defined or imported) in the module being expanded ?
  (define (bound-in-current-module? id)
    (let ((v (module-variable (current-module) (syntax->datum id))))
      (and v #t)))

  ;; in (overload-operator proc x funct (pred ...)) , is x a module name rather than the list of return values ?
  ;; a module name is a list of symbols naming an existing module , as (guile) or (srfi srfi-1)
  (define (module-name? stx)
    (let ((d (syntax->datum stx)))
      (and (pair? d)
	   (list? d)
	   (every symbol? d)
	   (false-if-exception (resolve-interface d))
	   #t)))

  ;; the name of the imported module that provides the binding of sym in the module being expanded , or #f
  (define (imported-from sym)
    (let ((v (module-variable (current-module) sym)))
      (and v
	   (not (module-local-variable (current-module) sym))
	   (let loop ((uses (module-uses (current-module))))
	     (cond ((null? uses) #f)
		   ((eq? (module-variable (car uses) sym) v) (module-name (car uses)))
		   (else (loop (cdr uses))))))))

  ;; the expression of the current procedure of the name proc-stx : the name itself when it is already overloaded in
  ;; this module (the next overloads add to it , even with a module given) ; the binding of the given module ; the name
  ;; itself when it is defined in this module ; (@ module proc) when it is imported (in a compiled
  ;; module , the proc of (define proc (... proc ...)) would be the variable of the module , not yet defined) ; #f for a
  ;; new name
  (define (current-expression proc-stx module-stx)
    (define sym (syntax->datum proc-stx))
    (cond ((already-overloaded? proc-stx) proc-stx) ; the next overloads add to the overloaded procedure of the module
	  (module-stx (with-syntax ((proc proc-stx) (module module-stx)) #'(@ module proc)))
	  ((imported-from sym) => (lambda (name) (with-syntax ((proc proc-stx) (module (datum->syntax proc-stx name)))
						    #'(@ module proc))))
	  ((bound-in-current-module? proc-stx) proc-stx)
	  (else #'#f)))

  ;; (define proc (overload-add! <current> 'proc kind overload-kind funct (list pred ...)))
  (define (overload-expansion stx proc-stx module-stx kind overload-kind funct-stx preds-stx)
    (syntax-case preds-stx ()
      ((pred ...) #t)
      (_ (syntax-violation 'overload "expected a list of predicates" stx preds-stx)))
    (let ((current (current-expression proc-stx module-stx)))
      (note-overloaded! proc-stx)
      (with-syntax ((proc proc-stx) (current current) (kind (datum->syntax proc-stx kind))
		    (overload-kind (datum->syntax proc-stx overload-kind))
		    (funct funct-stx) ((pred ...) preds-stx))
	#'(define proc (overload-add! current 'proc 'kind 'overload-kind funct (list pred ...))))))

  ;; the kind of an overload of an operator from its predicates : 1 predicate n-ary , 2 predicates binary
  (define (operator-overload-kind stx preds-stx)
    (syntax-case preds-stx ()
      ((pred) 'n-ary)
      ((pred1 pred2) 'exact)
      (_ (syntax-violation 'overload-operator
			   "expected 1 predicate (n-ary overload : all arguments of that type) or 2 predicates (binary overload)"
			   stx preds-stx))))

  ;; (define proc <the overloaded procedure , without overload yet>) : the first step of the old macros
  (define (definition-expansion proc-stx module-stx kind)
    (let ((current (current-expression proc-stx module-stx)))
      (note-overloaded! proc-stx)
      (with-syntax ((proc proc-stx) (current current) (kind (datum->syntax proc-stx kind)))
	#'(define proc (overloaded-procedure-of current 'proc 'kind))))))


;; ---------- the interface

;; (overload-operator proc funct (pred ...))
;; (overload-operator proc (ret-val ...) funct (pred ...))   ; list of the return values , for the annotation
;; (overload-operator proc module-name funct (pred ...))     ; the original operator is taken from module-name
;; (overload-operator proc module-name (ret-val ...) funct (pred ...))
(define-syntax overload-operator
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc funct preds)
       (overload-expansion stx #'proc #f 'operator (operator-overload-kind stx #'preds) #'funct #'preds))
      ((_ proc x funct preds)
       (overload-expansion stx #'proc (and (module-name? #'x) #'x) 'operator (operator-overload-kind stx #'preds)
			   #'funct #'preds))
      ((_ proc module-name (ret-val ...) funct preds)
       (overload-expansion stx #'proc #'module-name 'operator (operator-overload-kind stx #'preds) #'funct #'preds)))))

;; (overload-unary-operator - vector-negate (vector?)) : an overload of exactly one argument
(define-syntax overload-unary-operator
  (lambda (stx)
    (define (check preds-stx)
      (syntax-case preds-stx ()
	((pred) #t)
	(_ (syntax-violation 'overload-unary-operator "expected 1 predicate (the type of the single argument)"
			     stx preds-stx))))
    (syntax-case stx ()
      ((_ proc funct preds)
       (begin (check #'preds) (overload-expansion stx #'proc #f 'operator 'exact #'funct #'preds)))
      ((_ proc x funct preds)
       (begin (check #'preds)
	      (overload-expansion stx #'proc (and (module-name? #'x) #'x) 'operator 'exact #'funct #'preds)))
      ((_ proc module-name (ret-val ...) funct preds)
       (begin (check #'preds) (overload-expansion stx #'proc #'module-name 'operator 'exact #'funct #'preds))))))

;; (overload-procedure area area-square (number?)) : any number of predicates , exact match
(define-syntax overload-procedure
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc funct preds)
       (overload-expansion stx #'proc #f 'procedure 'exact #'funct #'preds))
      ((_ proc x funct preds)
       (overload-expansion stx #'proc (and (module-name? #'x) #'x) 'procedure 'exact #'funct #'preds))
      ((_ proc module-name (ret-val ...) funct preds)
       (overload-expansion stx #'proc #'module-name 'procedure 'exact #'funct #'preds)))))

;; the overloads made in an imported module , restated for the type annotation pass : nothing at run time
(define-syntax require-overload-operator
  (syntax-rules () ((_ . args) (if #f #f))))
(define-syntax require-overload-unary-operator
  (syntax-rules () ((_ . args) (if #f #f))))
(define-syntax require-overload-procedure
  (syntax-rules () ((_ . args) (if #f #f))))


;; ---------- the two steps macros of the old versions

(define-syntax define-overload-operator ; a new binary operator
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #f 'operator)))))

(define-syntax define-overload-n-ary-associative-operator ; a new n-ary operator
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #f 'operator)))))

(define-syntax define-overloaded-operator ; an operator , existing or not
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #f 'operator))
      ((_ proc module-name) (definition-expansion #'proc #'module-name 'operator)))))

(define-syntax define-overload-existing-operator
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #'(guile) 'operator))
      ((_ proc module-name) (definition-expansion #'proc #'module-name 'operator)))))

(define-syntax define-overload-existing-n-ary-associative-operator
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #'(guile) 'operator))
      ((_ proc module-name) (definition-expansion #'proc #'module-name 'operator)))))

;; the *-annot macros also define orig-proc , the original operator , under the name given
(define-syntax define-overload-existing-operator-annot
  (syntax-rules ()
    ((_ proc orig-proc) (define-overload-existing-operator-annot proc orig-proc (guile)))
    ((_ proc orig-proc module-name)
     (begin
       (define orig-proc (@ module-name proc))
       (define-overload-existing-operator proc module-name)))))

(define-syntax define-overload-existing-n-ary-associative-operator-annot
  (syntax-rules ()
    ((_ proc orig-proc) (define-overload-existing-n-ary-associative-operator-annot proc orig-proc (guile)))
    ((_ proc orig-proc module-name)
     (begin
       (define orig-proc (@ module-name proc))
       (define-overload-existing-n-ary-associative-operator proc module-name)))))

;; the second step : add an overload to an operator defined by the first step
(define-syntax overload-existing-operator ; binary : exact number of arguments
  (syntax-rules ()
    ((_ proc funct (pred ...)) (overload-add! proc 'proc 'operator 'exact funct (list pred ...)))
    ((_ proc (ret-val ...) funct (pred ...)) (overload-existing-operator proc funct (pred ...)))))

(define-syntax overload-n-ary-operator ; n-ary : all the arguments satisfy the predicate
  (syntax-rules ()
    ((_ proc funct (pred ...)) (overload-add! proc 'proc 'operator 'n-ary funct (list pred ...)))
    ((_ proc (ret-val ...) funct (pred ...)) (overload-n-ary-operator proc funct (pred ...)))))

(define-syntax overload-existing-n-ary-operator
  (syntax-rules ()
    ((_ proc funct (pred ...)) (overload-add! proc 'proc 'operator 'n-ary funct (list pred ...)))
    ((_ proc (ret-val ...) funct (pred ...)) (overload-existing-n-ary-operator proc funct (pred ...)))))


;; ---------- the names of the previous versions of Scheme+ for Guile

(define-syntax define-overload-procedure ; a new procedure
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #f 'procedure)))))

(define-syntax define-overload-existing-procedure ; an existing procedure (of (guile) by default)
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc) (definition-expansion #'proc #'(guile) 'procedure))
      ((_ proc module-name) (definition-expansion #'proc #'module-name 'procedure)))))

(define-syntax overload-existing-procedure
  (syntax-rules ()
    ((_ proc funct (pred ...)) (overload-add! proc 'proc 'procedure 'exact funct (list pred ...)))))

;; (overload-function (proc (arg pred) ...) body ...) : an overload written as a lambda
(define-syntax overload-function
  (syntax-rules ()
    ((_ (proc (arg pred) ...) body ...)
     (overload-add! proc 'proc 'procedure 'exact (lambda (arg ...) body ...) (list pred ...)))))

(define-syntax define-overload-n-arity-operator
  (syntax-rules () ((_ . args) (define-overload-n-ary-associative-operator . args))))
(define-syntax define-overload-existing-n-arity-operator
  (syntax-rules () ((_ . args) (define-overload-existing-n-ary-associative-operator . args))))
(define-syntax overload-n-arity-operator
  (syntax-rules () ((_ . args) (overload-n-ary-operator . args))))
(define-syntax overload-existing-n-arity-operator
  (syntax-rules () ((_ . args) (overload-existing-n-ary-operator . args))))


;; ---------- the square brackets : T[i ...] for a type of the program (as in Scheme+ for Racket)

(define $ovrld-square-brackets-lst$ '()) ; list of ((pred ...) (getter . setter))

(define-syntax overload-square-brackets
  (syntax-rules ()
    ;; with the list of the return values of the getter , for the type annotation pass (ignored here) :
    ;; (overload-square-brackets (flonum?) matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
    ((_ (ret-val ...) getter setter (pred-arg pred-arg1 ...))
     (overload-square-brackets getter setter (pred-arg pred-arg1 ...)))
    ((_ getter setter (pred-arg pred-arg1 ...)) ; getter setter and list of predicate to check the arguments
     (modify-$ovrld-square-brackets-lst$ (list (list pred-arg pred-arg1 ...)
					       (cons getter setter))))))

(define-syntax require-overload-square-brackets
  (syntax-rules () ((_ . args) (if #f #f))))

(define (modify-$ovrld-square-brackets-lst$ arg)
  (set! $ovrld-square-brackets-lst$ (cons arg $ovrld-square-brackets-lst$)))

;; the (getter . setter) whose predicates match the arguments (the container and the indexes)
(define (find-getter-and-setter-for-overloaded-square-brackets args-lst)
  (or (any (lambda (pred-proc-list)
	     (and (check-arguments (car pred-proc-list) args-lst)
		  (cadr pred-proc-list)))
	   $ovrld-square-brackets-lst$)
      (error '$bracket-apply$
	     (format #f "no matching found in $ovrld-square-brackets-lst$ : failed with those arguments list ~a" args-lst))))

(define (find-getter-for-overloaded-square-brackets args-lst)
  (car (find-getter-and-setter-for-overloaded-square-brackets args-lst)))

(define (find-setter-for-overloaded-square-brackets args-lst)
  (cdr (find-getter-and-setter-for-overloaded-square-brackets args-lst)))
