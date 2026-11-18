;; Translated from Scheme+ for Racket (program-type.rkt) for Guile.

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

;; try to return the type of a callable object (procedure,macro..)

(define-module (Scheme+ program-type)
  #:use-module (Scheme+ racket-compat))


	

	(use-modules
	((srfi srfi-13) #:select (string-contains))
	(Scheme+ def-function)
	((Scheme+ operators-list) #:select (member-in-executables-lst))
	((Scheme+ syntax) #:select (secure-syntax->datum)))




	;; known-program? is kept a plain procedure, on purpose, NOT a macro.
	;;
	;; A macro would not help: (known-program? gag), with gag a bare, unquoted identifier
	;; that is nowhere bound, can never work inside a *compiled* program (a Racket module,
	;; or any compiled Scheme) -- macro or procedure makes no difference. A compiled module
	;; must statically resolve, at compile time, every identifier it references as an
	;; expression; an identifier bound nowhere is a *compile-time* error ("gag: unbound
	;; identifier"), which aborts compiling the whole module before any code -- including
	;; a macro-expanded defatalize/with-handlers -- ever runs. Substituting gag into a macro
	;; template changes nothing: it is still the very same unbound identifier, still resolved
	;; statically by the compiler at the call site.
	;; This only "works" (gracefully returning #f instead of failing to compile) at the
	;; interactive REPL/top-level, which specifically defers unbound-*variable* checks to
	;; runtime -- a property of the REPL, unrelated to macro vs procedure, and irrelevant to
	;; compiled code. Since every real caller in this codebase already passes a quoted symbol
	;; or a bound variable holding a datum (see quote-known-program?,
	;; more-generic-known-program?, generic-known-program? in infix-prefix-postfix.rkt),
	;; never a bare unquoted identifier, this limitation is never actually hit in practice.
	(def (known-program? var1)

	  (when (list? var1) ; var1 could be an infix expression like '(x + y): avoid evaluating arbitrary data as code
	    (return #f))

	  ;; a procedure or a macro of the program, unknown by Racket : defined in the file, typed with a type of procedure,
	  ;; overloaded (see parser-declarations.rkt)
	  (when (member-in-executables-lst (secure-syntax->datum var1))
	    (return #t))

	  ;; Guile : a symbol is looked up in the module (guile) , the base language , as Racket evaluates it in a
	  ;; namespace of racket/base : #t for a procedure , 'macro for a macro or a syntactic keyword , #f otherwise
	  (define d (secure-syntax->datum var1))
	  (if (symbol? d)
	      (let ((v (module-variable (resolve-module '(guile)) d)))
		(cond ((not (and v (variable-bound? v))) #f)
		      (((@ (guile) macro?) (variable-ref v)) 'macro) ; the macro? of Guile , not the one of this module
		      (else (procedure? (variable-ref v)))))
	      (procedure? d)))


        ;; (quote-known-program? '+)
	;; #t

	;; (quote-known-program? 'foo)
	;; (exn:fail:contract:variable "foo: undefined;\n cannot reference an identifier before its definition" #<continuation-mark-set> 'foo)


	;; macro versions are not usefull

	;; (define-syntax quote-known-program?
	;;   (syntax-rules ()
	;;     ((_ kwot-var1)
	;;      ;; (defatalize (program? (eval kwot-var1
	;;      ;; 				   (make-base-namespace) ;(current-namespace)
	;;      ;; 				   )))
	;;      (known-program? kwot-var1))))


	(define (quote-known-program? kwot-var1)
	  (known-program? kwot-var1))
	
        
	;; (define-syntax syntax-known-program?
	;;   (syntax-rules ()
	;;     ((_ stx-var1)
	;;      (begin
	;;        (display "syntax-known-program?") (newline)
	;;        (known-program? (syntax->datum stx-var1))))))

	(define (syntax-known-program? stx-var1)
	  ;;(display "syntax-known-program?") (newline)
	  (known-program? (syntax->datum stx-var1)))

	;; warning: (list not allowed)
	;; (generic-known-program? '(x + y))
	;; fate=#(struct:exn:fail:contract:variable x: undefined;
	;;  cannot reference an identifier before its definition #<continuation-mark-set> x)
	;; #f

	;;(generic-known-program? '(x or y))
	;; #f
	
	;; this generic will choose which one from the other macro to call
	;;
	;; generic-known-program? vs more-generic-known-program?
	;; -------------------------------------------------------
	;; Both macros are identical except for their single-argument fallback clause
	;; (the (kwot-or-syntax var1) clause, shared below, behaves the same in both):
	;;   generic-known-program?      : (known-program? var1)           -- evaluates var1
	;;   more-generic-known-program? : (known-program? (quote var1))   -- quotes var1
	;; That difference matters only for a BARE argument (neither 'quoted nor #'syntax) and
	;; gives genuinely different results depending on what that argument is:
	;;
	;; * var1 is a variable holding a value (the normal case: testing what a variable
	;;   contains, e.g. a token grabbed while parsing, as infix-prefix-postfix.rkt does) :
	;;   use generic-known-program?, which evaluates var1 to get at that value.
	;;     (define f 'and)
	;;     (generic-known-program? f)      -> 'macro  (correctly tests the SYMBOL f holds)
	;;     (more-generic-known-program? f) -> #f       (wrong here: quotes the identifier f
	;;                                                   itself, i.e. tests the literal
	;;                                                   symbol 'f, ignoring what f holds)
	;;
	;; * var1 is a syntactic keyword written directly at the call site (and, or, ...) :
	;;   use more-generic-known-program?, since evaluating a syntactic keyword as an
	;;   expression is a *compile-time* error -- quoting it first avoids that.
	;;     (generic-known-program? and)      -> does not even compile: "and: bad syntax"
	;;     (more-generic-known-program? and) -> 'macro
	;;
	;; * var1 is already a global, always-bound identifier (+, -, ...), or the argument is
	;;   already wrapped in 'quote/#'syntax : both macros agree (evaluating a global gives
	;;   its value directly ; the (kwot-or-syntax var1) clause is shared).
	;;     (generic-known-program? +)      -> #t
	;;     (more-generic-known-program? +) -> #t
	;;
	;; infix-prefix-postfix.rkt always calls generic-known-program? on a variable holding a
	;; parsed token, never on a bare keyword written in source -- the right choice there.
	(define-syntax generic-known-program?
	  (syntax-rules ()
	    ((_ (kwot-or-syntax var1))
	     (cond ((eq? (quote quote) (syntax->datum #'kwot-or-syntax))
		    (quote-known-program? (kwot-or-syntax var1)))

		   ((eq? (quote syntax) (syntax->datum #'kwot-or-syntax))
		    ;;(display "generic-known-program? : syntax detected.") (newline)
		    (syntax-known-program? (kwot-or-syntax var1)))

		   (else
		    (error "generic-known-program? : find not syntax ,nor quote but:" (quote kwot-or-syntax) ))))

	    ((_ var1)
	     (known-program? var1))))

	(define-syntax more-generic-known-program? ; works also with macro at REPL,runtime, not only parsing:
	  ; (more-generic-known-program? and) --> 'macro (instead generic-known-program will issue 'bad syntax' error
	  (syntax-rules ()
	    ((_ (kwot-or-syntax var1))
	     (cond ((eq? (quote quote) (syntax->datum #'kwot-or-syntax))
		    (quote-known-program? (kwot-or-syntax var1)))

		   ((eq? (quote syntax) (syntax->datum #'kwot-or-syntax))
		    ;;(display "generic-known-program? : syntax detected.") (newline)
		    (syntax-known-program? (kwot-or-syntax var1)))

		   (else
		    (error "generic-known-program? : find not syntax ,nor quote but:" (quote kwot-or-syntax) ))))

	    ((_ var1)
	     (known-program? (quote var1)))))

	;; (macro? and) -> #t ; only works because macro? goes through more-generic-known-program?,
	;; which quotes its argument -- see the comment above generic-known-program?
	(define-syntax macro?
	  (syntax-rules ()
	    ((_ var) (eq? 'macro (more-generic-known-program? var)))))

	; test for (lambda ....)
	(define (lambda-list? L)
	  (and (list? L)
	       (not (null? L))
	       (or (eq? (car L) 'lambda)
		   (eq? (car L) 'λ))))
	
	

(export
	known-program?
	syntax-known-program?
	quote-known-program?
	generic-known-program?
	more-generic-known-program?
	lambda-list?)
(eval-when (expand load eval)
  (module-replace! (current-module) '(macro?)))
 ; end module



;; (define v (vector 1 2 3))

;; (vector-syntax? (syntax v) (current-namespace)) ; 1

;; (define (foo) 7)

;; (syntax-known-program? (syntax foo)) ; 2

;; (syntax-known-program? (syntax +)) ; 3

;; (known-program? +)
;; #t

;; (vector-syntax? (syntax v) (make-base-namespace))  ; 5



;; #lang reader SRFI-105
;; (module repl racket

;;   (provide (all-defined-out)) 
;;   (require Scheme+)
;;   (require Scheme+/program-type)
  
;;   ;; put your code here or simply use the REPL

;;   (define (foo) 7)
;;   (generic-known-program? foo)

;;   )
