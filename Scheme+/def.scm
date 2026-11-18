;; Translated from Scheme+ for Racket (def.rkt) for Guile.


;; This file is part of Scheme+

;; Copyright 2021-2025 Damien MATTEI

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





(define-module (Scheme+ def)
  #:use-module (Scheme+ racket-compat))


	

	(use-modules
	(srfi srfi-31)
	(Scheme+ nfx)
	(Scheme+ return))
	  




;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/def)
;; > (def (foo) (display "hello") (newline) (return) (display "world") (newline))
;; > (foo)
;; hello
;; > (return)
;; return: can only be used inside def


;; scheme@(guile-user)> (def (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"

;; (def x)

;;(define return '()) ;; for debug of Typed Racket


;;(def (foo) (return 1 2 3))

;; (foo)
;; 1
;; 2
;; 3

;; list of the arguments from the formals of a procedure
;; (formals->list (a b)) -> (cons a (cons b '()))
;; (formals->list (a b . L)) -> (cons a (cons b L))
;; (formals->list L) -> L
(define-syntax formals->list

  (syntax-rules ()

    ((_ ()) '())
    ((_ (a . rest)) (cons a (formals->list rest)))
    ((_ r) r)))


;; the symbol sym is written in the datum d (the body of a def) : the call/cc of return and return-rec are made only
;; when they are used (a return produced by another macro of the body is not seen)
(eval-when (expand load eval)
  (define (uses-symbol? sym d)
    (cond ((eq? d sym) #t)
	  ((pair? d) (or (uses-symbol? sym (car d))
			 (uses-symbol? sym (cdr d))))
	  ((vector? d) (uses-symbol? sym (vector->list d)))
	  (else #f))))


(define-syntax def

  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def (x y z))
	;; TODO: remove? redundant with (declare x y z)
	((_ (var1 ...)) #'(begin (define var1 '()) ...))

	;; function definition without return nor return-rec in its body : a define , no call/cc (17 times faster for
	;; a call , and the procedure can be inlined)
	((_ (<name> . <formals>) <body> <body>* ...)
	 (let ((body (syntax->datum #'(<body> <body>* ...))))
	   (not (or (uses-symbol? 'return body)
		    (uses-symbol? 'return-rec body))))
	 #'(define (<name> . <formals>)
	     <body> <body>* ...))

	;; with return but without return-rec : one call/cc by call , as lambda+
	((_ (<name> . <formals>) <body> <body>* ...)
	 (not (uses-symbol? 'return-rec (syntax->datum #'(<body> <body>* ...))))
	 #'(define (<name> . <formals>)
	     (call/cc
	      (lambda (ret-id)
		(syntax-parameterize
		 ([return (syntax-rules ()
			    [(return vals (... ...))
			     (ret-id vals (... ...))])])
		 <body> <body>* ...)))))

	;; function definition, with fixed, variadic or only rest arguments , with return-rec in its body
	;; (def (foo a b . L) (when #t (return (list a b L))))
	;; (foo 1 2 3 4)
	;; '(1 2 (3 4))
	((_ (<name> . <formals>) <body> <body>* ...)

	 #'(define (<name> . <formals>)

	     (call/cc

	      (lambda (ret-rec-id)
		;; In the body we adjust the 'return-rec' keyword so that calls
		;; to 'return-rec' are replaced with calls to the escape
		;; continuation.

		(syntax-parameterize
		 ([return-rec (syntax-rules ()
				[(return-rec vals (... ...))
				 (ret-rec-id vals (... ...))])])

		 (apply (letrec ((<name> (lambda <formals>

					   (call/cc

					    (lambda (ret-id)
					      ;; In the body we adjust the 'return' keyword so that calls
					      ;; to 'return' are replaced with calls to the escape
					      ;; continuation.
					      (syntax-parameterize
					       ([return (syntax-rules ()
							  [(return vals (... ...))
							   (ret-id vals (... ...))])])
					       <body> <body>* ...))))))
			  <name>)

			(formals->list <formals>)))))))


	;; single definition without a value assigned
	;; (def x)
	((_ var) #'(define var '()))

	;; (def s  3 ² + 2 * 3 * 5 + 5 ²)
	;; 64
	;; DEPRECATED (Scheme+ 18.0) : kept for the compatibility but no more documented nor improved , its infix parsing
	;; is done at expansion time ($nfx$-rec) and knows only the procedures of Racket : use (def s {3 ² + 2 * 3 * 5 + 5 ²})
	((_ var expr expr* ...) #'(define var ($nfx$-rec expr expr* ...)))

	((_ . err) #'(error "Bad def form"))

	)))




;; (def t 3 * (+ 2 4) - 1)
;; t
;; 17


;; (def z (3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5))
;; z
;; 27.817919173354642


;; (def x 1 + 2 + 3)
;; x
;; 6


;; (def k 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)
;; k
;; 3.883381924198251


;; (def s 3 ² + 2 * 3 * 5 + 5 ²)
;; s
;; 64










(export
	def)
(re-export
	return
	return-rec)
 ; end module
