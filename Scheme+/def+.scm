;; Translated from Scheme+ for Racket (def+.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2023-2025 Damien MATTEI

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



(define-module (Scheme+ def+)
  #:use-module (Scheme+ racket-compat))

	;; DEPRECATED (Scheme+ 18.0) : def+ is kept for the compatibility with old programs but it is no more documented
	;; nor improved. Its infix parsing is done at expansion time ($nfx$-rec) and knows only the procedures of Racket ,
	;; not those of the program. Use def (or define) with the curly brackets { } :
	;; (def (f x) {x * 2 + 1}) instead of (def+ (f x) (x * 2 + 1))


	 ; warning return and return-rec also provide elsewhere ! but it works....

	(use-modules
	(srfi srfi-31)
	(Scheme+ return)
	(Scheme+ nfx))


;;(def+ (foo) (when (2 < 3) (return "vrai")))

;;(foo)
;;"vrai"

;; list of the arguments from the formals of a procedure
;; (formals->list (a b)) -> (cons a (cons b '()))
;; (formals->list (a b . L)) -> (cons a (cons b L))
;; (formals->list L) -> L
(define-syntax formals->list

  (syntax-rules ()

    ((_ ()) '())
    ((_ (a . rest)) (cons a (formals->list rest)))
    ((_ r) r)))


(define-syntax def+

  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def+ (x y z))
	;; TODO: remove? redundant with (declare x y z)
	((_ (var1 ...)) #'(begin (define var1 '()) ...))

	;; function definition, with fixed, variadic or only rest arguments
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
					       ($nfx$-rec <body>)
					       ($nfx$-rec <body>*)
					       ...))))))
			  <name>)

			(formals->list <formals>)))))))


	;; single definition without a value assigned
	;; (def x)
	((_ var) #'(define var '()))

	;; (def s  3 ² + 2 * 3 * 5 + 5 ²)
	;; 64
	((_ var expr expr* ...) #'(define var ($nfx$-rec expr expr* ...)))

	((_ . err) #'(error "Bad def form"))

	)))




(export
	def+)
(re-export
	return
	return-rec)
 ; end module
