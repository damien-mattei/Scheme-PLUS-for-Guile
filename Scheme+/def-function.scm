;; Translated from Scheme+ for Racket (def-function.rkt) for Guile.


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



;; for internal use only , see def for the extern use

(define-module (Scheme+ def-function)
  #:use-module (Scheme+ racket-compat))


	
	
	(use-modules
	(srfi srfi-31)
	(Scheme+ return))
	  




;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/def-function)
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


(define-syntax def

  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def (x y z))
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
					       <body> <body>* ...))))))
			  <name>)

			(formals->list <formals>)))))))


	;; single definition without a value assigned
	;; (def x)
	((_ var) #'(define var '()))

	;; (def x 7)
	((_ var expr) #'(define var expr))

	((_ . err) #'(error "Bad def form"))

	)))






(export
	def)
(re-export
	return
	return-rec)
 ; end module
