;; Translated from Scheme+ for Racket (defun.rkt) for Guile.


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


;; should only allow return, not return-rec for time optimisation


(define-module (Scheme+ defun)
  #:use-module (Scheme+ racket-compat))


	

	(use-modules
	(srfi srfi-31)
	(Scheme+ nfx)
	(Scheme+ return))
	  




;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/defun)
;; > (defun (foo) (display "hello") (newline) (return) (display "world") (newline))
;; > (foo)
;; hello
;; > (return)
;; return: can only be used inside defun


;; scheme@(guile-user)> (defun (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"

;; (defun x)

;;(define return '()) ;; for debug of Typed Racket


;;(defun (foo) (return 1 2 3))

;; (foo)
;; 1
;; 2
;; 3

(define-syntax defun

  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (defun (x y z))
	;; TODO: remove? redundant with (declare x y z)
	((_ (var1 ...)) #`(begin (define var1 '()) ...))


	((_ (<name> <arg> ...) <body> <body>* ...)
	 
         
	 #'(define (<name> <arg> ...)
		 
	     (#;apply (rec <name> (lambda (<arg> ...)
				  
				  (call/cc

				   (lambda (ret-id) ;(#,ret-id)
				     ;; In the body we adjust the 'return' keyword so that calls
				     ;; to 'return' are replaced with calls to the escape
				     ;; continuation.
				     (syntax-parameterize
				      ([return (syntax-rules ()
						 [(return vals (... ...))
						  (ret-id vals (... ...))])])
				      <body>
				      <body>*
				      ...)))))
		    
		      ;;(list
		            <arg>
			    ...;)
			    )))

	

	;; variadic arguments in list
	;; (defun (foo a . L) (when #t (return (cons a L))))
	;; (foo 1 2 3)
	;; '(1 2 3)
	((_ (<name> <arg> . L) <body> <body>* ...)
	 
         
	 #'(define (<name> <arg> . L)

	     
	     (apply (rec <name> (lambda (<arg> . L)
				  
				  (call/cc

				   (lambda (ret-id) ;(#,ret-id)
				     ;; In the body we adjust the 'return' keyword so that calls
				     ;; to 'return' are replaced with calls to the escape
				     ;; continuation.
				     (syntax-parameterize
				      ([return (syntax-rules ()
						 [(return vals (... ...))
						  (ret-id vals (... ...))])])
				      <body>
				      <body>*
				      ...)))))
		    
		    (cons <arg> L))))



	;; single definition without a value assigned
	;; (defun x)
	((_ var) #`(define var '()))

	;; (defun s  3 ² + 2 * 3 * 5 + 5 ²)
	;; 64
	;; DEPRECATED (Scheme+ 18.0) : kept for the compatibility but no more documented nor improved , its infix parsing
	;; is done at expansion time ($nfx$-rec) and knows only the procedures of Racket : use (defun s {3 ² + 2 * 3 * 5 + 5 ²})
	((_ var expr expr-optional ...) #`(define var ($nfx$-rec expr expr-optional ...))) ; expr expr-optional ...))

	((_) #`(error "Bad defun form"))
	
	;; (def x 7)
	;;((_ var expr) #`(define var expr))

	;;((_ err ...) #`(syntax-error "Bad def form"))

	)))




;; (defun t 3 * (+ 2 4) - 1)
;; t
;; 17


;; (defun z (3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5))
;; z
;; 27.817919173354642


;; (defun x 1 + 2 + 3)
;; x
;; 6


;; (defun k 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)
;; k
;; 3.883381924198251


;; (defun s 3 ² + 2 * 3 * 5 + 5 ²)
;; s
;; 64










(export
	defun)
(re-export
	return
	return-rec)
 ; end library
