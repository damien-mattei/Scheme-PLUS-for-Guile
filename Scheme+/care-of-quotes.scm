;; Translated from Scheme+ for Racket (care-of-quotes.rkt) for Guile.

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


;; The quoted lists and the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF
;;
;; CARE-OF-QUOTES-ON (the state at the beginning of each file) : the quotes are respected, a quoted list is data ;
;; in a quoted { } only the top level is infix, its ( ) stay data (as in SRFI 105) , see infix-top-level-only.
;;
;; CARE-OF-QUOTES-OFF : a region for the symbolic computation , the quoted and quasiquoted lists are infix expressions
;; at all levels : '(x + y * z) is '(+ x (* y z)) , `(,a · x ⊕ y) is `(⊕ (· ,a x) y) ; the parts ,... and ,@... are code
;; and are not converted (only their own quoted lists are , as all the code of the region).
;;
;; A pragma acts on the elements that follow it in the same list (the body of the module, of a procedure, ...) and on
;; all that is nested in them, until the other pragma or the end of the list ; after the end of the list the state of
;; before comes back. Written at the top level of the file, before the module form, it acts on the whole module.
;; The pragmas are removed from the code.

(define-module (Scheme+ care-of-quotes)
  #:use-module (Scheme+ racket-compat))

  

  (use-modules
	((Scheme+ infix-with-precedence-to-prefix) #:select (!*prec-generic-infix-parser-rec-prepare))
	((Scheme+ source-locations) #:select (link-cell!))
	((Scheme+ operators) #:select (QUOTE-op? QUASIQUOTE-op? SYNTAX-QUOTE-op?)))


  (define care-of-quotes-on-symbol 'CARE-OF-QUOTES-ON)
  (define care-of-quotes-off-symbol 'CARE-OF-QUOTES-OFF)

  (define (care-of-quotes-marker? x)
    (or (eq? x care-of-quotes-on-symbol)
	(eq? x care-of-quotes-off-symbol)))

  ;; #t : the quotes are respected (data) , #f : the quoted lists are infix expressions
  (define care-of-quotes (make-parameter #t))


  ;; forms : the forms read in a file ; each file starts with the quotes respected
  (define (process-care-of-quotes forms)
    (parameterize ((care-of-quotes #t))
      (process-list forms)))

  ;; the state of the REPL : the last pragma read acts on the rest of the session
  (define repl-care-of-quotes #t)

  (define (process-care-of-quotes-repl datum)
    (cond ((care-of-quotes-marker? datum)
	   (set! repl-care-of-quotes (eq? datum care-of-quotes-on-symbol))
	   '(void))
	  (else
	   (parameterize ((care-of-quotes repl-care-of-quotes))
	     (process-element datum)))))


  ;; a list of code : a marker changes the state for the elements that follow it in the list , and is removed ;
  ;; the parameterize ends with the list , so the state of before comes back
  (define (process-list lst)
    (cond ((null? lst) '())
	  ((not (pair? lst)) ; the end of an improper list (a . b)
	   (process-element lst))
	  ((care-of-quotes-marker? (car lst))
	   (parameterize ((care-of-quotes (eq? (car lst) care-of-quotes-on-symbol)))
	     (process-list (cdr lst))))
	  (else
	   (let ((e (process-element (car lst))))
	     #;(cons e
		   (process-list (cdr lst)))
	     (link-cell! (cons e ; the copy keeps the locations of the source
			       (process-list (cdr lst)))
			 lst)))))

  (define (process-element e)
    (cond ((not (pair? e)) e) ; an atom
	  ((SYNTAX-QUOTE-op? (car e)) e) ; #'... #`... (quote-syntax ...) : templates of macros , never converted
	  ((quote-form? e)
	   (if (care-of-quotes)
	       e ; data
	       (list (car e) (infix-quoted (cadr e)))))
	  ((quasiquote-form? e)
	   (if (care-of-quotes)
	       e ; data
	       (list (car e) (infix-quasiquoted (cadr e)))))
	  (else
	   (process-list e)))) ; code

  (define (quote-form? e) ; (quote X)
    (and (QUOTE-op? (car e))
	 (pair? (cdr e))
	 (null? (cddr e))))

  (define (quasiquote-form? e) ; (quasiquote X)
    (and (QUASIQUOTE-op? (car e))
	 (pair? (cdr e))
	 (null? (cddr e))))

  (define (unquote-form? e) ; (unquote E) or (unquote-splicing E)
    (and (pair? e)
	 (memq (car e) '(unquote unquote-splicing))
	 (pair? (cdr e))
	 (null? (cddr e))))


  ;; x : a quoted datum in a region CARE-OF-QUOTES-OFF , an infix expression converted at all levels
  (define (infix-quoted x)
    (if (pair? x)
	(!*prec-generic-infix-parser-rec-prepare x
						 (lambda (op a b) (list op a b))) ; creator
	x))

  ;; x : a quasiquoted datum in a region CARE-OF-QUOTES-OFF : the parts (unquote E) and (unquote-splicing E) are code,
  ;; they are replaced by placeholders during the conversion of x , then put back
  (define (infix-quasiquoted x)
    (define parts '()) ; ((placeholder . unquoted-form) ...)
    (define (protect y)
      (cond ((not (pair? y)) y)
	    ((unquote-form? y)
	     (let ((p (new-placeholder)))
	       (set! parts (cons (cons p (list (car y) (process-element (cadr y)))) ; E is code of the region
				 parts))
	       p))
	    ((QUASIQUOTE-op? (car y)) y) ; a nested quasiquote : an other level of quotation , not converted
	    (else
	     (cons (protect (car y))
		   (protect (cdr y))))))
    (let ((converted (infix-quoted (protect x)))) ; first : protect fills parts
      (put-back parts converted)))

  ;; lyst : the elements of a quoted { } with several operators (called by the readers SRFI-105 and SRFI-110) :
  ;; only the top level is infix , the sub-lists stay as they are (a { } inside is already converted by the reader)
  (define (infix-top-level-only lyst)
    (define parts '()) ; ((placeholder . sub-list) ...)
    (define protected
      (map (lambda (e)
	     (if (pair? e)
		 (let ((p (new-placeholder)))
		   (set! parts (cons (cons p e) parts))
		   p)
		 e))
	   lyst))
    (put-back parts
	      (!*prec-generic-infix-parser-rec-prepare protected
						       (lambda (op a b) (list op a b)))))


  ;; the placeholders are symbols that no program uses
  (define placeholder-counter 0)

  (define (new-placeholder)
    (set! placeholder-counter (+ 1 placeholder-counter))
    (string->symbol (string-append "$care-of-quotes-placeholder-" (number->string placeholder-counter) "$")))

  (define (put-back parts y)
    (cond ((symbol? y)
	   (let ((p (assq y parts)))
	     (if p (cdr p) y)))
	  ((pair? y)
	   (cons (put-back parts (car y))
		 (put-back parts (cdr y))))
	  (else y)))

  

(export
	process-care-of-quotes
	process-care-of-quotes-repl
	infix-top-level-only)
 ; end module
