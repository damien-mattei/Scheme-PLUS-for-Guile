;; Translated from Scheme+ for Racket (if-parser.rkt) for Guile.

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



(define-module (Scheme+ if-parser)
  #:use-module (Scheme+ racket-compat))

  
  
  (use-modules
	(srfi srfi-1)
	(Scheme+ declare)
	(Scheme+ insert)
	(Scheme+ syntax))
  

;; usefull procedures and macro for the next part of code
(define (then=? arg)

  (or (datum=? arg 'then)
      (datum=? arg 'THEN)))

  ;; (or (equal? arg 'then)
  ;;     (equal? arg 'THEN)
  ;;     (check-syntax=? #'then arg)
  ;;     (check-syntax=? #'THEN arg)))

(define (else=? arg)

  (or (datum=? arg 'else)
      (datum=? arg 'ELSE)))

  ;; (or (equal? arg 'else)
  ;;     (equal? arg 'ELSE)
  ;;     (check-syntax=? #'else arg)
  ;;     (check-syntax=? #'ELSE arg)))


;; > (if #f else 3)
;; 3
;; > (if #t else 3)
;; > (if #t 2 else 3)
;; 2
;; > (if #t then 2 else 3)
;; 2
;; > (if #f then 2 else 3)
;; 3

;; > (if #f then 1 2 else 3 4)
;; if : parsed-args=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/if-parser.rkt:174:21 (cond (#f (let () 1 2)) (else...>
;; 4

;; > (if #t then 1 2 else 3 4)
;; 2
;; > (if #t 1 2 3)
;; 3
;; > (if #t then 1 2 else 3 4 then 5)
;; . . SRFI-105.rkt:181:17: if: then after else near : '(then 5)
;; > (if #t then 1 2 else 3 4 else 5)
;; . . SRFI-105.rkt:181:17: if: 2 else inside near: '(else 5)

;; > (if #t else 1 2 then 3 4)
;; . . Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/if-parser.rkt:129:39: if: then after else near : '(#<syntax:2-interactions from an unsaved editor:10:18 then> #<syntax:2-interactions from an unsaved editor:10:23 3> #<syntax:2-interactions from an unsaved editor:10:25 4>)
;; >

;; > (if #t then 1 2 then 3 4)
;; . . SRFI-105.rkt:181:17: if: 2 then inside near: '(then 3 4)


;; #|kawa:1|# (import (if-then-else))
;; #|kawa:2|# (if (< 2 3) then "2 < 3" else "error")
;; 2 < 3


;; a block written as a sub-list , (then e ...) or (else e ...) , is put back flat : then e ...
;; in sweet-expressions (SRFI 110) the indentation gives sub-lists :
;; if {x < 0}
;;   then
;;     'negative
;;   else
;;     'positive
;; is read (if (< x 0) (then 'negative) (else 'positive))
;; > (if #f (then 1 2) (else 3 4))
;; 4
;; > (if #t then 1 (else 2))
;; 1
(define (flatten-then-else-blocks Lr) ; Lr : the arguments of 'if' without the test
  (append-map (lambda (arg)
		(let ((l (and (syntax? arg)
			      (syntax->list arg))))
		  (if (and l
			   (not (null? l))
			   (or (then=? (car l))
			       (else=? (car l))))
		      l ; then e ... or else e ...
		      (list arg))))
	      Lr))


;;(define (call-parse-if-args-syntax Largs) ; Largs = (test e1 ...)
(define (call-parse-if-args-syntax Largs-with-blocks) ; Largs-with-blocks = (test e1 ...) , e1 ... can be blocks (then ...) (else ...)

  ;; the blocks (then e ...) and (else e ...) are put back flat , the test is kept as it is
  (define Largs (if (null? Largs-with-blocks)
		    Largs-with-blocks
		    (cons (car Largs-with-blocks)
			  (flatten-then-else-blocks (cdr Largs-with-blocks)))))

  ;;(display "Largs=") (display Largs) (newline)
  (define lenL (length Largs))

  (declare test e1)
  
  (cond ((< lenL 2)
	 (error "if: too few arguments:" Largs)))

  (set! test (car Largs))
  (set! e1 (cadr Largs))

  
  ; deal with the old 2 args 'if' but modified
  (cond ((and (= lenL 2) (then=? e1))
	 (error "if: syntax error,found (if test then) only: near " Largs))
	((and (= lenL 2) (else=? e1))
	 (error "if: syntax error,found (if test else) only: near " Largs))
	((= lenL 2) #`(cond (#,test #,e1))) ; (if test e1)
	
	 ((and (= lenL 3) (then=? e1)) #`(cond (#,test ; (if test then e2)
						#,(third Largs))))
	 ((and (= lenL 3) (else=? e1)) #`(cond ((not #,test) ; (if test else e2)
						 #,(third Largs))))
	 ((= lenL 3) #`(cond (#,test #,e1)
			     (else #,(third Largs))))

	 (else

	  (let ()
	    (define L-then '())
	    (define L-else '())
	    (define cpt-then 0)
	    (define cpt-else 0)
	    
	    (define (parse-if-args L)
	    
	      (cond ((null? L) (set! L-then (reverse L-then))
		             (set! L-else (reverse L-else)))
		   		   			 		   
		    ((then=? (car L)) (cond ((= cpt-else 1)
				       (error "if: then after else near :" L)))
		                     (cond ((= cpt-then 1)
				       (error "if: 2 then inside near:" L)))
		                     (set! cpt-then (+ 1 cpt-then))
		                     (parse-if-args (cdr L))) ; recurse
		   
		   ((else=? (car L)) (cond ((= cpt-else 1)
				       (error "if: 2 else inside near:" L)))
		                     (set! cpt-else (+ 1 cpt-else))
		                     (parse-if-args (cdr L))) ; recurse

		   
		   ((and (>= cpt-then 1) (= cpt-else 0)) (insert-set! (car L) L-then)
		                                         (parse-if-args (cdr L))) ; recurse

		   
		   ((>= cpt-else 1) (insert-set! (car L) L-else)
		                    (parse-if-args (cdr L)))  ; recurse
		   
		   (else ; start with 'then' directives but without 'then' keyword !
		    ;; i allow this syntax but this is dangerous:  risk of confusion with regular scheme syntax
		    
		    (insert-set! (car L) L-then)
		    
		    (set! cpt-then 1)
		    (parse-if-args (cdr L))))) ; recurse
	    
	    (define Lr (cdr Largs)) ; list of arguments of 'if' without the test
						    
	    (parse-if-args Lr) ; call the parsing of arguments
	    
	    (cond ((null? L-then) #`(cond ((not #,test)
					   (let ()
					     #,@L-else))))
		  ((null? L-else) #`(cond (#,test
					   (let ()
					     #,@L-then))))
		  (else ;; `(if-scheme ,test
			;; 	    (let ()
			;; 	      ,@L-then)
			;; 	    (let ()
			;; 	      ,@L-else)))))))
		   #`(cond  (#,test (let ()
				      #,@L-then))
			    (else
			     (let ()
			       #,@L-else)))))))))




(export
	then=?
	else=?
	call-parse-if-args-syntax)
 ; end library

