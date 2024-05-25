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




(define-module (if-then-else)

  ;;#:use-module (guile)

  #:use-module (srfi srfi-1) ; first ,third
  #:use-module (insert)
  #:use-module (syntax)
  
  ;;#:replace ( if )
  #:export ( if )

  ) ;; end module declaration



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
;; . . SRFI-105.rkt:181:17: if: then after else near : '(then 3 4)
;; > (if #t then 1 2 then 3 4)
;; . . SRFI-105.rkt:181:17: if: 2 then inside near: '(then 3 4)





(define (call-parse-if-args-syntax Largs) ; Largs = (test e1 ...)

  ;;(display "Largs=") (display Largs) (newline)
  (define lenL (length Largs))

  (cond ((< lenL 2)
	 (error "if: too few arguments:" Largs)))

  (define test (car Largs))
  (define e1 (cadr Largs))

  
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








(define-syntax if
  
  (lambda (stx)
    
    (syntax-case stx (then else)
      
      ((if tst ...)

       (with-syntax ((parsed-args  (call-parse-if-args-syntax #'(tst ...))))

		    (display "if : parsed-args=") (display #'parsed-args) (newline)
       		    #'parsed-args)))))




;; scheme@(guile-user)> (if #t 7)
;; WARNING: (guile-user): imported module (if-then-else) overrides core binding `if'
;; if : parsed-args=(#<syntax:if-then-else.scm:244:23 cond> (#<syntax:unknown file:2:4 #t> #<syntax:unknown file:2:7 7>))
;; $1 = 7
;; scheme@(guile-user)> (if #t then 7)
;; if : parsed-args=(#<syntax:if-then-else.scm:246:42 cond> (#<syntax:unknown file:3:4 #t> #<syntax:unknown file:3:12 7>))
;; $2 = 7
;; scheme@(guile-user)> (if #t 2 else 3)
;; if : parsed-args=(#<syntax:if-then-else.scm:307:22 cond> (#<syntax:unknown file:4:4 #t> #<syntax:unknown file:4:7 2>) (#<syntax:if-then-else.scm:308:29 else> #<syntax:unknown file:4:14 3>))
;; $3 = 2
;; scheme@(guile-user)> (if #t then 2 else 3)
;; if : parsed-args=(#<syntax:if-then-else.scm:307:22 cond> (#<syntax:unknown file:5:4 #t> #<syntax:unknown file:5:12 2>) (#<syntax:if-then-else.scm:308:29 else> #<syntax:unknown file:5:19 3>))
;; $4 = 2
;; scheme@(guile-user)> (if #f then 2 else 3)
;; if : parsed-args=(#<syntax:if-then-else.scm:307:22 cond> (#<syntax:unknown file:6:4 #f> #<syntax:unknown file:6:12 2>) (#<syntax:if-then-else.scm:308:29 else> #<syntax:unknown file:6:19 3>))
;; $5 = 3
;; scheme@(guile-user)> (if #f then 1 2 else 3 4)
;; if : parsed-args=(#<syntax:if-then-else.scm:307:22 cond> (#<syntax:unknown file:7:4 #f> #<syntax:unknown file:7:12 1> #<syntax:unknown file:7:14 2>) (#<syntax:if-then-else.scm:308:29 else> #<syntax:unknown file:7:21 3> #<syntax:unknown file:7:23 4>))
;; $6 = 4
;; scheme@(guile-user)> (if #t then 1 2 else 3 4)
;; if : parsed-args=(#<syntax:if-then-else.scm:307:22 cond> (#<syntax:unknown file:8:4 #t> #<syntax:unknown file:8:12 1> #<syntax:unknown file:8:14 2>) (#<syntax:if-then-else.scm:308:29 else> #<syntax:unknown file:8:21 3> #<syntax:unknown file:8:23 4>))
;; $7 = 2
;; scheme@(guile-user)> (if #t 1 2 3)
;; if : parsed-args=(#<syntax:if-then-else.scm:300:37 cond> (#<syntax:unknown file:9:4 #t> #<syntax:unknown file:9:7 1> #<syntax:unknown file:9:9 2> #<syntax:unknown file:9:11 3>))
;; $8 = 3
;; scheme@(guile-user)> (if #t then 1 2 else 3 4 then 5)
;; While compiling expression:
;; if: then after else near : (#<syntax:unknown file:10:25 then> #<syntax:unknown file:10:30 5>)
;; scheme@(guile-user)> (if #t then 1 2 else 3 4 else 5)
;; While compiling expression:
;; if: 2 else inside near: (#<syntax:unknown file:11:25 else> #<syntax:unknown file:11:30 5>)
;; scheme@(guile-user)> (if #t else 1 2 then 3 4)
;; While compiling expression:
;; if: then after else near : (#<syntax:unknown file:12:16 then> #<syntax:unknown file:12:21 3> #<syntax:unknown file:12:23 4>)
;; scheme@(guile-user)> (if #t then 1 2 then 3 4)
;; While compiling expression:
;; if: 2 then inside near: (#<syntax:unknown file:13:16 then> #<syntax:unknown file:13:21 3> #<syntax:unknown file:13:23 4>)
;; scheme@(guile-user)> 
