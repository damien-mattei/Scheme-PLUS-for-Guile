
(define-module (parse-if)

  #:use-module (condx)
  #:use-module (srfi srfi-1) ; first ,third
  #:use-module (insert)
  
  #:export ( then=?
	     else=?
	     call-parse-if-args)) ;; end module declaration




;; usefull procedures and macro for the next part of code
(define (then=? arg)
  (or (equal? arg 'then) (equal? arg 'THEN)))

(define (else=? arg)
  (or (equal? arg 'else) (equal? arg 'ELSE)))


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
(define (call-parse-if-args Largs) ; Largs = (test e1 ...)

  ;;(display "Largs=") (display Largs) (newline)
  (define lenL (length Largs))

  (when (< lenL 2)
	(error "if: too few arguments:" Largs))

  (define test (car Largs))
  (define e1 (cadr Largs))

  ; deal with the old 2 args 'if' but modified
  (condx ((and (= lenL 2) (then=? e1))
	  (error "if: syntax error,found (if test then) only: near " Largs))
	 ((and (= lenL 2) (else=? e1))
	  (error "if: syntax error,found (if test else) only: near " Largs))
	 ((= lenL 2) `(when ,test ,e1)) ; (if test e1)
	 (exec (define e2 (third Largs)))
	 ((and (= lenL 3) (then=? e1)) `(when ,test ; (if test then e2)
					      ,e2))
	 ((and (= lenL 3) (else=? e1)) `(unless ,test ; (if test else e2)
						,e2))
	 ((= lenL 3) `(if ,test
			  ,e1
			  ,e2))

	 (else
	  
	  (define L-then '())
	  (define L-else '())
	  (define cpt-then 0)
	  (define cpt-else 0)
	  			   
	  (define (parse-if-args L)
	    
	    (condx ((null? L) (set! L-then (reverse L-then))
		              (set! L-else (reverse L-else)))
		   
		   (exec (define ec (car L))
			 (define rstL (cdr L)))
			 		   
		   ((then=? ec) (when (= cpt-else 1)
				      (error "if: then after else near :" L))
		                (when (= cpt-then 1)
				      (error "if: 2 then inside near:" L))
		                (set! cpt-then (+ 1 cpt-then))
		                (parse-if-args rstL)) ; recurse
		   
		   ((else=? ec) (when (= cpt-else 1)
				      (error "if: 2 else inside near:" L))
		                (set! cpt-else (+ 1 cpt-else))
		                (parse-if-args rstL)) ; recurse

		   
		   ((and (>= cpt-then 1) (= cpt-else 0)) (insert-set! ec L-then)
		                                         (parse-if-args rstL)) ; recurse

		   
		   ((>= cpt-else 1) (insert-set! ec L-else)
		                    (parse-if-args rstL))  ; recurse
		   
		   (else ; start with 'then' directives but without 'then' keyword !
		    ;; i allow this syntax but this is dangerous:  risk of confusion with regular scheme syntax
		    
		    (insert-set! ec L-then)
		    
		    (set! cpt-then 1)
		    (parse-if-args rstL)))) ; recurse
	    
	    (define Lr (cdr Largs)) ; list of arguments of 'if' without the test
						    
	    (parse-if-args Lr) ; call the parsing of arguments
	    
	    (cond ((null? L-then) `(unless ,test
					   ,@L-else))
		  ((null? L-else) `(when ,test
					 ,@L-then))
		  (else `(if ,test
			     (let ()
			       ,@L-then)
			     (let ()
			       ,@L-else)))))))



;; (define (call-parse-if-args Largs) ; Largs = (test e1 ...)

;;   ;;(display "Largs=") (display Largs) (newline)
;;   (define lenL (length Largs))

;;   (cond ((< lenL 2)
;; 	 (error "if: too few arguments:" Largs)))

;;   (define test (car Largs))
;;   (define e1 (cadr Largs))

  
;;   ; deal with the old 2 args 'if' but modified
;;   (cond ((and (= lenL 2) (then=? e1))
;; 	 (error "if: syntax error,found (if test then) only: near " Largs))
;; 	((and (= lenL 2) (else=? e1))
;; 	 (error "if: syntax error,found (if test else) only: near " Largs))
;; 	((= lenL 2) `(cond (,test ,e1))) ; (if test e1)
	
;; 	 ((and (= lenL 3) (then=? e1)) `(cond (,test ; (if test then e2)
;; 					       ,(third Largs))))
;; 	 ((and (= lenL 3) (else=? e1)) `(cond ((not ,test) ; (if test else e2)
;; 					       ,(third Largs))))
;; 	 ((= lenL 3) `(cond (,test ,e1)
;; 			     (else ,(third Largs))))

;; 	 (else

;; 	  (let ()
;; 	  (define L-then '())
;; 	  (define L-else '())
;; 	  (define cpt-then 0)
;; 	  (define cpt-else 0)
	  			   
;; 	  (define (parse-if-args L)
	    
;; 	    (cond ((null? L) (set! L-then (reverse L-then))
;; 		             (set! L-else (reverse L-else)))
		   		   			 		   
;; 		   ((then=? (car L)) (cond ((= cpt-else 1)
;; 				       (error "if: then after else near :" L)))
;; 		                     (cond ((= cpt-then 1)
;; 				       (error "if: 2 then inside near:" L)))
;; 		                     (set! cpt-then (+ 1 cpt-then))
;; 		                     (parse-if-args (cdr L))) ; recurse
		   
;; 		   ((else=? (car L)) (cond ((= cpt-else 1)
;; 				       (error "if: 2 else inside near:" L)))
;; 		                     (set! cpt-else (+ 1 cpt-else))
;; 		                     (parse-if-args (cdr L))) ; recurse

		   
;; 		   ((and (>= cpt-then 1) (= cpt-else 0)) (insert-set! (car L) L-then)
;; 		                                         (parse-if-args (cdr L))) ; recurse

		   
;; 		   ((>= cpt-else 1) (insert-set! (car L) L-else)
;; 		                    (parse-if-args (cdr L)))  ; recurse
		   
;; 		   (else ; start with 'then' directives but without 'then' keyword !
;; 		    ;; i allow this syntax but this is dangerous:  risk of confusion with regular scheme syntax
		    
;; 		    (insert-set! (car L) L-then)
		    
;; 		    (set! cpt-then 1)
;; 		    (parse-if-args (cdr L))))) ; recurse


	  
	    
;; 	    (define Lr (cdr Largs)) ; list of arguments of 'if' without the test
						    
;; 	    (parse-if-args Lr) ; call the parsing of arguments
	    
;; 	    (cond ((null? L-then) `(cond ((not ,test)
;; 					  (let ()
;; 					    ,@L-else))))
;; 		  ((null? L-else) `(cond (,test
;; 					  (let () ,@L-then))))
;; 		  (else ;; `(if-scheme ,test
;; 			;; 	    (let ()
;; 			;; 	      ,@L-then)
;; 			;; 	    (let ()
;; 			;; 	      ,@L-else)))))))
;; 		   `(cond  (,test (let ()
;; 				    ,@L-then))
;; 			   (else ,@L-else)))))))) ;; with some other version or implementations we could have to use (let () ...
