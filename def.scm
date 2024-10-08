
;; This file is part of Scheme+

;; Copyright 2021-2024 Damien MATTEI

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


(define-module (def)

  ;;#:use-module ((guile)) ;; pourquoi (( et ))

  #:use-module (srfi srfi-31) ;; rec

  #:export (def return return-rec
	    <+ +>
	    ⥆ ⥅
	    :+ +:))
  



;; scheme@(guile-user)> (defined-symbol? x)
;;$1 = #f
;; (define-syntax defined-symbol?              
;;   (syntax-rules ()
    
;;     ((_ x) (defined? (quote x)))))

;; Warning: does not works in all context ! (limitation/bug in Guile) :
;; scheme@(guile-user)> (let () (define x 3) (defined? 'x))
;; $1 = #f ;; !!!!!!


;; scheme@(guile-user)> (defined-symbol? d)
;; ;;; <stdin>:20:17: warning: possibly unbound variable `d'
;; $3 = #f
;; (define-syntax defined-symbol?
;;   (syntax-rules ()
;;     ((_ x) (call-with-current-continuation 
;; 	    (lambda (exit)
;; 	      (with-exception-handler
;; 	       (lambda (e)
;; 		 (display "defined-symbol? : undefined") (newline)
;; 		 (exit #f)) ; eval failed => not defined
;; 	       (lambda ()
;; 		 (eval x (interaction-environment))
;; 		 #t))))))) ; eval suceeded => defined


;; scheme@(guile-user)> (define r 2)
;; scheme@(guile-user)> (if-defined r 'defined (define r 7))
;; if-defined : where=#t
;; $16 = defined
;; scheme@(guile-user)> (if-defined t3 'defined (define t3 7))
;; if-defined : where=#f
;; scheme@(guile-user)> t3
;; $17 = 7
;; scheme@(guile-user)> (if-defined z (list z) 'not-defined)
;; if-defined : where=#f
;; $18 = not-defined
;; (define-syntax if-defined
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((_ id iftrue iffalse)
;;        (let ((where (defined-symbol? #'id))) ;;(quote id))))
;; 	 (display "if-defined : where=") (display where) (newline)
;; 	 (display "id=") (display #'id) (newline)
;; 	 (if where #'iftrue #'iffalse))))))


(define-syntax-parameter return
  (lambda (stx)
    (syntax-violation 'return "used outside of a def" stx)))

(define-syntax-parameter return-rec
  (lambda (stx)
    (syntax-violation 'return-rec "used outside of a def" stx)))



;; scheme@(guile-user)> (def (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"

;; (def x)



;; (def (foo n)
;;      (cond ((= n 0) 'end0)
;; 	   ((= n 7) (return 'end7))
;; 	   (else (cons n (foo {n - 1})))))


;; scheme@(guile-user)> (foo 5)
;; (5 4 3 2 1 . end0)
;; scheme@(guile-user)> (foo 10)
;; (10 9 8 . end7)

;; (def (bar n)
;;      (cond ((= n 0) 'end0)
;; 	   ((= n 7) (return-rec 'end7))
;; 	   (else (cons n (bar {n - 1})))))

;; scheme@(guile-user)> (bar 5)
;; $4 = (5 4 3 2 1 . end0)
;; scheme@(guile-user)> (bar 10)
;; $5 = end7
(define-syntax def
  
  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def (x y z))
	((_ (var1 ...)) #`(begin (define var1 '()) ...))
	
	;;  (def (foo) (when #t (return "hello") "bye"))
        ;; ((_ (<name> <arg> ...) <body> <body>* ...)
        ;;  (let ((ret-id (datum->syntax stx 'return)))
        ;;    #`(define (<name> <arg> ...)
	;;        (call/cc (lambda (#,ret-id) <body> <body>* ...)))))

	
	;; ((_ (<name> <arg> ...) <body> <body>* ...)
	 
        ;;  (let ((ret-id (datum->syntax stx 'return))
	;;        (ret-rec-id (datum->syntax stx 'return-rec)))

	;;    #`(define (<name> <arg> ...)

	;;        (call/cc (lambda (#,ret-rec-id)
			  
	;; 		 (apply (rec <name> (lambda (<arg> ...)
	;; 				      (call/cc (lambda (#,ret-id) <body> <body>* ...)))) (list <arg> ...)))))))


	((_ (<name> <arg> ...) <body> <body>* ...)
	 
	 #'(define (<name> <arg> ...)

	     (call/cc

	      (lambda (ret-rec-id) ;(#,ret-rec-id)

		(syntax-parameterize
		 ([return-rec (syntax-rules ()
				[(return-rec vals (... ...))
				 (ret-rec-id vals (... ...))])])
		 
		 (apply (rec <name> (lambda (<arg> ...)
				      
				      (call/cc

				       (lambda (ret-id) ;(#,ret-id)
					 ;; In the body we adjust the 'return' keyword so that calls
					 ;; to 'return' are replaced with calls to the escape
					 ;; continuation.
					 (syntax-parameterize
					  ([return (syntax-rules ()
						     [(return vals (... ...))
						      (ret-id vals (... ...))])])
					  <body> <body>* ...)))))
			
			(list <arg> ...)))))))

	      

	
	;; single definition without a value assigned
	;; (def x)
	((_ var) #`(define var '()))

	;; (def x 7)
	((_ var expr) #`(define var expr))

	((_ err ...) #`(syntax-error "Bad def form"))

	)))



;; definition and assignment
;; { x <+ 7 } is equivalent to : (<- x 7) or (define x 7)

;; > {(a b c) <+ (values 7 8 9)}
;; 7
;; 8
;; 9
;; > (list a b c)
;; '(7 8 9)

;; > { y <+ z <+ 7 } 
;; > z
;; 7
;; > y
;; 7
;; > { x <+ y <+ z <+ 7 } 
;; > (list x y z)
;; '(7 7 7)

;; > {(x y z) <+ (u v w) <+ (a b c)  <+ (values 2 4 5)}
;; 2
;; 4
;; 5
;; > (list x y z u v w a b c)
;; '(2 4 5 2 4 5 2 4 5)
(define-syntax <+
  (syntax-rules ()
    
    ((_ (var1 ...) expr) 
     (define-values (var1 ...) expr)
     )
			 
    ;; (begin
    ;;   (define var1 '())
    ;;   ...
    ;;   ;;(display "<+ multiple") (newline)
    ;;   (set!-values (var1 ...) expr)))

    ;; > {(x y z) <+ (u v w) <+ (a b c)  <+ (values 2 4 5)}
    ;; 2
    ;; 4
    ;; 5
    ;; > (list x y z u v w a b c)
    ;; '(2 4 5 2 4 5 2 4 5)
    ((_ (var10 ...) (var11 ...) ... expr) (begin  ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result 
					    (define-values (var10 ...) expr)
					    (define-values (var11 ...) (values var10 ...))
					    ...
					    ))
					   
    
    ((_ var expr) (define var expr))
		  
    
     ;; > { y <+ z <+ 7 }
     ;; 7
     ;; > z
     ;; 7
     ;; > y
     ;; 7
     ;; > { x <+ y <+ z <+ 7 }
     ;; 7
     ;; > (list x y z)
     ;; '(7 7 7)
     ((_ var var1 ... expr) (begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result 
			      (define var expr)
			      (define var1 var)
			      ...
			      ;;var))
			      ))
    
     ))
		 			

(define-syntax ⥆
  (syntax-rules ()

     ((_ var ...) (<+ var ...))))



;; scheme@(guile-user)> {x :+ 7}
;; scheme@(guile-user)> x
;; $1 = 7
;; scheme@(guile-user)> {x := "hello"}
;; scheme@(guile-user)> x
;; $2 = "hello"

(define-syntax :+
  (syntax-rules ()

    ((_ var ...) (<+ var ...))))




;; > {(values 2 4 5) +> (x y z) +> (u v w) +> (a b c)} 
;; 2
;; 4
;; 5
(define-syntax +>
  (syntax-rules ()

    ((_ exp var ...) (<+ var ... exp)))) 


(define-syntax +:
  (syntax-rules ()

    ((_ exp var ...) (<+ var ... exp))))


   
;; > {(values 2 4 5) ⥅ (x y z) ⥅ (u v w) ⥅ (a b c)} 
;; 2
;; 4
;; 5
;; > (list x y z u v w a b c)
;; '(2 4 5 2 4 5 2 4 5)
(define-syntax ⥅
  (syntax-rules ()

     ((_ expr ...) (+> expr ...))))
