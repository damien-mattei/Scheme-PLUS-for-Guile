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

; version 17

(define-module (Scheme+ defvar)

  #:export (if-defined
	    <+ +>
	    ⥆ ⥅
	    :+ +:
	    ))
  


;; tested with assignment : do not works as in Racket
(define-syntax if-defined
    (lambda (stx)
      (syntax-case stx ()
	[(_ id iftrue iffalse)
	 '(let ([exist-id (defined? (syntax->datum #'id))])
	   ;;(display "id=") (display #'id) (newline)
	   ;;(display "if-defined : exist-id=") (display exist-id) (newline) (newline)
	   (if exist-id #'iftrue #'iffalse))])))




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
