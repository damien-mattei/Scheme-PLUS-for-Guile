;; overload
;; use with Scheme+:
;;(use-modules (Scheme+))


;; DEPRECATED
;;(use-modules (overload))

;; (define-module (overload)
;;   #:use-module ((guile))
;; ;;  #:use-module (srfi srfi-69 ) ;; Basic hash tables 
;;   #:use-module (srfi srfi-1) ;; any,every
;;   #:export (;;%ovrldht%
;; 	    overload
;; 	    ;; check-if-is-a-function-mapping
;; 	    ;;check-arguments
;; 	    overload-procedure
;; 	    overload-operator
;; 	    overload-function
;; 	    ))




;; scheme@(guile-user)> (use-modules (Scheme+))
;; scheme@(guile-user)> (define (add-vect-vect v1 v2) (map + v1 v2))
;; scheme@(guile-user)> (overload + add-vect-vect (list? list?) 'operator)
;; create-overloaded-operator : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; funct: #<procedure add-vect-vect (v1 v2)>
;; orig-funct: #<procedure + (#:optional _ _ . _)>
;; old-funct: #<procedure + (#:optional _ _ . _)>
;; new-funct: #<procedure new-funct args>
;; scheme@(guile-user)> (+ '(1 2 3) '(4 5 6) '(7 8 9))
;; new-funct: new-funct = #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; new-funct : args = ((1 2 3) (4 5 6) (7 8 9))
;; new-funct : nb-args = 3
;; (12 15 18)

(define-syntax overload

  (syntax-rules ()

    ;; arguments are symbol of function to be overloaded, procedure that do the overloading, list of predicate to check the arguments
    ((_ funct-symb proc (pred-arg1 ...)) (overload-procedure funct-symb proc (pred-arg1 ...)))
    ((_ funct-symb proc (pred-arg1 ...) quote-operator) (overload-operator funct-symb proc (pred-arg1 ...)))))



;; (define (mult-num-vect k v) (map (λ (x) (* k x)) v))
  
;; (overload * mult-num-vect (number? list?) 'operator) 

;; (* 3 (+ '(1 2 3) '(4 5 6)))

;; (15 21 27)

;; (+ (* 3 '(1 2 3)) '(4 5 6))
;; (7 11 15)

;; {3 * '(1 2 3) + '(4 5 6)}
;; (7 11 15)

;; {3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}
;; (14 19 24)


;; scheme@(guile-user)> {3 * '(1 2 3)}
;; $3 = (3 6 9)

;; scheme@(guile-user)> (define (add-vect v) v)
;; scheme@(guile-user)> (overload + add-vect (list?) 'operator)
;; scheme@(guile-user)> (+ '(1 2 3))
;; $7 = (1 2 3)

;; scheme@(guile-user)> (define (add-pair p1 p2) (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))
;; scheme@(guile-user)> (overload + add-pair pair? pair?)
;; overload
;; scheme@(guile-user)> (+ (cons 1 2) (cons 3 4))
(define-syntax overload-procedure
  
  (syntax-rules ()

     ((_ orig-funct funct (pred-arg1 ...)) (define orig-funct (create-overloaded-procedure orig-funct funct (list pred-arg1 ...))))))


(define-syntax overload-operator
  
  (syntax-rules ()

     ((_ orig-funct funct (pred-arg1 ...)) (define orig-funct (create-overloaded-operator orig-funct funct (list pred-arg1 ...))))))



(define (check-arguments pred-list args)
  (if (= (length pred-list) (length args))
      (let ((pred-arg-list (map cons pred-list args)))
	;;(andmap (λ (p) ((car p) (cdr p)))
	;; replace andmap with every
	(every (λ (p) ((car p) (cdr p)))
	       pred-arg-list))
      #f))




;; (define (add-list-list L1 L2) (map + L1 L2))
;; (define + (overload-proc + add-list-list (list list? list?)))
;; (+ '(1 2 3) '(4 5 6))
;; (define (add-pair p1 p2) (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))
;; (define + (overload-proc + add-pair (list pair? pair?)))
;; (+ (cons 1 2) (cons 3 4))


;; when a function that overload an operator has more than 2 args (f a1 a2 a3 ...) and only (f a1 a2) is defined
;; we do: (f a1 (f a2 a3 ...)) for operators
(define (create-overloaded-procedure orig-funct funct pred-list)

  (display "create-overloaded-procedure")
  (display " : pred-list = ") (display pred-list) (newline)
  (define old-funct orig-funct)
  (define new-funct (lambda args ;; args is the list of arguments
		      (display "new-funct: ") (display new-funct) (newline)
		      (display "new-funct : pred-list = ") (display pred-list) (newline)
		      (display "new-funct : args = ") (display args) (newline)
		      (if (check-arguments pred-list args)
			  (begin
			    (display "new funct :calling:") (display funct) (newline)
			    (apply funct args))
			  (begin
			    (display "new funct :calling:") (display old-funct) (newline)
			    (apply old-funct args)))))
				    
  (display "funct: ") (display funct) (newline)
  (display "orig-funct: ") (display orig-funct) (newline)
  (display "old-funct: ") (display old-funct) (newline)
  (display "new-funct: ") (display new-funct) (newline)

  new-funct)


;; scheme@(guile-user)> (use-modules (overload))
;; ;;; note: source file /usr/local/share/guile/site/3.0/overload.scm
;; ;;;       newer than compiled /Users/mattei/.cache/guile/ccache/3.0-LE-8-4.6/usr/local/share/guile/site/3.0/overload.scm.go
;; ;;; note: auto-compilation is enabled, set GUILE_AUTO_COMPILE=0
;; ;;;       or pass the --no-auto-compile argument to disable.
;; ;;; compiling /usr/local/share/guile/site/3.0/overload.scm
;; ;;; compiled /Users/mattei/.cache/guile/ccache/3.0-LE-8-4.6/usr/local/share/guile/site/3.0/overload.scm.go
;; scheme@(guile-user)> (define (add-vect-vect v1 v2) (map + v1 v2))
;; scheme@(guile-user)> (overload + add-vect-vect (list? list?) 'operator)
;; create-overloaded-operator : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; funct: #<procedure add-vect-vect (v1 v2)>
;; orig-funct: #<procedure + (#:optional _ _ . _)>
;; old-funct: #<procedure + (#:optional _ _ . _)>
;; new-funct: #<procedure new-funct args>
;; scheme@(guile-user)> (+ '(1 2 3) '(4 5 6))
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; new-funct : args = ((1 2 3) (4 5 6))
;; new funct :calling:#<procedure add-vect-vect (v1 v2)>
;; $1 = (5 7 9)
;; scheme@(guile-user)> (+ 2 3)
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; new-funct : args = (2 3)
;; new funct :calling:#<procedure + (#:optional _ _ . _)>
;; $2 = 5
;; scheme@(guile-user)> (+ '(1 2 3) '(4 5 6) '(7 8 9))
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; new-funct : args = ((1 2 3) (4 5 6) (7 8 9))
;; $3 = (12 15 18)

;; scheme@(guile-user)> {'(1 2 3) + '(4 5 6) + '(7 8 9)}
;; new-funct: new-funct = #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure list? (_)> #<procedure list? (_)>)
;; new-funct : args = ((1 2 3) (4 5 6) (7 8 9))
;; new-funct : nb-args = 3
;; (12 15 18)

(define (create-overloaded-operator orig-funct funct pred-list) ;; works for associative operators

  (display "create-overloaded-operator")
  (display " : pred-list = ") (display pred-list) (newline)
  (define old-funct orig-funct)
  (define new-funct (lambda args ;; args is the list of arguments
		      (display "new-funct: new-funct = ") (display new-funct) (newline)
		      (display "new-funct : pred-list = ") (display pred-list) (newline)
		      (display "new-funct : args = ") (display args) (newline)
		      (define nb-args (length args))
		      (display "new-funct : nb-args = ") (display nb-args) (newline)
		      (cond ((check-arguments pred-list args) (begin
								(display "new funct :calling:") (display funct) (newline)
								(apply funct args)))
			    ((> nb-args 2) (new-funct (car args) (apply new-funct (cdr args)))) ;; op(a,b,...) = op(a,op(b,...))
			    (else
			     (begin
			       (display "new funct :calling: ") (display old-funct) (newline)
			       (apply old-funct args))))))
				    
  (display "funct: ") (display funct) (newline)
  (display "orig-funct: ") (display orig-funct) (newline)
  (display "old-funct: ") (display old-funct) (newline)
  (display "new-funct: ") (display new-funct) (newline)

  new-funct)



;; (overload-function (+ (L1 list?) (L2 list?)) (map + L1 L2)) ;; bad example ,it is not a function but an operator!
(define-syntax overload-function
  
  (syntax-rules ()

    ((_ (orig-funct (arg1 pred-arg1) ...) expr ...) (overload orig-funct (lambda (arg1 ...) expr ...) (pred-arg1 ...)))))




;; exist in Racket but not Guile
;; (define andmap
;;   (lambda (function list1 . more-lists)
;;     (letrec ((some? (lambda (fct list)
;; 		      ;; returns #f if (function x) returns #t for 
;; 		      ;; some x in the list
;; 		      (and (pair? list)
;; 			   (or (fct (car list))
;; 			       (some? fct (cdr list)))))))

;;       ;; variadic map implementation terminates
;;       ;; when any of the argument lists is empty.
;;       (let ((lists (cons list1 more-lists)))
;; 	(if (some? null? lists)
;; 	    #t
;; 	    (and (apply function (map car lists))
;; 		 (apply andmap function (map cdr lists))))))))




;; (define (check-arguments-eval pred-list args)
;;   ;; (display pred-list)
;;   ;; (newline)
;;   ;; (display args)
;;   ;; (newline)
;;   (if (= (length pred-list) (length args))
;;       (let () 
;; 	(define pred-arg-list (map cons pred-list args))
;; 	(display pred-arg-list) (newline)
;; 	(every (λ (p) ((eval (car p) (interaction-environment)) (cdr p)))
;; 		pred-arg-list))
;;       #f))






;; 
;;   (+ ((add-list (list? list?)) ;;  lst-fct-pred
;;       (add-array (array? array?))))

 ;; (check-if-is-a-function-mapping '(1 2) `((add-list (,list? ,list?)) (add-cacahuete (,number? ,number?))))
;; (#<procedure list? (_)> #<procedure list? (_)>)
;; (1 2)
;; ((#<procedure list? (_)> 1) (#<procedure list? (_)> 2))
;; (#<procedure number? (_)> #<procedure number? (_)>)
;; (1 2)
;; ((#<procedure number? (_)> 1) (#<procedure number? (_)> 2))
;; $4 = add-cacahuete

;; (check-if-is-a-function-mapping '((toto) (titi)) `((add-list (,list? ,list?)) (add-cacahuete (,number? ,number?))))
;; (#<procedure list? (_)> #<procedure list? (_)>)
;; ((toto) (titi))
;; ((#<procedure list? (_)> (toto)) (#<procedure list? (_)> (titi)))
;; $5 = add-list

;; with eval:
;; scheme@(guile-user)> (check-if-is-a-function-mapping '(1 2) '((add-list (list? list?)) (add-cacahuete (number? number?))))
;; (list? list?)
;; (1 2)
;; ((list? . 1) (list? . 2))
;; (number? number?)
;; (1 2)
;; ((number? . 1) (number? . 2))
;; $1 = add-cacahuete

;; (define (check-if-is-a-function-mapping args lst-fct-pred)
;;   (any (lambda (fct-types)
;; 	 (if (check-arguments-eval (second fct-types) args)
;; 	     (first fct-types)
;; 	     #f))
;;        lst-fct-pred))



