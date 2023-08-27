;; overload
;; use with Scheme+:
;;  sudo cp overload.scm /usr/local/share/guile/site/3.0



;;(use-modules (overload))

(define-module (overload)
   #:use-module ((guile))
   ;;  #:use-module (srfi srfi-69 ) ;; Basic hash tables 
   #:use-module (srfi srfi-1) ;; any,every
   #:export ( define-overload-procedure ;; todo: rename define declare ?
	      overload-procedure
	      
	      define-overload-existing-procedure
	      overload-existing-procedure
	      
	      define-overload-operator
	      overload-operator
	      
	      define-overload-existing-operator
	      overload-existing-operator
	      
	      define-overload-n-arity-operator
	      overload-n-arity-operator

	      define-overload-existing-n-arity-operator
	      overload-existing-n-arity-operator
	      
	      overload-function ;; see how to do the same for operator, see the possible problem with infix precedence?

	      ))




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


;; TODO: try to write overload with define-method




;;(define $ovrld-meta$ (make-hash-table))



(define-syntax define-overload-existing-procedure

  (syntax-rules ()

    ((_ proc) '())))

     ;; (begin

     ;;   (define qproc (quote proc))

     ;;   (hash-table-set! $ovrld-meta$ qproc (list 'procedure 'existing))))))



(define-syntax define-overload-procedure

  (syntax-rules ()

    ((_ proc)

     (begin
       
       (define qproc (quote proc))
       (define (proc . args-lst) (error 'overload "failed because procedure can not be applied to arguments list. procedure , arguments list = " qproc args-lst))
       (display "define-overload-procedure : proc =") (display proc) (newline)

       ;(hash-table-set! $ovrld-meta$ qproc (list 'procedure))
       ))))


(define-syntax define-overload-existing-operator

  (syntax-rules ()

    ((_ proc) '()))) ;; do nothing


(define-syntax define-overload-existing-n-arity-operator

  (syntax-rules ()

    ((_ proc) '())))



(define-syntax define-overload-operator

  (syntax-rules ()

    ((_ proc)

     (begin
       
       (define qproc (quote proc))
       (define (proc . args-lst) (error 'overload "failed because operator can not be applied to arguments list. operator , arguments list = " qproc args-lst))
       (display "define-overload-operator : proc =") (display proc) (newline)
       ))))


(define-syntax define-overload-n-arity-operator

  (syntax-rules ()

    ((_ proc)

     (begin
       
       (define qproc (quote proc))
       (define (proc . args-lst) (error 'overload "failed because operator can not be applied to arguments list. operator , arguments list = " qproc args-lst))
       (display "define-overload-operator : proc =") (display proc) (newline)
       ))))




;; (define-syntax overload

;;   (syntax-rules ()

;;     ;; arguments are symbol of function to be overloaded, procedure that do the overloading, list of predicate to check the arguments
;;     ((_ funct-symb proc (pred-arg1 ...))

;;      (begin
;;        (define qproc (quote proc)) ; quoted procedure
;;        (define spec (hash-table-ref $ovrld-meta$ qproc)) ; specifications

;;        (define proc-flag (member 'procedure spec))
;;        (define exst-flag (member 'existing spec))
;;        (define oper-flag (member 'operator spec))
;;        (define n-ari-flag (member 'n-arity spec))
       

;;        (cond ((and proc-flag exst-flag) (overload-procedure funct-symb proc (pred-arg1 ...)))
;; 	     ((and oper-flag exst-flag) (overload-operator funct-symb proc (pred-arg1 ...))
;; 	                                (update-operators))
;; 	     ((and oper-flag exst-flag n-ari-flag) (overload-n-arity-operator funct-symb proc (pred-arg1 ...))
;; 	                                           (update-operators))
;; 	     (else (error "overload: unknow case")))))))
 	 




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

;; (define (add-pair p1 p2) (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))
;; (overload + add-pair (pair? pair?) 'operator)
;; (+ (cons 1 2) (cons 3 4))
;; '(4 . 6)


;; overload

(define-syntax overload-existing-procedure
  
  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))
     (define orig-funct (create-overloaded-procedure orig-funct funct (list pred-arg1 ...))))))



(define-syntax overload-procedure
  
  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))
     (overload-existing-procedure orig-funct funct (pred-arg1 ...)))))


(define-syntax overload-existing-operator
  
  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))
     ;(begin
       (define orig-funct (create-overloaded-existing-operator orig-funct funct (list pred-arg1 ...)))
       ;(display"Updating operators...") (newline)
       ;(update-operators)))))
       )))

(define-syntax overload-operator
  
  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))
     (overload-existing-operator orig-funct funct (pred-arg1 ...)))))
        ;; note: same as existing as we create it in first stage (declare/define)
       

(define-syntax overload-existing-n-arity-operator
  
  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))
     (define orig-funct (create-overloaded-existing-n-arity-operator orig-funct funct (list pred-arg1 ...))))))


(define-syntax overload-n-arity-operator
  
  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))
     (overload-existing-n-arity-operator orig-funct funct (pred-arg1 ...)))))
     
    
 


(define (check-arguments pred-list args)
  (if (= (length pred-list) (length args))
      (let ((pred-arg-list (map cons pred-list args)))
	;;(andmap (λ (p) ((car p) (cdr p)))
	;; replace andmap with every in Guile
	(every (λ (p) ((car p) (cdr p)))
	       pred-arg-list))
      #f))


;; args can be not the same number as predicates and their types must all match 
(define (check-arguments-for-n-arity pred-list args)
  (define type (car pred-list)) ;; i suppose all predicate are same
  (define lbd-assign (lambda (arg) (cons type arg)))
  (define pred-arg-list (map lbd-assign args))
  ;;(andmap (λ (p) ((car p) (cdr p)))
  ;; replace andmap with every in Guile
  (every (λ (p) ((car p) (cdr p)))
	 pred-arg-list))



;; (define (add-list-list L1 L2) (map + L1 L2))
;; (define + (overload-proc + add-list-list (list list? list?)))
;; (+ '(1 2 3) '(4 5 6))
;; (define (add-pair p1 p2) (cons (+ (car p1) (car p2)) (+ (cdr p1) (cdr p2))))
;; (define + (overload-proc + add-pair (list pair? pair?)))
;; (+ (cons 1 2) (cons 3 4))


;; when a function that overload an operator has more than 2 args (f a1 a2 a3 ...) and only (f a1 a2) is defined
;; we do: (f a1 (f a2 a3 ...)) associativeness for operators like this.
;; + - * / ^ and other if any... we know,from overloading those operators are separate distinct case from simple functions.They are n-arity operators.
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


(define (create-overloaded-existing-operator orig-funct funct pred-list) ;; works for associative operators

  (display "create-overloaded-existing-operator")
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
			    ((> nb-args 2) (new-funct (car args)
						      (apply new-funct (cdr args)))) ;; op(a,b,...) = op(a,op(b,...))
			    (else
			     (begin
			       (display "new funct :calling: ") (display old-funct) (newline)
			       (apply old-funct args))))))
				    
  (display "funct: ") (display funct) (newline)
  (display "orig-funct: ") (display orig-funct) (newline)
  (display "old-funct: ") (display old-funct) (newline)
  (display "new-funct: ") (display new-funct) (newline)

  new-funct)



(define (create-overloaded-existing-n-arity-operator orig-funct funct pred-list) ;; works for associative operators

  (newline)
  (display "create-overloaded-existing-n-arity-operator")
  (display " : pred-list = ") (display pred-list) (newline)
  (display "orig-funct = ") (display orig-funct) (newline)
  (display "funct = ") (display funct) (newline)
  (define old-funct orig-funct)
  (define new-funct (lambda args ;; args is the list of arguments
		      (newline)
		      (display "overloaded-existing-n-arity-operator") (newline)
		      (display "orig-funct = ") (display orig-funct) (newline)
		      (display "funct = ") (display funct) (newline)
		      (display "new-funct : new-funct = ") (display new-funct) (newline)
		      (display "new-funct : pred-list = ") (display pred-list) (newline)
		      (display "new-funct : args = ") (display args) (newline)
		      (define nb-args (length args))
		      (display "new-funct : nb-args = ") (display nb-args) (newline)
		      (if (check-arguments-for-n-arity pred-list args)
			     (begin
			       (display "new funct : calling:") (display funct) (newline)
			       (apply funct args))
			    ;; ((> nb-args 2)
			    ;;  (begin
			    ;;    (display "new funct : calling:") (display new-funct) (newline)
			    ;;    (new-funct (car args) (apply new-funct (cdr args))))) 
			    ;; op(a,b,...) = op(a,op(b,...))
			    ;;(else
			     (begin
			       (display "new funct : calling: ") (display old-funct) (newline)
			       (apply old-funct args))))) ;; "recursively" call the older functions
				    
  (display "funct: ") (display funct) (newline)
  (display "orig-funct: ") (display orig-funct) (newline)
  (display "old-funct: ") (display old-funct) (newline)
  (display "new-funct: ") (display new-funct) (newline)
  (newline)

  new-funct)





;; this macro do the two overload steps in one for an existing procedure, (see the potential problem with infix precedence?)
;; (overload-function (+ (L1 list?) (L2 list?)) (map + L1 L2)) ;; bad example ,it is not a function but an operator!
(define-syntax overload-function
  
  (syntax-rules ()

    ((_ (orig-funct (arg1 pred-arg1) ...) expr ...) (create-overloaded-procedure orig-funct
										 (lambda (arg1 ...) expr ...)
										 (pred-arg1 ...)))))




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

