;;#lang reader "SRFI-105.rkt"
;;#lang r5rs

;; infix evaluator with operator precedence

;;(provide (all-defined-out)) ;; export all bindings

;; as $nfx$ will be part of the definition of Scheme+ it is not wise to use Scheme+ for implementing itself !
;; at some point this would cause infinite recursive load 
;;(require "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/Scheme+.rkt")


;; > {5 * 3 + 2}
;; 17

;; { #f or #f and (begin (display "BAD") (newline) #t)}
;; #f 

;; > { #t and #f and (begin (display "BAD") (newline) #t)}
;; #f


;; > (define x 3)
;; > (define y -5)
;; > (! 3 * x + (* 5 y) - 7)
;; -23

;;  (define x 3)
;; > (! x * 5 >= 10)
;; #t
;; ($nfx x * 5 >= 10)
;; #t
;; ($nfx$ 3 * x + (* 5 y) - 7)
;; -23

;; > (! #f && (begin (display "BAD") (newline) #t))
;; BAD
;; #f
;;; evaluates `terms` as a basic infix expression
;;(define (! . terms) ;; this version get the arguments and put them directly in a list
(define (! terms) ;; this version get a list as argument
  ;;(display "! terms =") (display terms) (newline)
  (if (null? terms) ;; i added this null case but with correct input this should not be necessary
      terms
      (car (!* terms infix-operators #f))))


;; > (alternate-quote 3 * 5 + 1)
;; (3 * 5 + 1)
;; > (alternate-quote 3 * 5)
;; (3 * 5)
;; > (alternate-quote 3)
;; (3)
;; > (alternate-quote 3 * 5 + 1 + 7)
;; (3 * 5 + 1 + 7)
;; > (alternate-quote 3 * 5 + 1 + 7 * 8)
;; (3 * 5 + 1 + 7 * 8)
;; > (alternate-quote #t and #f or #t or #f)
;; (#t and #f or #t or #f)

;; a simplier version with a flag alternating
;; (alternate-quote #f 3 * 5 + 1 + 7 * 8)
;; '(3 * 5 + 1 + 7 * 8)
;; > (alternate-quote #f 3)
;; '(3)
;; > (alternate-quote #f 3 * 5)
;; '(3 * 5)
;; DEPRECATED
(define-syntax alternate-quote
  (syntax-rules ()

    ((_ flag e1) (list e1))
    ((_ flag e1 e2 ...) (if flag
                            (cons (quote e1) (alternate-quote (not flag) e2 ...))
                            (cons e1 (alternate-quote (not flag) e2 ...))))))

;; (define-syntax alternate-quote
;;   (syntax-rules ()

;;     ((_ symb) (list symb))
;;     ((_ oper symb) (list (quote oper) symb)) 
;;     ((_ symb oper symb2 ...) (append (list symb (quote oper)) (alternate-quote symb2 ...)))))


;; (factorise-left-arrow-cross (<+ x (<+ y (<+ z (+ (* 3 5) 2)))))
;; '(x y z (+ (* 3 5) 2))
(define (factorise-left-arrow-cross expr)
  ;;(display "factorise-definitions : expr = ") (display expr) (newline)
  (cond ((not (list? expr)) expr)
	((equal? (quote <+) (car expr))
	 (cons (cadr expr)  (factorise-left-arrow-cross (caddr expr))))
	(else (list expr))))



;; (factorise-right-arrow-cross '(+> (+> (+ (* 3 5) 2) x) y))
;; '(y x (+ (* 3 5) 2))
;; note: if it is not the good order of variables but the +> reverse then again
;; setting all back to the good order (which is of NO importance indeed)
(define (factorise-right-arrow-cross expr)
  ;;(display "factorise-definitions : expr = ") (display expr) (newline)
  (cond ((not (list? expr)) expr)
	((equal? (quote +>) (car expr))
	 (cons (caddr expr)  (factorise-right-arrow-cross (cadr expr))))
	(else (list expr))))

(define (infix-with-precedence2prefix expr)
  ;;(display "infix-with-precedence2prefix :")(newline)
  ;;(display expr) (newline)
  (define op (car expr))
  (define expr2eval expr)
  (cond ((equal? op (quote <+)) (let ((expr-fact (factorise-left-arrow-cross expr)))
				  ;;(display "evaluate : expr-fact =") (display expr-fact) (newline)
				  (cons op expr-fact)))
	((equal? op (quote +>)) (let ((expr-fact (factorise-right-arrow-cross expr)))
				  ;;(display "evaluate : expr-fact =") (display expr-fact) (newline)
				  (cons op (reverse expr-fact))))
	(else expr)))

;; (define (compute expr env)
;;   (display "evaluate : expr =") (display expr) (newline)
;;   ;;(eval expr2eval (interaction-environment)))
;;   (local-eval expr2eval env))


;; > (define b #f)
;; > ($nfx$ #t and b)
;; #f

;; > ($nfx$ #f and (begin (display "BAD") (newline) #t))
;; #f
;;  { 4 + 3 * 2 - 19 < 0 - 4}
;; #t
;; (define-syntax $nfx$
;;   (syntax-rules ()
;;     ;; Warning: argument names of macro do not always reprensent the values stored in
;;     ((_ ident opspecial term1 op term2) (cond ((or (equal? (quote opspecial) (quote <-)) (equal? (quote opspecial) (quote ←))) ;; <- operator, example x <- 3 + 2
;; 					       (begin
;; 						 ;;(display "$nfx$") (newline)
;; 						 (opspecial ident (op term1 term2)))) ;; {ident <- term1 op term2}
					      
;; 					      ((or (equal? (quote op) (quote ->)) (equal? (quote op) (quote →))) ;; -> operator, example : 3 + 2 -> x
;; 					       (op term2 (opspecial ident term1))) ;; Warning: argument names of macro do not reprensent the values contained in this case
					      
;; 					      ;;(else (! (quote ident) (quote opspecial) (quote term1) (quote op) (quote term2)))))
;; 					      ;;(else (! ident (eval (quote opspecial) (interaction-environment)) term1 op term2))))
;; 					      (else (! (list ident (quote opspecial) term1 (quote op) term2))))) ;; example : 3 * 5 + 2, the new version of ! get a list as argument
    
						   
;;     ((_ ident opspecial term1 op term2 ...) (if (or (equal? (quote opspecial) (quote <-)) (equal? (quote opspecial) (quote ←))) ;; <- operator, ex: x <- 3 * 5 + 2
;; 						;; there is no 'cond here because there is no way to deal with the 2nd case of 'cond above with multiple values unfortunately
;; 						(opspecial ident ($nfx$ term1 op term2 ...)) 
;; 						;;(! (alternate-quote #f ident opspecial term1 op term2 ...)))))) ;; ex: 3 * 5 + 2 - 7 , this is in fact a general case ($nfx$ term0 ops term1 op term2 ...)
;; 						(! (append (ident) ((quote opspecial) term1) ((quote op) term2) ...))))))




;; now quoting all the operators ... so we are no more annoyed with macro 'bad syntax' error and also this  keep the 'and and 'or short-circuited functionalities.

(define-macro (quote-all . Largs)
  (if (null? Largs)
      `(quote ,Largs)
      `(cons (quote ,(car Largs)) (quote-all ,@(cdr Largs)))))

;; but we then have to eval-uate all the expression at end

(define-macro ($nfx$ . Largs)
  `(local-eval (infix-with-precedence2prefix (! (quote-all ,@Largs))) (the-environment))) 
  

;; (define-syntax $nfx$
;;   (syntax-rules ()

;;     ((_ sexpr ...)   ;;(local-eval (infix-with-precedence2prefix (! (quote sexpr) ...)) (the-environment)))))
;;      (eval (infix-with-precedence2prefix (! (quote sexpr) ...)) (interaction-environment)))))



;; DEPRECATED (4 procedures below)
;;  (andy (#t #t #t))
;; #t
;; test if short circuited:
;;  (andy (#f (begin (display "BAD") (newline) #t)))
;; #f
;; (define-syntax andy ;; Warhol :-)
;;   (syntax-rules ()
    
;;     ((_ (term ...)) (and (eval term (interaction-environment)) ...)))) ;; as 'and and 'or has been quoted previously but as macro can not be evaluated we need procedures like the macro


;; (define-syntax ory ;; Oryx ? :-) ,no just call 'or on terms

;;    (syntax-rules ()
    
;;     ((_ (term ...)) (or (eval term (interaction-environment)) ...)))) ;; as 'and and 'or has been quoted previously but as macro can not be evaluated we need procedures like the macro
  
  			   

;; (define-syntax andy2
;;   (syntax-rules ()
    
;;     ((_ term1 term2) (if odd?
;; 			   (and (eval term1 (interaction-environment)) (eval term2 (interaction-environment)))
;; 			   (and (eval term2 (interaction-environment)) (eval term1 (interaction-environment)))))))


;; ;; > (define b #t)
;; ;; > {#f or #f or b}
;; ;; #t
;; (define-syntax ory2

;;   (syntax-rules ()
    
;;     ((_ term1 term2) (if odd?
;; 			   (or (eval term1 (interaction-environment)) (eval term2 (interaction-environment)))
;; 			   (or (eval term2 (interaction-environment)) (eval term1 (interaction-environment)))))))




;; evaluate one group of operators in the list of terms
(define (!** terms stack operators odd?)

  ;;(newline)
  ;;(display "!** : terms = ") (display terms) (newline)
  ;;(display "!** : operators = ") (display operators) (newline)
  ;;(display "!** : stack = ") (display stack) (newline)
  ;;(display "!** : odd? = ") (display odd?) (newline)

					; why `odd?`? because scheme's list-iteration is forwards-only and
					; list-construction is prepend-only, every other group of operators is
					; actually evaluated backwards which, for operators like / and -, can be a
					; big deal! therefore, we keep this flipped `odd?` counter to track if we
					; should flip our arguments or not


  ;; sometimes quoted args must be evaluated, depending of odd? args order must be swaped,sometimes both !
  ;; (define (calc-proc op a b) ;; keep in mind that now the op-erator and args are quoted !
 
  ;;   (define proc (eval op (interaction-environment)))  ;; this only works with procedure, not special form !
  ;;   ;;(display "before eval arg")
  ;;   (define val-a (eval a (interaction-environment)))
  ;;   (define val-b (eval b (interaction-environment)))
  ;;   ;;(display "after eval arg")
  ;;   (if odd? (proc val-a val-b) (proc val-b val-a)))


  ;; (define (calc op a b)
  ;;   (if odd? (op a b) (op b a)))

  ;; (define (calc op a b)
  ;;   (if odd? ((eval op (interaction-environment)) a b) ((eval op (interaction-environment)) b a))) 
  
  ;; (define (calc op a b) ;; keep in mind that now the op-erator and args are quoted !
  ;;   ;; (display "calc : op = ") (display op) (newline)
  ;;   ;; (display "calc : a = ") (display a) (newline)
  ;;   ;; (display "calc : b = ") (display b) (newline)
  ;;   ;; (display "calc : odd? = ") (display odd?) (newline)
    
  ;;   ;; special forms cases else procedure
  ;;   ;; (cond ((eq? op 'and) (andy2 a b))
  ;;   ;; 	  ((eq? op 'or) (ory2 a b))
  ;;   ;; 	  (else (calc-proc op a b)))) ;; procedure case
  ;;   (if odd? 
  ;; 	(eval (list op a b) (interaction-environment))
  ;; 	(eval (list op b a) (interaction-environment))))



  
  ;; (define (calc op a b) ;; keep in mind that now the op-erator and args are quoted !
  ;;   ;; (display "calc : op = ") (display op) (newline)
  ;;   ;; (display "calc : a = ") (display a) (newline)
  ;;   ;; (display "calc : b = ") (display b) (newline)
  ;;   ;; (display "calc : odd? = ") (display odd?) (newline)
    
  ;;   ;; special forms cases else procedure
  ;;   (cond ((eq? op 'and) (if odd?
  ;; 			     (and a b)
  ;; 			     (and b a)))
  ;;   	  ((eq? op 'or) (if odd?
  ;; 			     (or a b)
  ;; 			     (or b a)))
  ;;   	  (else ;; procedure case
  ;; 	   (if odd?
  ;; 	       ((eval op (interaction-environment)) a b)
  ;; 	       ((eval op (interaction-environment)) b a)))))

  ;; (define (calc op a b)
  ;;   (if odd? 
  ;; 	(eval (list op a b) (interaction-environment))
  ;; 	(eval (list op b a) (interaction-environment))))
  
  

  
  (cond ((null? terms) stack) ; base case
      
	;; operator we can evaluate -- pop operator and operand, then recurse
	;; begin clause
	((and (> (length stack) 1) (begin
				     ;;(display "operators=") (display operators) (newline)
				     (let* ((op (car stack))
				   	    (mres (member op operators)))
				       ;;(display "op=") (display op) (newline)
				       ;;(display "mres=") (display mres) (newline) (newline)
				       mres)))
	      ;;(memq (car stack) operators))
	      ;;(member (car stack) operators))
	      
	 (let* ((op (car stack))
		(fst (car terms)) ;; a
		(snd (cadr stack)) ;; b
		(calculation (if odd?
				 (list op fst snd)
				 (list op snd fst))))
	   
	   ;;(display "op=") (display op) (newline)
	   
	   (!** (cdr terms)
		;;(cons (calc op fst snd) (cddr stack))
		(cons calculation (cddr stack))
		operators
		(not odd?))))
	;; end clause
	
	;; otherwise just keep building the stack
	(else (!** (cdr terms)
		   (cons (car terms) stack)
		   operators
		   (not odd?)))))



;; evaluate a list of groups of operators in the list of terms
(define (!* terms operator-groups odd?)
  ;;(display "!* : terms = ") (display terms) (newline)
  ;;(display "!* : operator-groups = ") (display operator-groups) (newline)
  (if (or (null? operator-groups) ; done evaluating all operators
	  (null? (cdr terms)))    ; only one term left
      ;; (let ((cterms (car terms)))
      ;; 	(eval cterms (interaction-environment)))
      terms			; finished processing operator groups
      ;; evaluate another group -- separating operators into groups allows
      ;; operator precedence
      (!* (!** terms '() (car operator-groups) odd?)
	  (cdr operator-groups)
	  (not odd?))))


;; scheme@(guile-user)> (define i 3)
;; scheme@(guile-user)> {i <- i + 1}
;; ! terms =(i <- i + 1)
;; $4 = 4


;; ; also works for inequalities!

;; { 4 + 3 * 2 - 19 < 0 - 4}
;; #t

;; { b <+ #f and (begin (display "BAD") (newline) #t)}
;; #f

;; scheme@(guile-user)> (define c 300000)
;; scheme@(guile-user)> (define v 299990)
;; scheme@(guile-user)> (define t 30)
;; scheme@(guile-user)> (define x 120)
;; scheme@(guile-user)> (declare xp)
;; scheme@(guile-user)> '{xp <- {x - v * t} / (sqrt {1 - v ** 2 / c ** 2})} 
;; ($nfx$ xp <- ($nfx$ x - v * t) / (sqrt ($nfx$ 1 - v ** 2 / c ** 2)))
;; scheme@(guile-user)> {xp <- {x - v * t} / (sqrt {1 - v ** 2 / c ** 2})} 
;; -1102228130.2405226
;; scheme@(guile-user)> xp
;; -1102228130.2405226

;; {x <+ 3 * 5 + 2}
;; 17
;; x
;; 17

;; {3 * 5 + 2 +> x}

;; (declare y)
;; {x <+ y <- 3 * 5 + 2}

;; (define T (make-vector 5))
;; {3 * 5 + 2 -> T[1] -> T[2] -> T[3]}
;; 17


;; {T[3] <- T[2] <- T[1] <- 3 * 5 + 2}
;; 17

;; scheme@(guile-user)> (declare x y z)
;; scheme@(guile-user)> {x <- y <- z <- 3 * 5 + 2}
;; !* : terms = ((<- (<- (<- x y) z) (+ (* 3 5) 2)))
;; ice-9/boot-9.scm:1685:16: In procedure raise-exception:
;; Bad <- form: the LHS of expression must be an identifier or of the form ($bracket-apply$ container index) , first argument  <- " is not $bracket-apply$."


;; {3 * 5 + 2 -> x -> y}

;; {x <- y <- 3 * 5 + 2}

;; {3 * 5 + 2 +> x +> y}

;; {x <+ y <+ 3 * 5 + 2}

;; {x <+ y <+ z <+ 3 * 5 + 2}


;; can you believe they made && and || special forms??? yes :-) but with advantage of being short-circuited,but i admit it has been a headlock for an infix solution 
;; note: difference between bitwise and logic operator


;; a list of lists of operators. lists are evaluated in order, so this also
;; determines operator precedence
;;  added bitwise operator with the associated precedences and modulo too
(define infix-operators
  
  (list
  
   (list 'expt '**)
   (list '* '/ '%)
   (list '+ '-)
   
   (list '<< '>>)

   (list '& '|)

    ; now this is interesting: because scheme is dynamically typed, we aren't
    ; limited to any one type of function
   
   (list '< '> '= '<> '≠ '<= '>=)
   
   (list 'and)

   (list 'or)
 
   ;;(list 'dummy) ;; can keep the good order in case of non left-right assocciative operators.(odd? reverse them) 
   (list  '->  '→)
   (list '<-  '←)
   
   (list '+> '⥅)
   (list '<+ '⥆)
  
   )
  )
