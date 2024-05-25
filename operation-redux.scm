;; (define definition-operator '(<+ +>
;; 			      ⥆ ⥅
;; 			      :+ +:))

;; (define assignment-operator '(<- ->
;; 			      ← →
;; 			      :=  =:
;; 			      <v v>
;; 			      ⇜ ⇝))


(define definition-operator (list '<+ '+>
				  '⥆ '⥅
				  ':+ '+:))

(define assignment-operator (list '<- '->
				  '← '→
				  ':=  '=:
				  '<v 'v>
				  '⇜ '⇝))


(define infix-operators-lst-for-parser

  ;; `(
  ;;   (expt **)
  ;;   (* / %)
  ;;   (+ -)
	
  ;;   (<< >>)

  ;;   (& ∣)

  ;;   (< > = ≠ <= >= <>)

  ;;   (and)

  ;;   (or)
	
  ;; 	;;(list 'dummy) ;; can keep the good order in case of non left-right assocciative operators.(odd? reverse them) 

  ;;   ,assignment-operator ;(<- -> ← → := <v v> ⇜ ⇝)
  ;;   ,definition-operator  ; (<+ +> ⥆ ⥅ :+)
  ;;   )

  (list
    
   (list 'expt '**)
   (list '* '/ '%)
 
   (list '+ '-)
   
   (list '<< '>>)
   
   (list '&)
   (list '^)
   (list '∣)
   
   (list '< '> '= '≠ '<= '>= '<>)

   (list 'and)
   
   (list 'or)
    
   (append assignment-operator 
	   definition-operator)
     
   )
  
  )


(define definition-operator-syntax (list #'<+ #'+>
					 #'⥆ #'⥅
					 #':+ #'+:))

(define assignment-operator-syntax (list #'<- #'->
					 #'← #'→
					 #':=  #'=:
					 #'<v #'v>
					 #'⇜ #'⇝))



(define infix-operators-lst-for-parser-syntax

  (list
    (list #'expt #'**)
    (list #'* #'/ #'%)
    (list #'+ #'-)
	
    (list #'<< #'>>)

    (list #'& #'∣)

    (list #'< #'> #'= #'≠ #'<= #'>= #'<>)

    (list #'and)

    (list #'or)

    assignment-operator-syntax
    definition-operator-syntax 
    )

  )



;; liste à plate des operateurs
(define operators-lst
   (apply append infix-operators-lst-for-parser))


;; (define (operator? x oper-lst)
;;   (member x oper-lst))


;; check that expression is infix

;; modified for syntax too
(define (infix? expr oper-lst)

  ;;(display "infix? : expr=") (display expr) (newline)
  ;;(display "infix? : oper-lst=") (display oper-lst) (newline)
  
  (define (infix-rec? expr) ; (op1 e1 op2 e2 ...)
    ;;(display "infix-rec? : expr=") (display expr) (newline)
    (if (null? expr)
	#t
    	(and (member-syntax (car expr) oper-lst) ;; check (op1 e1 ...) 
	     (not (member-syntax (cadr expr) oper-lst)) ; check not (op1 op2 ...)
	     (infix-rec? (cddr expr))))) ; continue with (op2 e2 ...) 


  (define rv
    (cond ((not (list? expr)) (not (member-syntax expr oper-lst))) ; not an operator ! 
	  ((null? expr) #t) ; definition
	  ((null? (cdr expr)) #f) ; (a) not allowed as infix
	  (else
	   (and (not (member-syntax (car expr) oper-lst)) ; not start with an operator !
		(infix-rec? (cdr expr)))))) ; sublist : (op arg ...) match infix-rec
  
  ;;(display "infix? : rv=") (display rv) (newline)

  rv
  
  )

;; return the operator of an operation
(define (operator expr)
  (car expr))


;; return the first argument of a binary operation
(define (arg1 expr)
  (first (rest expr)))

;; return the second argument of a binary operation
(define (arg2 expr)
  (first (rest (rest expr))))

(define (arg expr)
  (arg1 expr))

;; return the arguments of an operation
(define (args expr)
  (cdr expr))

;; function without parameters
(define (function-without-parameters? expr)
  (null? (rest expr)))

;; (unary-operation? '(not a)) -> #t
(define (unary-operation? expr)
  (null? (rest (rest expr))))

;; (binary-operation? '(and a b)) -> #t
(define (binary-operation? expr)
  ;;(null? (rest (rest (rest expr)))))
  (and (pair? expr)
       (pair? (rest expr))
       (pair? (rest (rest expr)))
       (null? (rest (rest (rest expr))))))








;; operators test

;; test if an operator is AND
(define (AND-op? oper)
  (datum=? oper 'and))
  ;;(or (eqv? oper 'and) (check-syntax=? oper #'and)))
  ;;(or (eqv? oper 'and) (eqv? oper 'AND) (eqv? oper '·)))

;; test if an operator is OR
(define (OR-op? oper)
  (datum=? oper 'or))
  ;;(or (eqv? oper 'or) (check-syntax=? oper #'or)))
  ;;(or (eqv? oper 'or) (eqv? oper 'OR)  (eqv? oper '➕))) ;; middle dot

(define (XOR-op? oper) ;; note: logxor in Guile, xor in Racket
  (or (datum=? oper 'logxor)
      (datum=? oper 'xor)
      (datum=? oper '^)))

  ;;(or (eqv? oper 'xor) (check-syntax=? oper #'xor)))
  ;;(or (eqv? oper 'xor) (eqv? oper 'XOR)  (eqv? oper '⊕))) ;; ⨁


;; test if an operator is NOT
(define (NOT-op? oper)
  (datum=? oper 'not))
  ;;(or (eqv? oper 'not) (check-syntax=? oper #'not))) ; not sure it is usefull in syntax 
  ;;(or (eqv? oper 'not) (eqv? oper 'NOT)))

(define (ADD-op? oper)
  (or (eqv? oper +)
      (datum=? oper '+)))
  ;;(or (eqv? oper +) (eqv? oper '+) (check-syntax=? oper #'+)))

(define (IMPLIC-op? oper)
  (or (eqv? oper '⟹) (eqv? oper '=>)))

(define (EQUIV-op? oper)
  (or (eqv? oper '⟺) (eqv? oper '<=>)))


(define (DEFINE-op? oper)
  ;; (or (eqv? oper '<+) (eqv? oper '+>)
  ;;     (eqv? oper '←) (eqv? oper '+>)
  ;;     (eqv? oper '<-) (eqv? oper '->)
  ;;     (eqv? oper '←) (eqv? oper '+>)))

  (or (memv oper definition-operator)
      (member-syntax oper definition-operator-syntax)))


  
(define (ASSIGNMENT-op? oper)
  ;;(or (eqv? oper '<-) (eqv? oper '->)))
  (or (memv oper assignment-operator)
      (member-syntax oper assignment-operator-syntax)))





(define (is-associative-operator? op)
  (or (AND-op? op)
      (OR-op? op)
      (XOR-op? op)
      (EQUIV-op? op)
      (ADD-op? op)
      (MULTIPLY-op? op)
      (DEFINE-op? op)
      (ASSIGNMENT-op? op)))










;; expression tests

(define (isADD? expr)
  (and (pair? expr) (ADD-op? (car expr))))

(define (MULTIPLY-op? oper)
  (or (eqv? oper *)
      (datum=? oper '*)))

  ;;(or (eqv? oper *) (eqv? oper '*) (check-syntax=? oper #'*)))

(define (isMULTIPLY? expr)
  (and (pair? expr) (MULTIPLY-op? (car expr))))

;; test if an expression is a OR
(define (isOR? expr)
  ;;(and (pair? expr) (equal? (car expr) 'or)))
  (and (pair? expr) (OR-op? (car expr))))

;; test if an expression is a AND
(define (isAND? expr)
  ;;(and (pair? expr) (equal? (car expr) 'and)))
  (and (pair? expr) (AND-op? (car expr))))


;; is expression an (OR or AND) ?
(define (isOR-AND? expr)
  (or (isOR? expr)  (isAND? expr)))

(define (isNOT? expr)
  (and (pair? expr) (NOT-op? (car expr))))

(define (isIMPLIC? expr)
  (and (pair? expr) (IMPLIC-op? (operator expr))))

(define (isEQUIV? expr)
  (and (pair? expr) (EQUIV-op? (operator expr))))

(define (isXOR? expr)
  (and (pair? expr) (XOR-op?  (operator expr))))

(define (isDEFINE? expr)
  (and (pair? expr) (DEFINE-op?  (operator expr))))

(define (isASSIGNMENT? expr)
  (and (pair? expr) (ASSIGNMENT-op?  (operator expr))))


(define (isASSOCIATIVE? expr)
  (is-associative-operator? (first expr)))








;; initially written for logical expression of OR / AND
;; should works for every associative operator

;; n-arity function, this version will not show AND & OR case but collect them in one single line code
;; n-arity single function replacing n-arity-or and n-arity-and and that use the collect-leaves function 
;; with no match special form inside them and no operator show
;;
;;  (n-arity '(or a (or b c)))
;; '(or a b c)
;; > (n-arity '(or a (or b c d)))
;; '(or a b c d)
;; > (n-arity '(or a (and e f) (or b c d)))
;; '(or a (and e f) b c d)
;; > (n-arity '(or a (and e (or f g h i)) (or b c d)))
;; '(or a (and e (or f g h i)) b c d)
;; > (n-arity '(or a (and e (or f g h i)) (or b c d (or j k l))))
;; '(or a (and e (or f g h i)) b c d j k l)
;;
;; (n-arity '(or a (and e (or f g h i) (and m n)) (or b c d (or j k l))))
;; -> '(or a (and e (or f g h i) m n) b c d j k l)
;;
;; (n-arity '(not (or a (and e (or f g h i) (and m n)) (or b c d (or j k l)))))
;; '(not (or a (and e (or f g h i) m n) b c d j k l))
;; > (n-arity '(not (or a (and e (or f g (not h) i) (and m n)) (or b c d (or j k l)))))
;; '(not (or a (and e (or f g (not h) i) m n) b c d j k l))
;; > (n-arity '(not (or a (and e (or f g (not h) i) (and (not m) n)) (or (not b) c d (or j k l)))))
;; '(not (or a (and e (or f g (not h) i) (not m) n) (not b) c d j k l))

  
;;  > (n-arity (dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))
;;  '(or (and c (not a) a)
;;       (and c (not a) (not b))
;;       (and c b a)
;;       (and c b (not b))
;;       (and (not c) a (not b))
;;       (and (not c) (not a) b))

;;  > (n-arity '(or a (not b) (or (or (and c (and c2 c3)) d) e) (and (and (not f) g) h) (or i (and (not j) (and k (or l (or m (not n))))))) )
;;  '(or a (not b) (and c c2 c3) d e (and (not f) g h) i (and (not j) k (or l m (not n))))
;;  > 

  
;;  > (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))))
;;  '(and (or c (not c))
;;        (or c a (not a))
;;        (or c a b)
;;        (or c (not b) (not a))
;;        (or c (not b) b)
;;        (or (not a) b (not c))
;;        (or (not a) b a (not a))
;;        (or (not a) b a b)
;;        (or (not a) b (not b) (not a))
;;        (or (not a) b (not b) b)
;;        (or a (not b) (not c))
;;        (or a (not b) a (not a))
;;        (or a (not b) a b)
;;        (or a (not b) (not b) (not a))
;;        (or a (not b) (not b) b))
;;     
;; (n-arity '(+ a (+ b c))) -> '(+ a b c)
;;
;; 
;;(prefix->infix (n-arity (expt->^ (simplify (hereditary-base-monomial-1 '(expt 4 7))))))
;; -> '((3 * (4 ^ 6)) + (3 * (4 ^ 5)) + (3 * (4 ^ 4)) + (3 * (4 ^ 3)) + (3 * (4 ^ 2)) + (3 * 4) + 3)
;;

;; > (n-arity '(+ a (+ b c)))
;; '(+ a b c)
;; > (n-arity '(- a (- b c)))
;; '(- a (- b c))
;;  (n-arity '(- (- (- a b) c) d))
;; '(- a b c d) 
;; > (n-arity '(- a (+ b c)))
;; '(- a (+ b c))
;; > (n-arity '(+ (+ a (- b c)) d))
;; '(+ a (- b c) d)
;; > (n-arity '(+ (+ a (+ e (- b c))) d))
;; '(+ a e (- b c) d)
;; > (n-arity '(+ (+ a (+ e (- b (+ f (+ g c)))) d) h))
;; '(+ a e (- b (+ f g c)) d h)
;; >

;;  (n-arity '(<- a (<- b (<- c 7))))
;; '(<- a b c 7)
;; (n-arity '(<- x (<- a (<- b (- b c)))))
;; '(<- x a b (- b c))

;; warning: usualy give a false result if operator is not associative
;; could not work with exponentiation, expt , ** : is evaluation is from right to left (opposite normal evaluation and not associative! and not commutative!)
(define (n-arity expr)

  ;;(display "n-arity : expr =")(display expr) (newline)

  (define rv
    (cond
     ((not (list? expr)) expr) ;; symbol,number,boolean,vector,hash table....
     ((null? expr) expr)
     ((function-without-parameters? expr) expr)
     ((unary-operation? expr)
      (cons
       (operator expr)
       (list (n-arity (arg expr)))))
     ((isASSOCIATIVE? expr)
      (let ((opera (operator expr)))
	(cons opera
	      (apply
	       append
	       (map (make-collect-leaves-operator opera) (args expr))))))
     
     (else ;; safe else, will not touch non associative operators
      (let ((opera (operator expr)))
	(cons opera
	      (map n-arity (args expr)))))))

  ;;(display "n-arity : rv =") (display rv) (newline)
  ;;(newline)

  rv

  )


;; return a closure of collect-leaves function associated with an operator (AND or OR)
(define (make-collect-leaves-operator oper)

  (let ((ourOperation?
	 
	 (cond
	  
	  ((AND-op? oper) isAND?)
	  ((OR-op? oper) isOR?)
	  ((ADD-op? oper) isADD?)
	  ((DEFINE-op? oper) isDEFINE?)
	  ((ASSIGNMENT-op? oper) isASSIGNMENT?)

	  (else ; fonction generique pour tous les operateurs ( non syntaxiques )
	   (lambda (expr) (and (pair? expr) (equal? (car expr) oper)))))))

    
    (letrec ((collect-leaves-operator

	      (lambda (expr)
		(cond
		 ((not (list? expr)) (list expr)) ;; symbol,number,boolean,vector,hash table....
		 ((null? expr) (list expr))
		 ((function-without-parameters? expr) (list expr))
		 ((unary-operation? expr)
		  (list
		   (cons
		    (operator expr)
		    (list (n-arity (arg expr))))))
		 ((ourOperation? expr) ;; #;(eqv? oper (operator expr))
		  (apply append (map collect-leaves-operator (args expr))))
		 (else (list (n-arity expr)))))))

        collect-leaves-operator)))


