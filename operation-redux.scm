(define infix-operators-lst-for-parser

  '(
    (expt **)
    (* / %)
    (+ -)
	
    (<< >>)

    (& ∣)

    (< > = ≠ <= >= <>)

    (and)

    (or)
	
	;;(list 'dummy) ;; can keep the good order in case of non left-right assocciative operators.(odd? reverse them) 

    (<- -> ← → <v v> ⇜ ⇝)
    (<+ +> ⥆ ⥅)
    )

  )

(define operators-lst
  (apply append infix-operators-lst-for-parser))


(define (operator? x)
  (member x operators-lst))


;; check that expression is infix
(define (infix? expr)
  
  (define (infix-rec? expr)
    (cond ((null? expr) #t)
	  ((not (zero? (modulo (length expr) 2))) #f)
	  (else (and (operator? (car expr)) ;; check (op1 e1 ...) 
		     (not (operator? (cadr expr)))
		     (infix-rec? (cddr expr))))))
  
  (or (null? expr) (and (not (operator? (car expr)))
			(infix-rec? (cdr expr)))))


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


;; test if an operator is AND
(define (AND-op? oper)
  ;;(or (equal? oper 'and) (equal? oper 'AND)))
  (or (eqv? oper 'and) (eqv? oper 'AND) (eqv? oper '·)))

;; test if an operator is OR
(define (OR-op? oper)
  ;;(or (equal? oper 'or) (equal? oper 'OR)))
  (or (eqv? oper 'or) (eqv? oper 'OR)  (eqv? oper '➕))) ;; middle dot

(define (XOR-op? oper)
  (or (eqv? oper 'xor) (eqv? oper 'XOR)  (eqv? oper '⊕))) ;; ⨁


;; test if an operator is NOT
(define (NOT-op? oper)
  (or (eqv? oper 'not) (eqv? oper 'NOT)))

(define (ADD-op? oper)
  (or (eqv? oper +) (eqv? oper '+)))

(define (IMPLIC-op? oper)
  (or (eqv? oper '⟹) (eqv? oper '=>)))

(define (EQUIV-op? oper)
  (or (eqv? oper '⟺) (eqv? oper '<=>)))



(define (isADD? expr)
  (and (pair? expr) (ADD-op? (car expr))))

(define (MULTIPLY-op? oper)
  (or (eqv? oper *) (eqv? oper '*)))

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




(define (DEFINE-op? oper)
  (or (eqv? oper '<+) (eqv? oper '+>)))

(define (ASSIGNMENT-op? oper)
  (or (eqv? oper '<-) (eqv? oper '->)))

(define (is-associative-operator? op)
  (or (AND-op? op)
      (OR-op? op)
      (XOR-op? op)
      (EQUIV-op? op)
      (ADD-op? op)
      (MULTIPLY-op? op)
      (DEFINE-op? op)
      (ASSIGNMENT-op? op)))


(define (isASSOCIATIVE? expr)
  (is-associative-operator? (first expr)))



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
(define (n-arity expr)

  ;;(display "n-arity : expr =")(display expr) (newline)
  
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


;; return a closure of collect-leaves function associated with an operator (AND or OR)
(define (make-collect-leaves-operator oper)

  (let ((ourOperation?
	 (cond
	  ((AND-op? oper) isAND?)
	  ((OR-op? oper) isOR?)
	  ((ADD-op? oper) isADD?)
	  ((MULTIPLY-op? oper) isMULTIPLY?)
	  (else ; fonction genrique pour tous les operateurs
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


