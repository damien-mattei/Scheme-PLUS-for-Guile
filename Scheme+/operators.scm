;; Translated from Scheme+ for Racket (operators.rkt) for Guile.

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



(define-module (Scheme+ operators)
  #:use-module (Scheme+ racket-compat))


  

  
  
  (use-modules
	((srfi srfi-1) #:select (first member))
	(Scheme+ syntax)
	(Scheme+ operators-list)
	(Scheme+ multiply)
	(Scheme+ not-equal)
	(Scheme+ little-greater-or-equal))

  
(define rest cdr)




;; operators predicates
(define (operator? x)
  (member x operators-lst))

;; syntax version
;; works with any (procedure,symbol,syntax)

;; (operator-syntax? +)
;; #t

;; (operator-syntax? '+)
;; #t


;; (operator-syntax? #'+)
;; #t

(define (operator-syntax? x)
  (member-generic x operators-lst-syntax))

(define (operator-symbol-or-syntax? x)
  (or (operator? x)
      (operator-syntax? x)))

(define (arithmetic-operator? x)
  (member-generic x arithmetic-operator-lst-syntax))




;; (in/equalities-operator? <)
;; #t

;; (in/equalities-operator? '<=)
;; #t

;; (in/equalities-operator? #'<=)
;; #t

;; (in/equalities-operator? #'+)
;; #f
(define (in/equalities-operator? x)
  (member-generic x in/equalities-operator-syntax))


(define (not+-? x)
  (not (SIGN-op? x)))


(define (NO-OP? elem)
  (not (operator-symbol-or-syntax? elem)))





;; return the operator of an operation
(define (operator expr)
  (car expr))


;; return the first argument of a binary operation
(define (arg1 expr)
  ;;(display "arg1") (newline)
  (first (rest expr)))

;; return the second argument of a binary operation
(define (arg2 expr)
  ;;(display "arg2") (newline)
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
  (or (datum=? oper 'and)
      (datum=? oper '·)))
  ;;(or (eq? oper 'and) (check-syntax=? oper #'and)))
  ;;(or (eq? oper 'and) (eq? oper 'AND) (eq? oper '·)))

;; test if an operator is OR
(define (OR-op? oper)
  (datum=? oper 'or))
  ;;(or (eq? oper 'or) (check-syntax=? oper #'or)))
;;(or (eq? oper 'or) (eq? oper 'OR)  (eq? oper '➕))) ;; middle dot


(define (LOGIC-N-ARY-op? oper)
  (or (AND-op? oper)
      (OR-op? oper)))

;; Warning : no difference here between 'logic' and 'nbitwise' operators
(define (XOR-op? oper) ;; note: logxor in Guile, xor in Racket
  (or (datum=? oper 'logxor)
      (datum=? oper 'xor)
      (datum=? oper '^)
      (datum=? oper '⊕)))

  ;;(or (eq? oper 'xor) (check-syntax=? oper #'xor)))
  ;;(or (eq? oper 'xor) (eq? oper 'XOR)  (eq? oper '⊕))) ;; ⨁


;; test if an operator is NOT
(define (NOT-op? oper)
  (datum=? oper 'not))

;; test if an operator is the bitwise not ~ (a prefix unary operator in infix , see group-prefix-operators in infix-prefix-postfix.rkt)
(define (BITWISE-NOT-op? oper)
  (datum=? oper '~))
  ;;(or (eq? oper 'not) (check-syntax=? oper #'not))) ; not sure it is usefull in syntax 
  ;;(or (eq? oper 'not) (eq? oper 'NOT)))


(define (EQUAL-op? oper)
  (or (eq? oper =)
      (eq? oper equal?)
      (datum=? oper '=)
      (datum=? oper 'equal?)))

(define (NOT-EQUAL-op? oper)
  (or (eq? oper ≠)
      (eq? oper <>)
      (datum=? oper '≠)
      (datum=? oper '<>)))


(define (LESS-op? oper)
  (or (eq? oper <)
      (datum=? oper '<)))

(define (GREATER-op? oper)
  (or (eq? oper >)
      (datum=? oper '>)))

(define (LESS-OR-EQUAL-op? oper)
  (or (eq? oper <=)
      (datum=? oper '<=)
      (eq? oper ≤)
      (datum=? oper '≤)))

(define (GREATER-OR-EQUAL-op? oper)
  (or (eq? oper >=)
      (datum=? oper '>=)
      (eq? oper ≥)
      (datum=? oper '≥)))


(define (IN-EQUALITY-op? oper)
  (or (EQUAL-op? oper)
      (NOT-EQUAL-op? oper)
      (LESS-op? oper)
      (GREATER-op? oper)
      (LESS-OR-EQUAL-op? oper)
      (GREATER-OR-EQUAL-op? oper)))


(define (ADD-op? oper)
  (or (eq? oper +)
      (datum=? oper '+)))
;;(or (eq? oper +) (eq? oper '+) (check-syntax=? oper #'+)))

(define (MINUS-op? oper)
  (or (eq? oper -)
      (datum=? oper '-)))

(define (SIGN-op? oper)
  (or (ADD-op? oper)
      (MINUS-op? oper)))


(define (MULTIPLY-op? oper)
  (or (eq? oper *)
      (eq? oper ·)
      (datum=? oper '*)
      (datum=? oper '·)))


(define (BRACKET-APPLY-op? oper)
  (or (eq? oper '$bracket-apply$)
      (datum=? oper '$bracket-apply$)))

(define (BRACKET-APPLY-VECTOR-op? oper)
  (or (eq? oper '$bracket-apply-vector$)
      (datum=? oper '$bracket-apply-vector$)))

(define (BRACKET-APPLY-STRING-op? oper)
  (or (eq? oper '$bracket-apply-string$)
      (datum=? oper '$bracket-apply-string$)))

(define (BRACKET-APPLY-HASH-TABLE-op? oper)
  (or (eq? oper '$bracket-apply-hash-table$)
      (datum=? oper '$bracket-apply-hash-table$)))

(define (BRACKET-APPLY-ARRAY-op? oper)
  (or (eq? oper '$bracket-apply-array$)
      (datum=? oper '$bracket-apply-array$)))

;; the accesses written by the type annotation pass for indexes typed ℕ (natural) or ℤ (integer) , vectors and strings
(define (BRACKET-APPLY-VECTOR-NATURAL-op? oper)
  (datum=? oper '$bracket-apply-vector-natural$))

(define (BRACKET-APPLY-VECTOR-INTEGER-op? oper)
  (datum=? oper '$bracket-apply-vector-integer$))

(define (BRACKET-APPLY-STRING-NATURAL-op? oper)
  (datum=? oper '$bracket-apply-string-natural$))

(define (BRACKET-APPLY-STRING-INTEGER-op? oper)
  (datum=? oper '$bracket-apply-string-integer$))

(define (BRACKET-APPLY-TYPED-INDEX-op? oper) ; one of the four above
  (or (BRACKET-APPLY-VECTOR-NATURAL-op? oper)
      (BRACKET-APPLY-VECTOR-INTEGER-op? oper)
      (BRACKET-APPLY-STRING-NATURAL-op? oper)
      (BRACKET-APPLY-STRING-INTEGER-op? oper)))

; flomat removed (sci requires libgfortran)
#;(define (BRACKET-APPLY-FLOMAT-op? oper)
  (or (eq? oper '$bracket-apply-flomat$)
      (datum=? oper '$bracket-apply-flomat$)))

(define (VALUES-op? oper)
  (or (eq? oper 'values)
      (datum=? oper 'values)))


(define (IMPLIC-op? oper)
  (or (eq? oper '⟹) (eq? oper '=>)))

(define (EQUIV-op? oper)
  (or (eq? oper '⟺) (eq? oper '<=>)))


(define (DEFINITION-op? oper)
  (or (eq? oper 'define)
      (datum=? oper 'define)))

(define (QUOTE-op? oper)
  (define op 'quote)
  (or (eq? oper op)
      (datum=? oper op)))

(define (QUASIQUOTE-op? oper)
  (define op 'quasiquote)
  (or (eq? oper op)
      (datum=? oper op)))

;; #'(...) , #`(...) , (quote-syntax ...) : a syntax template is data, like a quoted expression
(define (SYNTAX-QUOTE-op? oper)
  (or (memq oper '(syntax quasisyntax quote-syntax))
      (datum=? oper 'syntax)
      (datum=? oper 'quasisyntax)
      (datum=? oper 'quote-syntax)))

;; (case key ((datum ...) expr ...) ...) : the lists of datums are data
(define (CASE-op? oper)
  (define op 'case)
  (or (eq? oper op)
      (datum=? oper op)))

;; (cond (test expr ...) ...) : a clause is not an expression
(define (COND-op? oper)
  (define op 'cond)
  (or (eq? oper op)
      (datum=? oper op)))

;; (lambda formals body ...) : the formals are not an expression
(define (LAMBDA-op? oper)
  (or (memq oper '(lambda λ))
      (datum=? oper 'lambda)
      (datum=? oper 'λ)))

;; (do ((var init step) ...) (test expr ...) body ...) : the specifications of variables and the clause of end are not expressions
(define (DO-op? oper)
  (define op 'do)
  (or (eq? oper op)
      (datum=? oper op)))


(define (DEFINE-op? oper)
  ;; (or (eq? oper '<+) (eq? oper '+>)
  ;;     (eq? oper '←) (eq? oper '+>)
  ;;     (eq? oper '<-) (eq? oper '->)
  ;;     (eq? oper '←) (eq? oper '+>)))

  (or (memv oper definition-operator)
      (member-generic oper definition-operator-syntax)))


  
(define (ASSIGNMENT-op? oper)
  ;;(or (eq? oper '<-) (eq? oper '->)))
  (or (memv oper assignment-operator)
      (member-generic oper assignment-operator-syntax)))

(define (FLOW-op? oper)
  (datum=? oper '~>))

(define (WALRUS-op? oper) ; the walrus operator : assigns and returns the value
  (datum=? oper ':=>))

(define (COMPOSITION-op? oper)
  (datum=? oper '∘))

;; (define (SINGLE-VARIABLE-ASSIGNMENT-op? oper)
;;   (member-generic oper single-variable-assignment-operator-syntax))


(define (EXPONENTIATION-op? oper)
  (or (memv oper exponentiation-operator)
      (member-generic oper exponentiation-operator-syntax)))

(define (exponentiation-operators-group? grp)
  (member-generic 'expt grp)) 


(define (is-associative-operator? op)
  (or (AND-op? op)
      (OR-op? op)
      (XOR-op? op)
      (EQUIV-op? op)
      (ADD-op? op)
      (MULTIPLY-op? op)
      (FLOW-op? op)
      (COMPOSITION-op? op)
      (DEFINE-op? op)
      (ASSIGNMENT-op? op)))










;; expression tests

;; test for prefix expression only

(define (isADD? expr)
  (and (pair? expr) (ADD-op? (car expr))))



  ;;(or (eq? oper *) (eq? oper '*) (check-syntax=? oper #'*)))

(define (isMULTIPLY? expr)
  (and (pair? expr) (MULTIPLY-op? (car expr))))

(define (isBRACKET-APPLY? expr)
  (and (pair? expr) (BRACKET-APPLY-op? (car expr))))

(define (isBRACKET-APPLY-VECTOR? expr)
  (and (pair? expr) (BRACKET-APPLY-VECTOR-op? (car expr))))

(define (isBRACKET-APPLY-STRING? expr)
  (and (pair? expr) (BRACKET-APPLY-STRING-op? (car expr))))

(define (isBRACKET-APPLY-HASH-TABLE? expr)
  (and (pair? expr) (BRACKET-APPLY-HASH-TABLE-op? (car expr))))

(define (isBRACKET-APPLY-ARRAY? expr)
  (and (pair? expr) (BRACKET-APPLY-ARRAY-op? (car expr))))

#;(define (isBRACKET-APPLY-FLOMAT? expr)
  (and (pair? expr) (BRACKET-APPLY-FLOMAT-op? (car expr))))

(define (isBRACKET-APPLY*? expr)
  (or (isBRACKET-APPLY? expr)
      (isBRACKET-APPLY-VECTOR? expr)
      (isBRACKET-APPLY-STRING? expr)
      (isBRACKET-APPLY-HASH-TABLE? expr)
      (isBRACKET-APPLY-ARRAY? expr)
      #;(isBRACKET-APPLY-FLOMAT? expr)
      (and (pair? expr) (BRACKET-APPLY-TYPED-INDEX-op? (car expr)))))


(define (isVALUES? expr)
  (and (pair? expr) (VALUES-op? (car expr))))

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

(define (isFLOW? expr)
  (and (pair? expr) (FLOW-op?  (operator expr))))

(define (isCOMPOSITION? expr)
  (and (pair? expr) (COMPOSITION-op?  (operator expr))))

(define (isEXPONENTIATION? expr)
  (and (pair? expr) (ASSIGNMENT-op?  (operator expr))))


(define (isASSOCIATIVE? expr)
  ;;(display "isASSOCIATIVE?") (newline)
  (is-associative-operator? (first expr)))


(define (strict-precedence-over-minus? op)
  ;;(display "strict-precedence-over-minus? : op =") (display op) (newline)
  ;;(display "strict-precedence-over-minus? : strict-precedence-over-minus =") (display strict-precedence-over-minus) (newline)
  (define rv (member-generic op strict-precedence-over-minus))
  ;;(display "strict-precedence-over-minus? : rv =") (display rv) (newline)
  rv
  )


;; (precedence-rank '+)
;; 5

;; (precedence-rank #'**)
;; 1

;; > (precedence-rank '*)
;; REPL Curly Infix:

;; (precedence-rank '*)

;; Parsed annotations. :
;; (precedence-rank '*)

;; 2
;; > (precedence-rank '-)
;; REPL Curly Infix:

;; (precedence-rank '-)

;; Parsed annotations. :
;; (precedence-rank '-)

;; 4
;; the rank of the first group that contains op , or the rank of the last group when no group contains it
(define (precedence-rank op)
  (let loop ((groups infix-operators-lst-for-parser-syntax)
	     (i 0))
    (cond ((null? groups) (- i 1))
	  ((member-generic op (car groups)) i)
	  (else (loop (cdr groups) (+ i 1))))))


(define (operator-precedence>? op1 op2)
  ;;(display "operator-precedence>? op1=") (display op1) (newline)
  ;;(display "operator-precedence>? op2=") (display op2) (newline)
  (> (precedence-rank op2)
     (precedence-rank op1)))


(define (operator-precedence=? op1 op2)
  (display "operator-precedence>? op1=") (display op1) (newline)
  (display "operator-precedence>? op2=") (display op2) (newline)
  (= (precedence-rank op2)
     (precedence-rank op1)))


;; binding power for the Pratt parser (pratt-infix-parser.rkt) : derived from precedence-rank, the
;; tightest group (rank 0, ∘) gets the highest binding power, the loosest group (rank 14, definition
;; operators) gets 0. The *10 factor leaves room for synthetic binding powers that do not correspond to
;; a whole precedence group (example: unary minus's nud needs a right binding power strictly between
;; * (bp 120) and expt (bp 130), see pratt-infix-parser.rkt).
;; only meaningful for op already known to be an operator (same caveat as precedence-rank itself, which
;; returns 14 -- not an error -- for a symbol found in no group at all).
;; (operator-bp '*) -> 120
;; (operator-bp '+) -> 100
;; (operator-bp 'expt) -> 130
(define (operator-bp op)
  (* (- 14 (precedence-rank op)) 10))

#;(define (is-define-overload-procedure? s) ; define-overload-procedure replaced by overload-procedure
  (eq? s 'define-overload-procedure))

(define (is-define-overload? s)
  (or (eq? s 'define-overload-existing-operator)
      ;(eq? s 'define-overload-existing-procedure) ; replaced by overload-procedure
      (eq? s 'define-overload-existing-n-ary-associative-operator)
      ;(is-define-overload-procedure? s) ; replaced by overload-procedure
      ))



;; overload-operator (overload.rkt) defines the overloaded operator if needed and adds an
;; overload, binary with 2 predicates or n-ary with 1 predicate :
;; (overload-operator op funct (pred ...))
;; (overload-operator op (ret-pred ...) funct (pred ...))
;; (overload-operator op module-name funct (pred ...))
;; (overload-operator op module-name (ret-pred ...) funct (pred ...))
;; a separate test from is-overload-existing-operator? : the parser must choose the binary or
;; the n-ary table from the number of predicates, not always the binary one
(define (is-overload-operator? s)
  (eq? s 'overload-operator))

;; require-overload-operator (overload.rkt) : same forms as overload-operator, restated in a module
;; importing the overloaded operator so that the parser knows the overloads done in the other module
(define (is-require-overload-operator? s)
  (eq? s 'require-overload-operator))

(define (is-overload-existing-operator? s)
  (eq? s 'overload-existing-operator))




(define (is-overload-existing-n-ary-operator? s)
  (eq? s 'overload-existing-n-ary-operator))




(define (is-overload-procedure? s)
  (eq? s 'overload-procedure))

(define (is-require-overload-procedure? s)
  (eq? s 'require-overload-procedure))

;; (overload-unary-operator op [module-name] [(ret-pred ...)] funct (pred)) : an overload with exactly one argument , and its
;; require form , for the type annotation pass
(define (is-overload-unary-operator? s)
  (eq? s 'overload-unary-operator))

(define (is-require-overload-unary-operator? s)
  (eq? s 'require-overload-unary-operator))

;; (overload-square-brackets getter setter! (pred-obj pred-index ...)) : for the type annotation pass
(define (is-overload-square-brackets? s)
  (eq? s 'overload-square-brackets))

;; restates , in a module importing them , square brackets overloaded in another module (type annotation only)
(define (is-require-overload-square-brackets? s)
  (eq? s 'require-overload-square-brackets))



;; replaced by overload-procedure / require-overload-procedure
#;(define (is-overload-existing-procedure? s)
  (eq? s 'overload-existing-procedure))

#;(define (is-require-overload-existing-procedure? s)
  (eq? s 'require-overload-existing-procedure))


;; definitions for testing equality to quoted regions
(define (quote-or-quasi? datum)
  (or (eq? datum 'quote)
      (eq? datum 'quasiquote)))

;; definitions for testing equality to unquoted regions and setting and storing the region modes
(define (unquote-or-splicing? datum)
  (or (eq? datum 'unquote)
      (eq? datum 'unquote-splicing)))




(export
	operator?
	operator-syntax?
	operator-symbol-or-syntax?
	not+-?
	NO-OP?
	arithmetic-operator?
	in/equalities-operator?
	operator
	arg1
	arg2
	arg
	args
	function-without-parameters?
	unary-operation?
	binary-operation?
	EQUAL-op?
	NOT-EQUAL-op?
	LESS-op?
	GREATER-op?
	LESS-OR-EQUAL-op?
	GREATER-OR-EQUAL-op?
	IN-EQUALITY-op?
	AND-op?
	OR-op?
	NOT-op?
	BITWISE-NOT-op?
	LOGIC-N-ARY-op?
	XOR-op?
	ADD-op?
	MINUS-op?
	SIGN-op?
	IMPLIC-op?
	EQUIV-op?
	DEFINITION-op?
	DEFINE-op?
	ASSIGNMENT-op?
	FLOW-op?
	COMPOSITION-op?
	EXPONENTIATION-op?
	WALRUS-op?
	MULTIPLY-op?
	BRACKET-APPLY-op?
	BRACKET-APPLY-VECTOR-op?
	BRACKET-APPLY-STRING-op?
	BRACKET-APPLY-HASH-TABLE-op?
	BRACKET-APPLY-ARRAY-op?
	BRACKET-APPLY-TYPED-INDEX-op?
	BRACKET-APPLY-VECTOR-NATURAL-op?
	BRACKET-APPLY-VECTOR-INTEGER-op?
	BRACKET-APPLY-STRING-NATURAL-op?
	BRACKET-APPLY-STRING-INTEGER-op?
	VALUES-op?
	QUOTE-op?
	QUASIQUOTE-op?
	SYNTAX-QUOTE-op?
	CASE-op?
	COND-op?
	LAMBDA-op?
	DO-op?
	is-associative-operator?
	isADD?
	isMULTIPLY?
	isOR?
	isAND?
	isOR-AND?
	isNOT?
	isIMPLIC?
	isEQUIV?
	isXOR?
	isDEFINE?
	isASSIGNMENT?
	isFLOW?
	isCOMPOSITION?
	isEXPONENTIATION?
	isASSOCIATIVE?
	isBRACKET-APPLY?
	isBRACKET-APPLY-VECTOR?
	isBRACKET-APPLY-STRING?
	isBRACKET-APPLY-HASH-TABLE?
	isBRACKET-APPLY-ARRAY?
	isBRACKET-APPLY*?
	isVALUES?
	exponentiation-operators-group?
	strict-precedence-over-minus?
	precedence-rank
	operator-precedence>?
	operator-precedence=?
	operator-bp
	is-define-overload?
	is-overload-operator?
	is-require-overload-operator?
	is-overload-existing-operator?
	is-overload-existing-n-ary-operator?
	is-overload-procedure?
	is-require-overload-procedure?
	is-overload-square-brackets?
	is-overload-unary-operator?
	is-require-overload-unary-operator?
	is-require-overload-square-brackets?
	quote-or-quasi?
	unquote-or-splicing?)
 ; end module
