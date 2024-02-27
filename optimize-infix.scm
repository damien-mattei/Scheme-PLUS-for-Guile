;; guile version




;;; evaluates `terms` as a basic infix expression
(define (!0 infix-operators terms)
  ;;(display "!0 : infix-operators=") (display infix-operators) (newline)
  ;;(display "!0 : terms=") (display terms) (newline)
  (if (null? terms) ;; i added this null case but with correct input this should not be necessary
      terms
      (car (!* terms
	       infix-operators
	       #f))))



;; evaluate one group of operators in the list of terms
(define (!** terms stack operators odd?)


  ;;(display "!** : terms = ") (display terms) (newline)
  ;;(display "!** : operators = ") (display operators) (newline)
  ;;(display "!** : stack = ") (display stack) (newline)
  ;;(display "!** : odd? = ") (display odd?) (newline)

					; why `odd?`? because scheme's list-iteration is forwards-only and
					; list-construction is prepend-only, every other group of operators is
					; actually evaluated backwards which, for operators like / and -, can be a
					; big deal! therefore, we keep this flipped `odd?` counter to track if we
					; should flip our arguments or not



  (define (calc op a b)
    (if odd? (list op a b) (list op b a)))

  
 
  (cond ((null? terms) stack) ; base case
	
	;; operator we can evaluate -- pop operator and operand, then recurse
	((and (> (length stack) 1) ;; (begin
				   ;;   (display "!** : operators=") (display operators) (newline)
				   ;;   (let* ((op (car stack))
				   ;; 	    (mres (memq op operators)))
				   ;;     (display "op=") (display op) (newline)
				   ;;     (display "mres=") (display mres) (newline) (newline)
				   ;;     mres)))
	      (memq (car stack) operators))
	      
	 (let ((op (car stack))
	       (fst (car terms)) ;; a
	       (snd (cadr stack))) ;; b
	   ;;(display "op=") (display op) (newline)
	   (!** (cdr terms)
		(cons (calc op fst snd) (cddr stack))
		operators
		(not odd?))))


	
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
      terms ; finished processing operator groups
      ;; evaluate another group -- separating operators into groups allows
      ;; operator precedence
      (!* (!** terms '() (car operator-groups) odd?)
	  (cdr operator-groups)
	  (not odd?))))



(define (!*prec terms)   ;; precursor of !*
  (if (null? terms) 
      terms
      (begin
	;;(display "!*prec : version=") (display (car infix-operators-lst)) (newline)
	(!* terms infix-operators-lst-for-parser #f))))
