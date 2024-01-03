(define operators-lst
  (apply append infix-operators-lst))


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






;; split the expression using slice as separator
(def (parse-square-brackets-arguments args-brackets)

  ;;(display "curly-infix2prefix4kawa.scm : parse-square-brackets-arguments : args-brackets=") (display args-brackets) (newline)

  (when (null? args-brackets)
	(return args-brackets))

  (declare result partial-result)
  
  (def (psba args) ;; parse square brackets arguments

       ;;(display "psba : args=") (display args) (newline)
       ;;(display "psba : partial-result =") (display partial-result) (newline)
       (when (null? args)
	     ;;(display "before !*prec") (newline)
	     (if (infix?  partial-result)
		 (<- result (append result (!*prec partial-result))) ;; !*prec is defined in optimize-infix.scm
		 (<- result (append result partial-result)))
	     ;;(display "after !*prec") (newline)
	     ;;(display result) (newline)
	     ;;(display "return-rec") (newline)
	     (return-rec result)) ;; return from all recursive calls
       
       (<+ fst  (car args))
       
       (if (equal? slice fst)
	   
	   ($>
	    (when (not (null? partial-result))
		  (if (infix?  partial-result)
		      (<- result (append result (!*prec partial-result))) ;; evaluate and store the expression
		      (<- result (append result partial-result)))
		  (<- partial-result  '())) ;; empty for the next possible portion between slice operator
	    (<- result  (append result (list fst)))) ;; append the slice operator
	   
	   (<- partial-result (append partial-result (list fst)))) ;; not a slice operator but append it

       ;;(display "psba : partial-result=") (display partial-result) (newline)
       
       (psba (cdr args))) ;; end def, recurse


  ;;(display "parse-square-brackets-arguments : args-brackets=") (display args-brackets) (newline)
  (<+ rs  (psba args-brackets))
  ;;(display "parse-square-brackets-arguments : rs=") (display rs) (newline)
  rs
  ) ;; initial call

