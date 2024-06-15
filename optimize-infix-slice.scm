;; some optimizer procedures




;; !*prec is defined in optimize-infix.scm

;; split the expression using slice as separator
(def (optimizer-parse-square-brackets-arguments args-brackets creator operator-precedence)

  ;;(display "optimizer-parse-square-brackets-arguments : args-brackets=") (display args-brackets) (newline)

  (define operators-lst (apply append operator-precedence))
  
  (when (null? args-brackets)
	(return args-brackets))

  (declare result partial-result)
  
  (def (psba args) ;; parse square brackets arguments ,note: it is a tail-recursive function (see end)

       ;;(display "psba : args=") (display args) (newline)
       ;;(display "psba : partial-result =") (display partial-result) (newline)
       (when (null? args)
  	     ;;(display "before !*prec") (newline)
  	     (if (infix?  partial-result operators-lst)
  		 (set! result (append result (!*prec-generic partial-result  operator-precedence creator))) ;; !*prec-generic is defined in optimize-infix.scm
  		 (set! result (append result partial-result)))
  	     ;; (display "after !*prec") (newline)
  	     ;; (display result) (newline)
  	     ;; (display "return-rec") (newline)
  	     (return-rec result)) ;; return from all recursive calls, as it is tail recursive
       
       (define fst (car args))

       ;;(display "fst=") (display fst) (newline)

       ;; test here for ',' for multi-dim arrays , that will remove the use of { } in [ ]
       (if (datum=? slice fst) ; separator
	      
  	   ($>
  	    ;;(display "slice detected") (newline)
  	    ;;(display "psba : partial-result =") (display partial-result) (newline)
  	    (when (not (null? partial-result))
  		  ;;(display "not null") (newline)
  		  (if (infix?  partial-result operators-lst) ;;  operateurs quotés ou syntaxés !
  		      (begin
  			;;(display "infix detected") (newline)
  			(set! result (append result (!*prec-generic partial-result  operator-precedence creator)))) ;; convert to prefix and store the expression
  		      (set! result (append result partial-result))) ; already atom
  		  (set! partial-result  '())) ;; empty for the next possible portion between slice operator
  	    (set! result  (append result (list fst)))) ;; append the slice operator
	   
  	   (set! partial-result (append partial-result (list fst)))) ;; not a slice operator but append it

       ;;(display "psba : result=") (display result) (newline)
       ;;(display "psba 2 : partial-result=") (display partial-result) (newline)
       
       (psba (cdr args))) ;; end def, recurse (tail recursive)


  ;;(display "parse-square-brackets-arguments : args-brackets=") (display args-brackets) (newline)
  (define rs  (psba args-brackets))
  ;;(display "parse-square-brackets-arguments : rs=") (display rs) (newline)
  rs
  ) ;; initial call



(define (optimizer-parse-square-brackets-arguments-lister args-brackets)
  ;;(display "optimizer-parse-square-brackets-arguments-lister : args-brackets=") (display args-brackets) (newline)
  (optimizer-parse-square-brackets-arguments args-brackets
					     (lambda (op a b) (list op a b))
					     infix-operators-lst-for-parser))


(define (optimizer-parse-square-brackets-arguments-lister-syntax args-brackets)
  (display "optimizer-parse-square-brackets-arguments-lister-syntax : args-brackets=") (display args-brackets) (newline)
  (optimizer-parse-square-brackets-arguments args-brackets ;; generic procedure
					     (lambda (op a b) (list op a b))
					     infix-operators-lst-for-parser-syntax)) ;; defined elsewhere

;; DEPRECATED
;; (define (optimizer-parse-square-brackets-arguments-evaluator args-brackets)
;;   ;;(display "optimizer-parse-square-brackets-arguments-evaluator : args-brackets=") (display args-brackets) (newline)
;;   (optimizer-parse-square-brackets-arguments args-brackets
;; 					     (lambda (op a b) (op a b))
;; 					     (get-operator-precedence)))

