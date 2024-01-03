;; guile version


(define-module (infix-operators)

	#:export ( infix-operators-lst
		   set-infix-operators-lst!
		   replace-operator! )) ;; end module declaration

(include-from-path "exponential.scm")
(include-from-path "modulo.scm")
(include-from-path "bitwise.scm")
(include-from-path "not-equal.scm")

(include-from-path "first-and-rest.scm")
(include-from-path "list.scm")
	

;; can you believe they made && and || special forms??? yes :-) but with advantage of being short-circuited,but i admit it has been a headlock for an infix solution 
;; note: difference between bitwise and logic operator


;; a list of lists of operators. lists are evaluated in order, so this also
;; determines operator precedence
;;  added bitwise operator with the associated precedences and modulo too
(define infix-operators-lst
  
  
  (list 0
	
	(list expt **)
	(list * / %)
	(list + -)
	
	(list << >>)

	;;(list & | ) ;; this pipe is weird
	
	(list & ∣ )
  					; now this is interesting: because scheme is dynamically typed, we aren't
  					; limited to any one type of function
	
	(list < > = ≠ <= >=) ;; <> not compatible with guile
	
	;;(list 'dummy) ;; can keep the good order in case of non left-right assocciative operators.(odd? reverse them) 
	
	)

  )


(define (set-infix-operators-lst! lst)
  (set! infix-operators-lst lst))

(define (replace-operator! op-old op-new)
  (display "replace-operator! :") (newline)
  ;; (display op-old) (newline)
  ;; (display op-new) (newline)
  (display infix-operators-lst) (newline)(newline)
  (define version-number (car infix-operators-lst))
  (define new-infix-operators-lst (replace infix-operators-lst op-old op-new))
  (set-infix-operators-lst! (cons (+ 1 version-number) ; increment the version number
				  (cdr new-infix-operators-lst)))
  (display infix-operators-lst) (newline)(newline))



