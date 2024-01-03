(define-syntax <-
  
  (syntax-rules ()


    ((_ (kar kdr) expr) ; expr must be a pair

     (begin
       (set! kar (car expr))
       (set! kdr (cdr expr))))
    
    
    ;;(<- x 5)
    ((_ var expr)
     
     ;;(begin
       ;;(display "<- : variable set!") (newline)
       (set! var expr))
       ;;var))

    
    ;; (declare x y z t)
    ;; {x <- y <- z <- t <- 7}
    ;; 7
    ;; (list x y z t)
    ;; (7 7 7 7)

    ;; > (require srfi/25)
    ;; > {I <- (make-array (shape 0 4 0 4))}
    ;; #<array:srfi-9-record-type-descriptor>
    ;; > {I[0 0] <- I[1 1] <- I[2 2] <- I[3 3] <- 1}
    ;; 1
    ;; > {I[0 0]}
    ;; 1
    ;; > {I[0 1]}
    ;; 0
    ;; > I
    ;; #<array:srfi-9-record-type-descriptor>
    
    ((_ var var1 ... expr)
     
     ;;(<- var (<- var1 ... expr)))

    (begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result 
	(<- var expr)
	(<- var1 var)
	...
	))
     
    ))

