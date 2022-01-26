

;; (local (x 1 y (+ x 1) z (+ 2 y)) z y) -> 2

;; (local [ x 1
;; 	    y (+ x 1)
;; 	    z (+ 2 y) ]
;;        z y)
;; 2

(define-syntax local

  (syntax-rules ()

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var val) expr  ...) (let ((var val)) expr ...))
    
    ((_ (var1 val1 var2 val2 ...) expr  ...) (let ((var1 val1))
					       (local (var2 val2 ...) expr ...))) ))


;;  special forms with arrows

;;scheme@(guile-user)> (let<-rec* (x <- 1
;;                                 y <- (+ x 1)
;;                                 z <- (+ 2 y))
;;                               z)
;; 4
;;scheme@(guile-user)> (let<-rec* [x <- 1
;;                                 y <- (+ x 1)
;;                                 z <- (+ 2 y)]
;;                               z)
;; 4
(define-syntax let<-rec*

  (syntax-rules (<-)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    ((_ (var1 <- val1) expr  ...) (letrec* ((var1 val1)) expr ...)) ;; case single binding
    ((_ (var1 <- val1 ;; multiple binding
	 var2 <- val2 ...)
	expr ...)
     (%parse-letrec-bindings-and-evaluate-expressions ((var1 val1))  (var2 <- val2 ...) expr ...))))

;; recursive macro with accumulator
(define-syntax %parse-letrec-bindings-and-evaluate-expressions

  (syntax-rules (<-)

    ;; note: _ is %parse-letrec-bindings-and-evaluate-expressions
    ((_ (bindings ...) (var1 <- val1) expr ...)  (letrec* (bindings ... (var1 val1)) ;; last binding
						   expr ...)) ;; we evaluate expressions
    
    ((_ (bindings ...) (var1 <- val1 ;; multiple bindings
			var2 <- val2
			...)
	
        	        expr ...)

     
     ;; store (var1 val1) binding in accumulator and continue parsing
     (%parse-letrec-bindings-and-evaluate-expressions (bindings ... (var1 val1))
						      (var2 <- val2
						       ...)
						      
						      expr ...)))) 
    



;; (let-arrow* (x ← 1
;; 	        y ← {x + 1})
;;             y)
;; $1 = 2

;; (let-arrow* (x <- 1
;;              y <- (+ x 1)
;;              z <- (+ 2 y))
;;             z) = 4

;; (let-arrow* [ x 1
;;               y (+ x 1)
;;               z (+ 2 y) ]
;;            z y)
;; = 2

;; scheme@(guile-user)>  (let-arrow* ({x ← 1}
;;                                    {y ← {x + 1}})
;;                                  x
;;                                  y)
;; $2 = 2
(define-syntax let-arrow*

  (syntax-rules (<- ->  ← →) ;;  ⟵ ⟶

    ((_ () expr ...) (begin expr ...)) ;;  case empty let

    
    ;; let with arrows
    ((_ (var <- val) expr  ...) (let ((var val)) expr ...))
    ((_ (val -> var) expr  ...) (let ((var val)) expr ...))
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (var2 <- val2 ...) expr ...)))
    ((_ (val1 -> var1 val2 -> var2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (var1 <- val1 val2 -> var2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (val1 -> var1 var2 <- val2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (var2 <- val2 ...) expr ...)))

    ((_ ((<- var val)) expr ...)    (let ((var val)) expr ...))
    ((_ ((<- var1 val1) (<- var2 val2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((<- var2 val2) ...) expr ...)))
    ((_ ((<- var1 val1) (-> val2 var2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val var)) expr ...)    (let ((var val)) expr ...))
    ((_ ((-> val1 var1) (-> val2 var2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val1 var1) (<- var2 val2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((-> val2 var2) ...) expr ...)))
    
    ((_ (var ← val) expr  ...) (let ((var val)) expr ...))
    ((_ (val → var) expr  ...) (let ((var val)) expr ...))
    ((_ (var1 ← val1 var2 ← val2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (var2 ← val2 ...) expr ...)))
    ((_ (val1 → var1 val2 → var2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (val2 → var2 ...) expr ...)))
    ((_ (var1 ← val1 val2 → var2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (val2 → var2 ...) expr ...)))
    ((_ (val1 → var1 var2 ← val2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (var2 ← val2 ...) expr ...)))

    ((_ ((← var val)) expr ...)    (let ((var val)) expr ...))
    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((← var2 val2) ...) expr ...)))
    ((_ ((← var1 val1) (→ val2 var2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val var)) expr ...)    (let ((var val)) expr ...))
    ((_ ((→ val1 var1) (→ val2 var2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val1 var1) (← var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((→ val2 var2) ...) expr ...)))
    

    
    ;; Warning : long arrow causes problems in Mac terminal at least
    ;; ((_ (val ⟶ var) expr  ...) (let ((var val)) expr ...))
    ;; ((_ (var ⟵ val) expr  ...) (let ((var val)) expr ...))
    ;; ((_ (val1 ⟶ var1 val2 ⟶ var2 ...) expr  ...) (let ((var1 val1))
    ;; 						   (let-arrow* (val2 ⟶ var2 ...) expr ...)))
    ;; ((_ (var1 ⟵ val1 var2 ⟵ val2 ...) expr  ...) (let ((var1 val1))
    ;; 						   (let-arrow* (var2 ⟵ val2 ...) expr ...)))
    ;; miss other definitions... for the long arrow

    ;; let with less brackets
    ((_ (var val) expr  ...) (let ((var val)) expr ...))
    ((_ (var1 val1 var2 val2 ...) expr  ...) (let ((var1 val1))
					       (let-arrow* (var2 val2 ...) expr ...)))
    
    ))



;; (letrec-arrow* [ fact ← (lambda (n)
;; 			  (if  {n = 1}
;; 			       1
;;                                {n * (fact {n - 1})}))
;;                           ]
;; 	       (fact 5))

;; = 120

(define-syntax letrec-arrow*

  (syntax-rules (<- ->  ← →) ;;  ⟵ ⟶

    ((_ () expr ...) (begin expr ...)) ;;  case empty letrec

    

    ;; letrec with arrows
    ((_ (var <- val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (val -> var) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (var2 <- val2 ...) expr ...)))
    ((_ (val1 -> var1 val2 -> var2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (var1 <- val1 val2 -> var2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (val1 -> var1 var2 <- val2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (var2 <- val2 ...) expr ...)))

    ((_ ((<- var val)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((<- var1 val1) (<- var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((<- var2 val2) ...) expr ...)))
    ((_ ((<- var1 val1) (-> val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val var)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((-> val1 var1) (-> val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val1 var1) (<- var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((-> val2 var2) ...) expr ...)))
    
    ((_ (var ← val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (val → var) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var1 ← val1 var2 ← val2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (var2 ← val2 ...) expr ...)))
    ((_ (val1 → var1 val2 → var2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (val2 → var2 ...) expr ...)))
    ((_ (var1 ← val1 val2 → var2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (val2 → var2 ...) expr ...)))
    ((_ (val1 → var1 var2 ← val2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (var2 ← val2 ...) expr ...)))

    ((_ ((← var val)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((← var2 val2) ...) expr ...)))
    ((_ ((← var1 val1) (→ val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val var)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((→ val1 var1) (→ val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val1 var1) (← var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((→ val2 var2) ...) expr ...)))
    ;; letrec with less brackets
    ((_ (var val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var1 val1 var2 val2 ...) expr  ...) (letrec ((var1 val1))
    					       (letrec-arrow* (var2 val2 ...) expr ...)))
    ))



