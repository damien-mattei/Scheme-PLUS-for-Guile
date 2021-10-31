
;; > (add-pair '(1 . 2) '(3 . 4) '(5 . 6))
;; '(9 . 12)
;; > (add-pair '(1 . 2) '(3 . 4))
;; '(4 . 6)
;; > (define p1 '(1 . 2))
;; > (define p2 '(3 . 4))
;; > (add-pair p1 p2)
;; '(4 . 6)
;; > (define p3 '(5 . 6))
;; > (add-pair p1 p2 p3)
;; '(9 . 12)
;; > (add-pair p1)
;; '(1 . 2)
;; > (add-pair '(1 . 2))
;; '(1 . 2)


(define-syntax add-pair
  (syntax-rules ()
    ;; restricted to two operands:
    ;; ((_ p1 p2)
    ;;  (cons (+ (car p1) (car p2))
    ;; 	   (+ (cdr p1) (cdr p2))))

    ;; not good but works like this:
    ;; (add-pair (1 . 2) (3 . 4)) -> '(4 . 6)

    ;;  (add-pair (5 . 4) (5 . 4) (8 . 3) (8 . 4)) -> '(26 . 15)
    ;; ((_ (a . b) ...)
    ;;  (cons (+ a ...) (+ b ...)))
    
     ((_ p ...)
     (cons (+ (car p) ...)
    	   (+ (cdr p) ...)))
    
    ))

    


;;(apply proc-add-pair '((5 . 4) (5 . 4) (8 . 3) (8 . 4)))  ->  '(26 . 15)
(define (proc-add-pair p . more-p)
  (let ((p-list (cons p more-p)))
    (cons (apply + (map car p-list))
	  (apply + (map cdr p-list)))))
