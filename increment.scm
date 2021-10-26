;; increment variable
;; this is different than add1 in DrRacket
(define-syntax incf
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1))
		    x))))

(define-syntax add1
  (syntax-rules ()
    ((_ x)   (+ x 1))))
