(use-modules (Scheme+))

;; (fibonacci 13)
;; 7
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))


;; (fib 11)
;; 89
(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))


;; (fib2 27)
;; 196418

(define (fib2 n)
  (if {n < 2}
      n
      { fib2({n - 1}) + fib2({n - 2}) }
      ))

;;(fib3 47)
;; 2971215073 ;; but it takes time in minutes...
(define (fib3 n)
  (if {n < 2}
      n
      { fib3{n - 1} + fib3{n - 2} }
      ))


(define size0 10000)
(define memo0 (make-vector size0 0))

;; example:
;; scheme@(guile-user)> (fibdyna0 57)
;; 365435296162

(define (fibdyna0 n)
  (cond ((< n 2) n)
	((not (zero? (vector-ref memo0 n))) (vector-ref memo0 n))
	(else (let ((fibn (+ (fibdyna0 (- n 1)) (fibdyna0 (- n 2)))))
		(vector-set! memo0 n fibn)
		fibn))))



;; cheme@(guile-user)> (fibdyna 27)
;; 196418
;; scheme@(guile-user)> (fibdyna 47)
;; 2971215073 ; immediate result, no more take times

;; scheme@(guile-user)> (fibdyna 257)
;; 229265413057075367692743352179590077832064383222590237

{size <+ 10000}
{memo <+ (make-vector size 0)} 
  
(define (fibdyna n)
  (cond ({n < 2} n)
	({memo[n] <> 0} {memo[n]})                ;; and not Nemo who is a fish ;-) do not make typos as me :-O or you get: Unbound variable: nemo :-)
	(else {memo[n] <- {(fibdyna {n - 1}) + (fibdyna {n - 2})}}) ))



