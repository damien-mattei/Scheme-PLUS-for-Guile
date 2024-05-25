(define (<< x n)
  (ash x n))

(define (>> x n)
  (ash x (- n)))

(define & logand)
(define ∣ logior)
(define ^ logxor)

;;(define | logior)  ;; this pipe is weird

