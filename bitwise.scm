(define (<< x n)
  (ash x n))

(define (>> x n)
  (ash x (- n)))

(define & logand)
(define ∣ logior)

;;(define | logior)  ;; this pipe is weird

