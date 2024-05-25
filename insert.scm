(define-module (insert)

  #:export ( insert
	     insert-set!)) ;; end module declaration


;; library procedures and macro
(define insert cons)

;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))
