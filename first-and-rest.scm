;; this for scheme implementation other than DrRacket and that do not have first and rest
;; (define-syntax first
;;   (syntax-rules ()
;;     ((_ p) (car p))))

;; (define-syntax rest
;;   (syntax-rules ()
;;     ((_ p) (cdr p))))


(define first car)
(define rest cdr)

