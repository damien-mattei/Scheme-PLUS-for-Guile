;; this one will not works with map:
;; (define-syntax rest
;;   (syntax-rules ()
;;     ((_ p) (cdr p))))


;; scheme@(guile-user)> (map rest '((1 . 2) (3 . 4)))
;; (2 4)
(define rest cdr)
