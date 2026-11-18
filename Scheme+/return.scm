;; Translated from Scheme+ for Racket (return.rkt) for Guile.

(define-module (Scheme+ return)
  #:use-module (Scheme+ racket-compat))

	

	

#;(define err-string "can only be used inside def,def+,lambda+")
#;(define err-string "can only be used inside def") ; def+ and lambda-x are deprecated
(define err-string "can only be used inside def or lambda+")
	
(define-syntax-parameter return-rec
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
      #;(raise-syntax-error 'return-rec "can only be used inside def,def+,rec+")
      (raise-syntax-error 'return-rec "can only be used inside def"))) ; def+ and rec+ are deprecated
  )

(define-syntax-parameter return
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
    #;(raise-syntax-error 'return "can only be used inside def,def+,lambda+")
    #;(raise-syntax-error 'return "can only be used inside def") ; def+ and lambda-x are deprecated
    (raise-syntax-error 'return "can only be used inside def or lambda+")))
  )



(export
	return
	return-rec)

