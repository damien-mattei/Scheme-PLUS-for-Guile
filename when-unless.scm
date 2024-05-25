



;; definitions redefined here only to allow 'define in body as allowed in Scheme+
(define-syntax when
  (syntax-rules ()
    ;;((when test result1 result2 ...)
    ((when test result1  ...)
     (cond (test
         ;;(begin result1 result2 ...)))))
	 ;;(let () result1 result2 ...)))))
	 (let () result1 ...))))))

(define-syntax unless
  (syntax-rules ()
    ;;((unless test result1 result2 ...)
    ((unless test result1 ...)
     (cond ((not test)
         ;;(begin result1 result2 ...)))))
	 ;;(let () result1 result2 ...)))))
	 (let () result1 ...))))))
