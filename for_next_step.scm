;; Copyright 2022 Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;(require (rename-in racket/base [for for-rack])) ;; backup original Racket 'for'


;; (define-module (for-next-step)
;;    #:export (break continue for))

;; > (for-basic ((k 5)) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5


;; > (for-basic (k 0 10) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; '()

;; > (for-basic (k 0 10 3) (display k) (newline) (newline) (for-basic (j 0 3) (display j) (newline)) (newline))
;; 0

;; 0
;; 1
;; 2
;; 3

;; 3

;; 0
;; 1
;; 2
;; 3

;; 6

;; 0
;; 1
;; 2
;; 3

;; 9

;; 0
;; 1
;; 2
;; 3

;; '()
;;

;; DEPRECATED
(define-syntax for-basic
  
  (syntax-rules ()

    ;; ((_ ((i to)) b1 ...) ;; for old compatibility
     
    ;;  (let loop ((i 0))
    ;;    (when (< i to)
    ;; 	     b1 ...
    ;; 	     (loop (incf i)))))
    
    ((_ (i to) b1 ...)
     
     (let loop ((i 0))
       (when (< i to)
	     b1 ...
	     (loop (incf i)))))

    
    
    ((_ (i from to) b1 ...)

     (let loop ((i from))
       (when (<= i to)
	     (let () ;; why only this one have let ?
	       b1 ...)
	     (loop (incf i)))))
    
    ((_ (i from to step) b1 ...)
     
     (let loop ((i from))
       (when (<= i to)
	     b1 ...
	     (loop (+ i step)))))))


;; (for-next k = 0 to 10  (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; > (for-next k = 0 to 5 step 2 (display k) (newline))
;; 0
;; 2
;; 4
;; > (for-next (k = 0 to 5 step 2) (display k) (newline))
;; 0
;; 2
;; 4
;; > (for-next (k = 0 to 5) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; > 
;; DEPRECATED
(define-syntax for-next
  
  (syntax-rules (= to step)


    ;; The patterns are matched top down (the one with step must be before others patterns)
    ((for-next i = start to finish step inc b1 ...)
     
     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (+ i inc)))))

    
    ((for-next i = start to finish b1 ...)

     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (incf i)))))
    
   
    
    ((for-next (i = start to finish) b1 ...)

     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (incf i)))))
    
    ((for-next (i = start to finish step inc) b1 ...)
     
     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (+ i inc)))))))

;; deprecated
;; (define-syntax for-next-step
;;   (syntax-rules (= to step)
    
;;     ((for-next-step i = start to finish step inc b1 ...)
     
;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (+ i inc)))))))


;; > (for-basic/break breaky (i 1 4) (for-basic/break breakable (j 1 2) (display-nl i)))
;; 1
;; 1
;; 2
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for-basic/break breaky (i 1 4) (for-basic/break breakable (j 1 2) (display-nl i) (when (= i 2) (breakable))))
;; 1
;; 1
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for-basic/break breaky (i 1 4) (for-basic/break breakable (j 1 2) (display-nl i) (when (= i 2) (breaky))))
;; 1
;; 1
;; 2
;; >
;; > (define x 0)
;; > (for-basic/break breaky (i 1 3) (set! x i) )
;; > x
;; 3
;;
;;  (for-basic/break breaky (i 1 3) (if (= i 3)  (breaky i) '()) )
;; 3

;; (for-basic/break breaky (i 1 3) (display i) (newline) (if (= i 2)  (begin (breaky i)) '()) )
;; 1
;; 2
;; $3 = 2
;; DEPRECATED
(define-syntax for-basic/break
  (syntax-rules ()
    ((_ <break-id> (i from to) b1 ...)
     (call/cc (lambda (<break-id>)
		(let loop ((i from))
        	  (when (<= i to)
			;;(begin b1 ...
			b1 ...
			(loop (incf i)))))))));)

;; scheme@(guile-user)>  (for-basic/break-cont break continue (i 1 3) (display i) (newline) (if (= i 2)  (begin (continue) (break)) '()) )
;; 1
;; 2
;; 3
;; DEPRECATED
(define-syntax for-basic/break-cont
  (syntax-rules ()
    ((_ <break-id> <continue-id> (i from to) b1 ...)
     (call/cc (lambda (<break-id>)
		(let loop ((i from))
        	  (when (<= i to)
			(call/cc (lambda (<continue-id>) b1 ...))
			(loop (incf i)))))))))


;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} (incf i)) (display i) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4

;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} {i <- {i + 1}}) (display i) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4

;;  (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline))
;; 7
;; 7
;; 7
;; 7
;; 7
;; (define-syntax for
  
;;   (syntax-rules ()
    
;;     ((_ (init test incrmt) b1 ...)

;;        (let ()
;; 	 init
;; 	 (let loop ()
;; 	   (when test
;; 		 ;;(let ()
;; 		   b1 ...
;; 		   incrmt
;; 		   (loop)))))));)


;; scheme@(guile-user)> (for/break-cont break continue ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline))
;; 7
;; 7
;; 7
;; 7
;; 7

;; scheme@(guile-user)> (for/break-cont break continue ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline) (break))
;; 7

;; scheme@(guile-user)> (for/break-cont break continue ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (continue) (display x) (newline) (break))
;; perheaps not usefull DEPRECATED? but compatible with R6RS and R5RS
(define-syntax for/break-cont
  
  (syntax-rules ()
    
    ((_ <break-id> <continue-id> (init test incrmt) b1 ...)

     (call/cc (lambda (<break-id>)
		(let ()
		  init
		  (let loop ()
		    (when test
			  (call/cc (lambda (<continue-id>) b1 ...))
			  incrmt
			  (loop)))))))))



;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline) (break))
;; 7
;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (continue) (display x) (newline) (break))
;; scheme@(guile-user)>

;; (for ({k <+ 0} {k < 3} {k <- {k + 1}})
;;      (display k)
;;      (newline)
;;      (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7}
;; 	  (display x)
;; 	  (newline)
;; 	  (break))
;;      (newline))

;; 0
;; 7

;; 1
;; 7

;; 2
;; 7

;; does not works as expected (break is from srfi-1)
;; (for ({k <+ 0} {k < 3} {k <- {k + 1}})
;;      (display k)
;;      (break even? '(3 1 4 1 5 9))
;;      (newline)
;;      (continue)
;;      (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7}
;; 	  (display x)
;; 	  (newline)
;; 	  (break))
;;      (newline))

;; 0
;; 1
;; 2

;; (define-syntax for
  
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((kwd (init test incrmt) body ...)
	  
;;        (with-syntax
;; 	((BREAK (datum->syntax #'kwd 'break))
;; 	 (CONTINUE (datum->syntax #'kwd 'continue)))

;; 	#`(call/cc
;; 	   (lambda (escape)
;; 	     (let-syntax
;; 		 ((BREAK (identifier-syntax (escape))))
;; 	       init
;; 	       (let loop ()
;; 		 (when test

;; 		       #,#'(call/cc
;; 			    (lambda (next)
;; 			      (let-syntax
;; 				  ((CONTINUE (identifier-syntax (next))))
;; 				body ...)))
			  
;; 		       incrmt
;; 		       (loop)))))))))))


;; note: may require the specific 'when (that allows inner definitions)

;; below syntax is incompatible with SRFI-105 implementation for Racket
;; (define-syntax for
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((kwd (init test incrmt) body ...)
;;        (with-syntax ((BREAK (datum->syntax #'kwd 'break))
;; 		     (CONTINUE (datum->syntax #'kwd 'continue)))
;; 		    #'(call/cc
;; 		       (lambda (escape)
;; 			 (let-syntax ((BREAK (identifier-syntax (escape))))
;; 			   init
;; 			   (let loop ()
;; 			     (when test
;; 				   (call/cc
;; 				    (lambda (next)
;; 				      (let-syntax ((CONTINUE (identifier-syntax (next))))
;; 					body ...)))
;; 				   incrmt
;; 				   (loop)))))))))))
;;
;; insert in header:
;; (require (for-syntax r6rs/private/base-for-syntax))

(define-syntax for
   (lambda (stx)
     (syntax-case stx ()
       ((kwd (init test incrmt) body ...)
        (with-syntax ((BREAK (datum->syntax (syntax kwd) 'break))
                      (CONTINUE (datum->syntax (syntax kwd) 'continue)))
		     (syntax
		      (call/cc
		       (lambda (escape)
			 (let-syntax ((BREAK (identifier-syntax (escape))))
			   init
			   (let loop ()
			     (when test
				   (call/cc
				    (lambda (next)
				     (let-syntax ((CONTINUE (identifier-syntax (next))))
				       body ...)))
				   incrmt
				   (loop))))))
		      ) ;; close syntax
		     
		     )))))


;; (for/bc ({k <+ 0} {k < 3} {k <- {k + 1}})
;;      (display k)
;;      (break even? '(3 1 4 1 5 9))
;;      (newline)
;;      (continue)
;;      (for/bc ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7}
;; 	  (display x)
;; 	  (newline)
;; 	  (break))
;;      (newline))

;; (use-modules (ice-9 control))


;; (define-syntax-parameter break
;;    (lambda (sintax)
;;      (syntax-violation 'break "break outside of for/bc" sintax)))

;; (define-syntax-parameter continue
;;    (lambda (sintax)
;;      (syntax-violation 'continue "continue outside of for/bc" sintax)))

;; (define-syntax-rule (for/bc (init test increment) body body* ...)
;;    (let () ;; (begin 
;;      init
;;      (let/ec escape
;;        (syntax-parameterize ((break (identifier-syntax (escape))))
;;          (let loop ()
;;            (when test
;;              (let/ec next
;;                (syntax-parameterize ((continue (identifier-syntax (next))))
;;                  body body* ...))
;;              increment
;;              (loop)))))))



;; (let ((i #f))
;;    (for/bc ((set! i 0) (< i 10) (set! i (1+ i)))
;;      (when (< i 5)
;;        continue)
;;      (when (> i 9)
;;        break)
;;      (display i)
;;      (newline)))






;; (define-syntax for/bc
;;    (lambda (stx)
;;      (syntax-case stx ()
;;        ((kwd (init test incrmt) body ...)
;;         #`(call/cc
;;            (lambda (escape)
;;              (let-syntax ((#,(datum->syntax #'kwd 'break)
;;                            (identifier-syntax (escape))))
;;                init
;;                (let loop ()
;;                  (when test
;;                    (call/cc
;;                     (lambda (next)
;;                       (let-syntax ((#,(datum->syntax #'kwd 'continue)
;;                                     (identifier-syntax (next))))
;;                         body ...)))
;;                    incrmt
;;                    (loop))))))))))
