;; > (for (k 0 10) (display k) (newline))
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

;; > (for (k 0 10 3) (display k) (newline) (newline) (for (j 0 3) (display j) (newline)) (newline))
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

(define-syntax for
  (syntax-rules ()

    ((_ ((i to)) b1 ...) ;; for DrRacket compatibility
     
    
     (let loop ((i 0))
       (when (< i to)
	     b1 ...
	     (loop (incf i)))))
    
    ((_ (i to) b1 ...)
     
     (let loop ((i 0))
       (when (< i to)
	     b1 ...
	     (loop (incf i)))))

    
    
    ((_ (i from to) b1 ...)

     (let loop ((i from))
       (when (<= i to)
	     b1 ...
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


;; > (for/break breaky (i 1 4) (for/break breakable (j 1 2) (display-nl i)))
;; 1
;; 1
;; 2
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for/break breaky (i 1 4) (for/break breakable (j 1 2) (display-nl i) (when (= i 2) (breakable))))
;; 1
;; 1
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for/break breaky (i 1 4) (for/break breakable (j 1 2) (display-nl i) (when (= i 2) (breaky))))
;; 1
;; 1
;; 2
;; >
;; > (define x 0)
;; > (for/break breaky (i 1 3) (set! x i) )
;; > x
;; 3
;;
;;  (for/break breaky (i 1 3) (if (= i 3)  (breaky i) '()) )
;; 3
(define-syntax for/break
  (syntax-rules ()
    ((_ <break-id> (i from to) b1 ...)
     (call/cc (lambda (<break-id>)
		(let loop ((i from))
        	  (when (<= i to)
			(begin b1 ...
			       (loop (incf i))))))))))

