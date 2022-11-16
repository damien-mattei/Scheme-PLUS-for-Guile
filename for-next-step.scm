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


;; TODO: create 'for' that works with 'down to' step

;;(require (rename-in racket/base [for for-rack])) ;; backup original Racket 'for'

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
	     (let ()
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
(define-syntax for-basic/break
  (syntax-rules ()
    ((_ <break-id> (i from to) b1 ...)
     (call/cc (lambda (<break-id>)
		(let loop ((i from))
        	  (when (<= i to)
			(begin b1 ...
			       (loop (incf i))))))))))



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


(define-syntax for
  
  (syntax-rules ()
    
    ((_ (init test incrmt) b1 ...)

       (let ()
	 init
	 (let loop ()
	   (when test
		 (let ()
		   b1 ...
		   incrmt
		   (loop))))))))


