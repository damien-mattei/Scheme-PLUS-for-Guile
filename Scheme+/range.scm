;; Translated from Scheme+ for Racket (range.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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


;; this whole module is DEPRECATED for Racket as already exists 'range'  


(define-module (Scheme+ range)
  #:use-module (Scheme+ racket-compat))


  
  
  (use-modules
	(Scheme+ for_next_step))

  



;; |kawa:2|# (in-range 5)
;; (0 1 2 3 4)
;; #|kawa:3|# (in-range 1 5)
;; (1 2 3 4)
;; #|kawa:4|# (in-range 1 5 2)
;;(1 3)
(define (in-range . arg-lst)
  (define n (length arg-lst))
  
  (define start 0)
  (define stop 1)
  (define step 1)
  (define res '())

  (define (arret step index stop)
    (if (> step 0)
	(< index stop)
	(> index stop)))

  (define i 0)

  (when (or (= n 0) (> n 3))
	(error "in-range: bad number of arguments"))
  
  (case n
    ((0) (error "in-range : too few arguments"))
    ((1) (set! stop (car arg-lst)))
    ((2) (begin (set! start (car arg-lst))
		(set! stop (cadr arg-lst))))
    ((3) (begin (set! start (car arg-lst))
		(set! stop (cadr arg-lst))
		(when (= 0 step)
		      (error "in-range: step is equal to zero"))
		(set! step (caddr arg-lst)))))

  
  (set! i start)
  
  ;;(for ((define i start) (arret step i stop) (set! i (+ step i)))
  (for ('() (arret step i stop) (set! i (+ step i)))
       (set! res (cons i res)))

  (reverse res))





;; #|kawa:3|# (in-range 1 11 3)
;; (1 4 7 10)
;; #|kawa:4|# (reversed (in-range 1 11 3))
;; (10 7 4 1)
;; #|kawa:5|# (in-range 8 1 -2)
;; (8 6 4 2)
;; #|kawa:6|# (reversed (in-range 8 1 -2))
;; (2 4 6 8)
;; #|kawa:7|# (in-range 8 1 -1)
;; (8 7 6 5 4 3 2)
;; #|kawa:8|# (reversed (in-range 8 1 -1))

(define reversed reverse)








(export
	in-range
	reversed)
 ; end module





;; below is old version of reversed but probably buggy

;; ;> (for ([x (in-range 0 3)]) (display x) (newline) )
;; ;0
;; ;1
;; ;2
;; ;> (for ([x (reversed (in-range 0 3))]) (display x) (newline) )
;; ;2
;; ;1
;; ;0
;; (define-syntax reversed ; same as Python : reversed(range(0,3))
  
;;   		(syntax-rules ()

;;     			((_ (name end)) (begin
;; 					  (unless (equal? (quote in-range) (quote name)) 
;; 	       					(error "first argument is not in-range:" (quote name)))
;; 					  ;;(in-range {end - 1} -1 -1)))
;; 					  (in-range (- end 1) -1 -1)))

;; 			((_ (name start end)) (begin
;; 					  	(unless (equal? (quote in-range) (quote name)) 
;; 	       						(error "first argument is not in-range:" (quote name)))
;; 					  	;;(in-range {end - 1} {start - 1} -1)))))
;; 						(in-range (- end 1) (- start 1) -1)))

;; 			((_ (name start end step)) (begin
;; 						     (unless (equal? (quote in-range) (quote name)) 
;; 							     (error "first argument is not in-range:" (quote name)))
;; 						     ;;(in-range {end - 1} {start - 1} (- step))))))
;; 						     (in-range (+ (- end step) 1) (- start 1) (- step)))))) ;; ERROR



