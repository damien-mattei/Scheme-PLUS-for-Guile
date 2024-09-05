;; Growable Vectors (1 dimension)

;; Copyright 2021-2023 Damien MATTEI

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


#!curly-infix

;; use:
;; (use-modules (growable-vector))
;; (use-modules (guile/growable-vector))  
;; use this --> (use-modules (guile growable-vector))  when defined with (guile growable-vector)
;; source at : https://github.com/damien-mattei/library-FunctProg
;; (include "array.scm")
;; (include "let.scm")

;; example:
;;
;; (define gva (growable-vector 1 2 3 4 5))
;;  (vector-set! gva 10 7)
;;  (describe gva)
;; #<<growable-vector> 10bd85c80> is an instance of class <growable-vector>
;; Slots are: 
;;      v = #(1 2 3 4 5 #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> 7 #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified>)
;;      end = 10
;;  {gva[7] ← 9}
;;  = 9
;;  {gva[7]}
;; = 9
;;  (vector-ref gva 7)
;;  = 9
;;  {gva[7] ← gva[3]}
;;  = 4
;;  {gva[7]}
;;  = 4
;;  {gva[37] ← gva[3]}
;;  = 4
;;  {gva[37]}
;;  = 4

;; (define gvb (growable-vector-copy gva))
;; (equal? gva gvb)
;; #f

;; scheme@(guile-user)> (growable-vector-v gva)
;; $4 = #(1 2 3 4 5)
;; scheme@(guile-user)> (growable-vector-v gvb)
;; $5 = #(1 2 3 4 5)
;; scheme@(guile-user)> (eq? (growable-vector-v gva) (growable-vector-v gvb))
;; $6 = #f
;; scheme@(guile-user)> (eqv? (growable-vector-v gva) (growable-vector-v gvb))
;; $7 = #f
;; scheme@(guile-user)> (equal? (growable-vector-v gva) (growable-vector-v gvb))
;; $8 = #t

(define-module (growable-vector) ;; (guile growable-vector) ;; (oop growable-vector)
  #:use-module (oop goops)
  #:use-module ((guile)
		#:select (vector-length vector-ref vector-set! vector->list vector-copy)
		#:prefix orig:)
  #:use-module ((srfi srfi-43) ;; vector
		#:select (vector-copy!)
		#:prefix srfi43:)
  
  #:export (<growable-vector>
	    growable-vector?
	    ;;growable-vector-get-v
	    growable-vector-v
	    make-growable-vector
	    growable-vector
	    list->growable-vector
	    growable-vector-resize)
	    
  
  #:replace (vector-length
	     vector-ref
	     vector-set!
	     vector->list
	     vector-copy
             vector-copy!)

  )


;; (use-modules (growable-vector))
;; source at : https://github.com/damien-mattei/Scheme-PLUS-for-Guile

;;; Constants

(define grow-factor 2)

;;; Capture original bindings of vector getters and setters

(define-generic vector-length)

(define-method (vector-length (v <vector>))
  (orig:vector-length v))



(define-generic vector-set!)

(define-method (vector-set! (v <vector>) (i <integer>) obj)
  (orig:vector-set! v i obj))




(define-generic vector-ref)

(define-method (vector-ref (v <vector>) (i <integer>))
  (orig:vector-ref v i))





(define-generic vector->list)

(define-method (vector->list (v <vector>))
  (orig:vector->list v))





(define-generic vector-copy)

(define-method (vector-copy (v <vector>))
  (orig:vector-copy v))

(define-method (vector-copy (v <vector>) (start <integer>))
  (orig:vector-copy v start))

(define-method (vector-copy (v <vector>) (start <integer>) (end <integer>))
  (orig:vector-copy v start end))

(define-method (vector-copy (v <vector>) (start <integer>) (end <integer>) (fill <integer>))
  (orig:vector-copy v start end fill))






(define-generic vector-copy!)

(define-method (vector-copy! (dst <vector>) (at <integer>) (src <vector>))
  (srfi43:vector-copy! dst at src))

(define-method (vector-copy! (dst <vector>) (at <integer>) (src <vector>) (start <integer>))
  (srfi43:vector-copy! dst at src start))

(define-method (vector-copy! (dst <vector>) (at <integer>) (src <vector>) (start <integer>) (end <integer>))
  (srfi43:vector-copy! dst at src start end))




;;; The <growable-vector> class

;; (make <growable-vector> #:v (vector 1 2 3 4 5))
;; #<<growable-vector> 102fcd4e0>

(define-class <growable-vector> (<vector>)
  (v  #:init-value 0 #:accessor growable-vector-v #:getter growable-vector-get-v #:setter growable-vector-set-v! #:init-keyword #:v)
  (end #:init-value 0 #:accessor growable-vector-end #:getter growable-vector-get-end #:setter growable-vector-set-end! #:init-keyword #:v-end)
  )
 ;; (length #:init-value 0 #:getter vector-length))

;; scheme@(guile-user)> (define gva (growable-vector 1 2 3 4 5))
;; scheme@(guile-user)> (vector-length gva)
;; $1 = 5
(define-method (vector-length (gv <growable-vector>))
  (orig:vector-length (growable-vector-get-v gv)))  ;; warning: size is of a growable vector (size of memory, not size used)




;; (make-growable-vector 3)
;; $1 = #<<growable-vector> 104ecfd20>
(define-method (make-growable-vector dim)
  ;;(make <growable-vector> dim) ;; with initialize
  (make <growable-vector> #:v (make-vector dim)))

;; (make-growable-vector 3 7)
(define-method (make-growable-vector dim fill)
  ;;(make <growable-vector> dim fill)
  (make <growable-vector> #:v (make-vector dim fill)))

;; (growable-vector 1 2 3 4 5)
;; $1 = #<<growable-vector> 10e5aa6c0>
(define-method (growable-vector . list_obj)
  (make <growable-vector> #:v (apply vector list_obj)))
  
  
(define-method (growable-vector? obj)
  (equal? (class-name (class-of obj)) (class-name <growable-vector>)))


;; (define (assert-size! gv i)
;;   (if (>= i (vector-length gv))
;;       *unspecified*)) ; do nothing for now



;; scheme@(guile-user)> (vector-ref (growable-vector-v gv1) 2)
;; $3 = 7
;; scheme@(guile-user)> (vector-set! (growable-vector-v gv1) 2 5)
;; scheme@(guile-user)> (vector-ref (growable-vector-v gv1) 2)
;; $4 = 5

;; scheme@(guile-user)> (define gva (growable-vector 1 2 3 4 5))
;; scheme@(guile-user)> (vector-set! gva 10 7)
;; growable-vector-resize : new-vector :#(1 2 3 4 5 #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified>)
;; scheme@(guile-user)> (describe gva)
;; #<<growable-vector> 10268d840> is an instance of class <growable-vector>
;; Slots are: 
;;      v = #(1 2 3 4 5 #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> 7 #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified> #<unspecified>)
;;      end = 10
(define-method (vector-set! (gv <growable-vector>) (i <integer>) obj)
  (if {i >= (vector-length gv)}  (if {{grow-factor * (vector-length gv)} <= i}
				     (growable-vector-resize gv {grow-factor * i})
				     (growable-vector-resize gv {grow-factor * (vector-length gv)})))
  (if {i > (growable-vector-get-end gv)}
      (growable-vector-set-end! gv i))
  (vector-set! (growable-vector-v gv) i obj))

(define-method (vector-ref (gv <growable-vector>) (i <integer>))
  (vector-ref (growable-vector-v gv) i))

;; scheme@(guile-user)> (define gva (growable-vector 1 2 3 4 5))
;; scheme@(guile-user)> (vector->list gva)
;; $1 = (1 2 3 4 5)
(define-method (vector->list (gv <growable-vector>))
  (orig:vector->list (growable-vector-v gv)))

;; scheme@(guile-user)> (describe (list->growable-vector '(a b c d e f)))
;; #<<growable-vector> 10fecb740> is an instance of class <growable-vector>
;; Slots are: 
;;      v = #(a b c d e f)
(define-method (list->growable-vector L)
  (make <growable-vector> #:v (list->vector L)))

;; scheme@(guile-user)> (define gva (growable-vector 1 2 3 4 5))
;; scheme@(guile-user)> (define gva2 (growable-vector-resize gva 8 7))
;; growable-vector-resize : new-vector :#(1 2 3 4 5 7 7 7)
;; scheme@(guile-user)> (describe gva2)
;; #<<growable-vector> 10fc25060> is an instance of class <growable-vector>
;; Slots are: 
;;      v = #(1 2 3 4 5 7 7 7)
(define-method (growable-vector-resize (gv <growable-vector>) (new-size <integer>) fill)
  (define actual-size (vector-length gv))
  (define old-vector (growable-vector-v gv))
  (define new-vector (vector-copy old-vector 0 new-size fill))
  ;;(display "growable-vector-resize : new-vector :") (display new-vector) (newline)
  (growable-vector-set-v! gv new-vector))

;; scheme@(guile-user)> (define gva (growable-vector 1 2 3 4 5))
;; scheme@(guile-user)> (define gva2 (growable-vector-resize gva 8))
;; growable-vector-resize : new-vector :#(1 2 3 4 5 #<unspecified> #<unspecified> #<unspecified>)
;; scheme@(guile-user)> (describe gva2)
;; #<<growable-vector> 10394d640> is an instance of class <growable-vector>
;; Slots are: 
;;      v = #(1 2 3 4 5 #<unspecified> #<unspecified> #<unspecified>)
(define-method (growable-vector-resize (gv <growable-vector>) (new-size <integer>)) ;; fill unspecified
  (define actual-size (vector-length gv))
  (define old-vector (growable-vector-v gv))
  (define new-vector (vector-copy old-vector 0 new-size))
  ;;(display "growable-vector-resize : new-vector :") (display new-vector) (newline)
  (growable-vector-set-v! gv new-vector))





(define-method (vector-copy (gv <growable-vector>))
  (make <growable-vector> #:v (orig:vector-copy (growable-vector-v gv))))


(define-method (vector-copy (gv <growable-vector>) (start <integer>))
  (make <growable-vector>
    #:v
    (orig:vector-copy (growable-vector-v gv))
    start))


(define-method (vector-copy (gv <growable-vector>) (start <integer>) (end <integer>))
  (make <growable-vector>
    #:v
    (orig:vector-copy (growable-vector-v gv))
    start
    end))





(define-method (vector-copy! (dst <vector>) (at <integer>) (src <growable-vector>))
  (srfi43:vector-copy! dst at (growable-vector-v src)))


(define-method (vector-copy! (dst <vector>) (at <integer>) (src <growable-vector>) (start <integer>))
  (srfi43:vector-copy! dst at (growable-vector-v src) start))


(define-method (vector-copy! (dst <vector>) (at <integer>) (src <growable-vector>) (start <integer>) (end <integer>))
  (srfi43:vector-copy! dst at (growable-vector-v src) start end))






(define-method (vector-copy! (dst <growable-vector>) (at <integer>) (src <growable-vector>))
  (srfi43:vector-copy! (growable-vector-v dst) at (growable-vector-v src)))


(define-method (vector-copy! (dst <growable-vector>) (at <integer>) (src <growable-vector>) (start <integer>))
  (srfi43:vector-copy! (growable-vector-v dst) at (growable-vector-v src) start))


(define-method (vector-copy! (dst <growable-vector>) (at <integer>) (src <growable-vector>) (start <integer>) (end <integer>))
  (srfi43:vector-copy! (growable-vector-v dst) at (growable-vector-v src) start end))





(define-method (vector-copy! (dst <growable-vector>) (at <integer>) (src <vector>))
  (srfi43:vector-copy! (growable-vector-v dst) at src))


(define-method (vector-copy! (dst <growable-vector>) (at <integer>) (src <vector>) (start <integer>))
  (srfi43:vector-copy! (growable-vector-v dst) at src start))


(define-method (vector-copy! (dst <growable-vector>) (at <integer>) (src <vector>) (start <integer>) (end <integer>))
  (srfi43:vector-copy! (growable-vector-v dst) at src start end))
