;; Growable Vectors (1 dimension)

;; Copyright 2021 Damien MATTEI

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;; use:
;; (use-modules (guile/growable-vector))
;; use this --> (use-modules (guile growable-vector))
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

(define-module (growable-vector) ;; (guile growable-vector) ;; (oop growable-vector)
  #:use-module (oop goops)
  #:use-module ((guile)
		#:select (vector-length vector-ref vector-set! vector->list)
		#:prefix orig:)
  #:use-module (srfi srfi-43) ;; vector
  #:export (<growable-vector>
	    growable-vector?
	    ;;growable-vector-get-v
	    growable-vector-v
	    make-growable-vector
	    growable-vector
	    list->growable-vector
	    growable-vector-resize)
	    ;;growable-vector-set-v!)
  #:replace (vector-length vector-ref vector-set! vector->list))

;; (use-modules (growable-vector))
;; source at : https://github.com/damien-mattei/library-FunctProg
;; (include "array.scm")
;; (include "let.scm")

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
  (orig:vector-length (growable-vector-get-v gv)))

;; (make <growable-vector> 7)
;; (make <growable-vector> 3 7)

;; (define-method (initialize (self <growable-vector>) listinitargs)
;;   (display "growable-vector :: initialize") (newline)
;;   (let-arrow* [ lg   ← (length listinitargs)
;; 		   dim  ← (if {lg = 0}
;; 			   (error "Wrong number of arguments:" lg)
;; 			   (first listinitargs))
;; 		   fill ← (if {lg > 1}
;; 			   (second listinitargs)
;; 			   '()) ]
;; 	      (cond [ {lg = 1} (display "growable-vector :: initialize :: cond lg = 1") (newline)
;; 		               (next-method self (list #:v (make-vector dim)))
;; 		      ;;{(growable-vector-v self) ← (make-vector dim)}
;; 		      ]
;; 		    [ {lg = 2} (next-method self (list #:v (make-vector dim fill)))
;; 		      ;;(growable-vector-set-v! self (make-vector dim fill))
;; 		      ]
;; 		    [ else (error "Wrong number of arguments:" lg) ])
;; 	      (display "growable-vector :: initialize :: end") (newline))
;;    )

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
