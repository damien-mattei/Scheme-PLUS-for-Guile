;; for guile (version compatible with my growable vector class)

;; This file is part of Scheme+

;; Copyright 2021 Damien MATTEI

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.



;; SRFI 105 : Curly-infix-expressions allows a syntax like {Array[index]} with vectors
;; and arrays of any dimensions,size and shape

;; (define T (make-vector 5))
;; (vector-set! T 3 7)
;; scheme@(guile-user)> {T[3]}
;; $3 = 7
;; {T[3] <- 7}
;; 7

;; scheme@(guile-user)> (define a (make-array 999 '(1 2) '(3 4)))
;; scheme@(guile-user)> (array-ref a 2 4)
;; $3 = 999

;; scheme@(guile-user)> {a[2 4]}
;; $9 = 999

;; scheme@(guile-user)> (define b (make-array 'ho 3))
;; scheme@(guile-user)> (array-ref b 1)
;; $13 = ho

;; scheme@(guile-user)> {b[2]}
;; $15 = ho

;; scheme@(guile-user)> {a[2 4] <- 7}
;; scheme@(guile-user)> {a[2 4]}
;; $19 = 7
;; scheme@(guile-user)> {a[1 3] <- 5}
;; scheme@(guile-user)> {a[1 3] <- a[2 4]}
;; scheme@(guile-user)> {a[1 3]}
;; $20 = 7

(define-syntax $bracket-apply$
  (syntax-rules ()
    
    ((_ array index)
     ;;(begin ;;(display "$bracket-apply$") (newline)
	    (if {(vector? array) or (growable-vector? array)}
		(vector-ref array index)
		(array-ref array index)));)
    
    ((_ array index ...)
     ;(begin ;;(display "$bracket-apply$") (newline)
	    (array-ref array index ...))));) 


