;; assignment version Guile (support my growable vectors)

;; This file is part of Scheme+

;; Copyright 2021 Damien MATTEI

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.




;; scheme@(guile-user)> {a[2 4] <- 7}
;; $1 = 7

;; scheme@(guile-user)> {a[2 4]}
;; $1 = 999
;; scheme@(guile-user)> {a[2 4] <- 7}
;; $2 = 7
;; scheme@(guile-user)> {a[2 4]}
;; $3 = 7
;; scheme@(guile-user)> {1 -> a[2 4]}
;; $4 = 1
;; scheme@(guile-user)> {a[2 4]}
;; $5 = 1
;; {x <- 2}
;;
;; (define T (make-vector 5))
;; scheme@(guile-user)> {T[3] <- 7}
;; <- : vector or array set!
;; $1 = 7
;;
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $3 = 7
;;
;; scheme@(guile-user)> {T[2] <- 4}
;; <- : vector or array set!
;; $2 = 4

;; scheme@(guile-user)> {T[3] <- T[2]}
;; $bracket-apply$
;; <- : vector or array set!
;; $4 = 4
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $5 = 4
(define-syntax <-
  (syntax-rules ()
    ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    
    ;; one dimension array, example: {a[4] <- 7}
    ;; $bracket-apply$ of SRFI 105
    ((_ ($bracket-apply$ array index) expr)
     (let ((tmp expr)) ;; to avoid compute it twice
						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
						 
       ;; normal case
       ;; {T[2] <- 4}
       ;; {T[3] <- T[2]}
       ;;(begin
	 ;;(display "<- : vector or array set!") (newline)
	 (if {(vector? array) or (growable-vector? array)}
	     (vector-set! array index tmp)
	     (array-set! array tmp index));)
       
       ;; rare case  (to prevent any error)
       ;; (let ((var (funct-or-macro array index))) ;; MUST be in a variable , otherwise:
       ;; While compiling expression:
       ;;  Syntax error:
       ;;  unknown location: quote: bad syntax in form quote
       ;; 	<- : variable set! after creation
       ;;  (set! var tmp)))
					     
	 tmp))


    ;; multi dimensions array :  {a[2 4] <- 7}
    ;; $bracket-apply$ of SRFI 105
    ((_ ($bracket-apply$ array index ...) expr)
     (let ((tmp expr)) ;; to avoid compute it twice
  						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
       ;; normal case
       ;;(begin
	 ;;(display "<- : multidimensional vector or array set!") (newline)
	 (array-set! array tmp index ...);)
						     
	 ;; rare case (to prevent any error)
	 ;; (let ((var (funct-or-macro array index ...))) ;; MUST be in a variable
	 ;;   (display "<- : variable set! after creation (multidimensional)") (newline)
	 ;;   (set! var tmp)))
	 tmp))

    ;; compact form but will display a warning: possibly wrong number of arguments to `vector-set!'
    ;; and if i remove ellipsis it is a severe error
    ;; ((_ (funct-or-macro array index ...) expr) (let ((tmp expr)
    ;; 						     (var (funct-or-macro array index ...)))
    ;; 						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    ;; 						     ;; normal case
    ;; 						     (if {(vector? array) or (growable-vector? array)}
    ;; 							 (vector-set! array index ... tmp)
    ;; 							 (array-set! array tmp index ...))
						     
    ;; 						     ;; rare case (to prevent any error)
    ;; 						     (set! var tmp))
					     
    ;; 						 tmp))


    ;; not sure this case will be usefull
    ((_ (funct-or-macro arg ...) expr)
     (let ((var (funct-or-macro arg ...))
	   (tmp expr)) ;; to avoid compute it twice
       (set! var tmp)
       var))

    
    ;;(<- x 5)
    ((_ var expr)
     
     (begin
       ;;(display "<- : variable set!") (newline)
       (set! var expr)
       var))))







(define-syntax ->
  (syntax-rules ()
    ;;  special form like : (-> ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ;; changé en (<- expr (funct-or-macro array index))
    ;;((_ expr (funct-or-macro array index)) {array[index] <- expr}  )
    ;; ((_ expr (funct-or-macro array index)) (<- (funct-or-macro array index) expr))
    
    ;; ;;((_ expr (funct-or-macro array index ...)) {array[index ...] <- expr} )
    ;; ((_ expr (funct-or-macro array index ...)) (<- (funct-or-macro array index ...) expr))
    
    ;; (-> 5 x)
    ;; note: it is short and infix but seems to work in all case indeed!
    ((_ expr var) {var <- expr})))








