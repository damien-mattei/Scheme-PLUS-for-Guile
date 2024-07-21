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



(define-module (syntax)

  #:use-module (srfi srfi-1) ;; any,every

  #:export ( ;;check-syntax=?
	    datum=?
	    member-syntax
	    ;;member-normal&syntax
	    )

  ) ;; end module declaration




;; syntax? is in (system syntax)

;; syntax? is enough for the operators like #'* #'+ etc that i  need to test (even if some complex syntax object are returned #f) :
;; scheme@(guile-user)> (use-modules (system syntax))
;; scheme@(guile-user)> syntax?
;; $1 = #<procedure syntax? (_)>
;; scheme@(guile-user)> (syntax? #'(2 * 3))
;; $2 = #f



;; scheme@(guile-user)> (syntax? #'(1 . 3))
;; $30 = #t

;; scheme@(guile-user)> (syntax? #'(1 2 3))
;; $28 = #t

;; scheme@(guile-user)> (syntax? #'*)
;; $23 = #t
;; scheme@(guile-user)> (syntax? 3)
;; $24 = #f
;; scheme@(guile-user)> (syntax? "")
;; $25 = #f

;; DEPRECATED

;; (define (syntax? obj)
  
;;   (cond ; a syntax object is:
;;    ((pair? obj) ; a pair of syntax objects, 
;;     (and (syntax? (car obj))
;; 	 (syntax? (cdr obj))))
;;    ((list? obj)
;; 	 (every syntax? obj))
;;    ((vector? obj) ; a vector of syntax objects
;;     (every syntax? (vector->list obj)))
;;    ((string? obj) ; as i will use the string representation of object 
;;     #f)
;;    ;; parse the representation of object to search for #<syntax something>
;;    (else (let* ((str-obj (format #f "~s" obj))
;; 		(lgt-str-obj (string-length str-obj))
;; 		(str-syntax "#<syntax")
;; 		(lgt-str-syntax (string-length str-syntax)))
;; 	   (and (> lgt-str-obj lgt-str-syntax) ; first, length greater
;; 		(string=? (substring str-obj 0 lgt-str-syntax)
;; 			  str-syntax) ; begin by "#<syntax"
;; 		(char=? #\> (string-ref str-obj (- lgt-str-obj 1)))))))) ; last char is >



;; DEPRECATED
;; (define (syntax-string=? obj1 obj2)
;;   (define str-obj1 (format #f "~s" obj1))
;;   (define str-obj2 (format #f "~s" obj2))
;;   (define lst-split-str-obj1 (string-split str-obj1 #\space))
;;   (define lst-split-str-obj2 (string-split str-obj2 #\space))
;;   (string=? (car (reverse lst-split-str-obj1)) (car (reverse lst-split-str-obj2))))




;; ;; scheme@(guile-user)> (check-syntax=? #'+ #'+)
;; ;; $8 = #t
;; ;; scheme@(guile-user)> (check-syntax=? #'+ +)
;; ;; $9 = #f
;; ;; scheme@(guile-user)> (check-syntax=? #'+ '+)
;; ;; $10 = #f
;; (define (check-syntax=? obj1 obj2)
;;   ;; (display "check-syntax=? : obj1 =") (display obj1) (newline)
;;   ;; (display "check-syntax=? : obj2 =") (display obj2) (newline)
;;   (define rv
;;     (and (identifier? obj1) ;(syntax? obj1)
;; 	 (identifier? obj2) ;(syntax? obj2)
;; 	 (or (free-identifier=? obj1 obj2) ; a procedure can be overloaded
;; 	     (syntax-string=? obj1 obj2)))) ; this solve the overloaded problem
;;   ;;(display "check-syntax=? : rv =") (display rv) (newline)(newline)
;;   rv)


(define (datum=? obj1 obj2)
  (eq? (syntax->datum obj1)
       (syntax->datum obj2)))





;; scheme@(guile-user)> (define op-lst (list #'* #'+ #'- #'/))
;; scheme@(guile-user)> (any (lambda (y) (check-syntax=? #'+ y)) op-lst)
;; $21 = #t
;; scheme@(guile-user)> (member-syntax #'+ op-lst)
;; member-syntax : x=#<syntax:unknown file:49:17 +>
;; member-syntax : y=#<syntax:unknown file:47:23 *>

;; member-syntax : x=#<syntax:unknown file:49:17 +>
;; member-syntax : y=#<syntax:unknown file:47:27 +>

;; $22 = #t

;; scheme@(guile-user)> (import (syntax))
;; scheme@(guile-user)> (member-syntax #'+ (list - + / *))
;; $1 = #f


(define (member-syntax x lst)
  (any (lambda (y)
	 ;;(display "member-syntax : x=") (display x) (newline)
	 ;;(display "member-syntax : y=") (display y) (newline)
	 ;;(newline)
	 ;;(check-syntax=? x y))
	 (datum=? x y))
       lst))



;; scheme@(guile-user)> (member-normal&syntax '* '(+ * -))
;; $27 = (* -)
;; scheme@(guile-user)> (member-normal&syntax #'+ op-lst)
;; member-syntax : x=#<syntax:unknown file:66:24 +>
;; member-syntax : y=#<syntax:unknown file:47:23 *>

;; member-syntax : x=#<syntax:unknown file:66:24 +>
;; member-syntax : y=#<syntax:unknown file:47:27 +>

;; $28 = #t

;; (define (member-normal&syntax x lst)
;;   (or (member x lst)
;;       (member-syntax x lst)))
