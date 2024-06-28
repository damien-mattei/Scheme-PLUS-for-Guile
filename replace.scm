;; This file is part of Scheme+

;; Copyright 2021-2024 Damien MATTEI

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


(define-module (replace)

  #:export (replace))


;; > (replace '(1 (1 2 3 4) 3 4) 3 7)
;; '(1 (1 2 7 4) 7 4)
;; > (replace '() 7 3)
;; '()
;; > (replace '(1 (1 2 3 4) 3 4) 3 7)
;; '(1 (1 2 7 4) 7 4)
;; > (replace '(1 (1 2 3 4 (5 6 3) 3 4)) 3 7)
;; '(1 (1 2 7 4 (5 6 7) 7 4))
;;
;;  (replace 4 4 5) -> 5
;; warning : element to replace must not be () !!!
(define (replace L old new)
 
	(if (list? L)
	    (map
	     (lambda (lst) (replace lst old new))
	     L)
	    (if (equal? L old)
		new
		L)))
