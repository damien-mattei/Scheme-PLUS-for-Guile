;; Translated from Scheme+ for Racket (map-nested.rkt) for Guile.

;; map on nested lists

;; This file is part of Scheme+

;; Copyright 2025 Damien MATTEI

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



(define-module (Scheme+ map-nested)
  #:use-module (Scheme+ racket-compat))


	


  ;; (map-nested sin '(0.1 0.2 (0.3) 0.4))
  ;; '(0.09983341664682815 0.19866933079506122 (0.29552020666133955) 0.3894183423086505)

  (define (map-nested f L)

    (if (list? L)
      (map (lambda (x) (map-nested f x))
	   L)
      (f L)))

  (define (map-nested-pair f s)

    (cond ((null? s) s)
	  ((pair? s) (cons (map-nested-pair f (car s))
			   (map-nested-pair f (cdr s))))
	  (else (f s))))      



(export
	map-nested
	map-nested-pair)
 
