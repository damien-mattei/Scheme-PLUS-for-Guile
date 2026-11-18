;; Translated from Scheme+ for Racket (disjunction.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2023-2025 Damien MATTEI

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


(define-module (Scheme+ disjunction)
  #:use-module (Scheme+ racket-compat))


	  ; this symbol is probably composed by other than | which is a reserved char in Racket, do not trust the apparence !

	(define (∣∣ f . r) ; there exist many vertical pipe in the charset !
	     
	   (cond ((null? r) f)
		 (f #t)
		 (else
		  (apply ∣∣ r))))



(export
	∣∣)
 ;  end library
