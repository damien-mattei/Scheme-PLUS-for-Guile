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


(define-module (Scheme+ bitwise)

  #:export (<<
	    >>
	    &
	    ∣
	    ^
	    ~))



(define (<< x n)
  (ash x n))

(define (>> x n)
  (ash x (- n)))

;; definitions based on SRFI 60 ,different than R6RS ! (not available in guile 3.0)
(define & logand) ; esuivalent of bitwise-and in R6RS
(define ∣ logior)
(define ^ logxor)
(define ~ lognot)

;;(define | logior)  ;; this pipe is weird

