;; increment variable
;; this is different than add1 in DrRacket

; This file is part of Scheme+

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



(define-module (increment)

  #:export (incf
	    inc!
	    add1))


(define-syntax incf
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1))
		    x))))

(define-syntax inc!
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1))
		    x))))

(define-syntax add1
  (syntax-rules ()
    ((_ x)   (+ x 1))))
