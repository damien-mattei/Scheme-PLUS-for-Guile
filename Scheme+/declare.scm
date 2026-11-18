;; Translated from Scheme+ for Racket (declare.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2021 Damien MATTEI

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


;; code from r6rs version

(define-module (Scheme+ declare)
  #:use-module (Scheme+ racket-compat))

  

  ;; (declare ls dyn) ;; declare multiple variables

  (define-syntax declare
    (syntax-rules ()
      ((_ var1 ...) (begin
		      (define var1 '())
		      ...))))


  

(export
	declare)
 ; end library
