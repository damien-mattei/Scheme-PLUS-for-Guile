;; Translated from Scheme+ for Racket (atom.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2024-2025 Damien MATTEI

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


(define-module (Scheme+ atom)
  #:use-module (Scheme+ racket-compat))

  

  (define (atom x) ; near Lisp definition (allow '() but do not allow pairs that are not lists
    (not (pair? x)))

  (define (atom? x) ; as atom in Lisp but allow pairs that are not lists but do not allow '()
    (and (not (list? x))
	 (not (null? x))))

(export
	atom?
	atom)

