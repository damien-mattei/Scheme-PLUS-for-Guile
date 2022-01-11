;; Scheme+.scm

;; version 1.0

;; author: Damien MATTEI

;; location: France

;; date: 2021

;; Guile Scheme version

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


;;  for curly infix notation put in your .guile:
;; (read-enable 'curly-infix)

;; use :
;; (use-modules (Scheme+))



(define-module (Scheme+)
  
  #:use-module (growable-vector)
  #:use-module (srfi srfi-69) ;; Basic hash tables 
  #:export (def $bracket-apply$ <- ← -> → <+ ⥆ +> ⥅ declare $ & condx <> ≠ **))



(include-from-path "def.scm")
(include-from-path "array.scm")
(include-from-path "apply-square-brackets.scm")
(include-from-path "assignment.scm")
(include-from-path "declare.scm")
(include-from-path "condx.scm")
(include-from-path "block.scm")
(include-from-path "not-equal.scm")
(include-from-path "exponential.scm")
