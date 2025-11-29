; This file is part of Scheme+

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

; version 17

(define-module (Scheme+ return)

  #:export (return return-rec))

(define-syntax-parameter return
  (lambda (stx)
    (syntax-violation 'return "used outside of a def" stx)))

(define-syntax-parameter return-rec
  (lambda (stx)
    (syntax-violation 'return-rec "used outside of a def" stx)))

