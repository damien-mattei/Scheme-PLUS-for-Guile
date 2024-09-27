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



(define-module (Scheme+ insert)

  #:export ( insert
	     insert-set!
	     insert-tail
	     insert-tail-set!
	     append-tail-set!)) ;; end module declaration


;; library procedures and macro
(define insert cons)

;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))

;; insert and set it to the new result list
(define-syntax append-tail-set!
  (syntax-rules ()
    ((_ lst lst-tail)
     (set! lst (append lst lst-tail)))))

;; insert at the tail of a list 
(define (insert-tail lst elem)
  (append lst (list elem)))

;; insert at the tail of a list and set it to the new result list
(define-syntax insert-tail-set!
  (syntax-rules ()
    ((_ lst elem)
     (set! lst (insert-tail lst elem)))))




