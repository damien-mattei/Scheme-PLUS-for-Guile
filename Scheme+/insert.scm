;; Translated from Scheme+ for Racket (insert.rkt) for Guile.

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
  #:use-module (Scheme+ racket-compat))

  
  

  (define insert cons)

  ;; insert and set it to the new result list
  (define-syntax insert-set!
    (syntax-rules ()
      ((_ elem lst)
       (set! lst (insert elem lst)))))

  ;; insert and set it to the new result list
  (define-syntax append-tail-set!
    (syntax-rules ()
      ((_ lst lst-tail)
       (set! lst (append lst lst-tail)))))

  ;; insert at the tail of a list 
  (define (insert-tail lst elem)
    (append lst (list elem)))



  
  ;; insert at the tail of a list and set it to the new result list
  ;; (define L '())

  ;; (insert-tail-set! L 'A)

  ;; L
  ;; '(A)

  ;; (insert-tail-set! L 'B)

  ;; L
  ;; '(A B)

  (define-syntax insert-tail-set!
    (syntax-rules ()
      ((_ lst elem)
       (set! lst (insert-tail lst elem)))))

  



(export
	insert
	insert-set!
	insert-tail
	insert-tail-set!
	append-tail-set!)
 ;; end module declaration


