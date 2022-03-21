;;; Copyright © 2022 Maxime Devos
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;; modified by Damien Mattei

;; this version can set values for arrays,hash tables,etc
;; it uses the Scheme+ assignment operator: <-
(define-syntax set!-values-plus
  (syntax-rules ()
    ((_ (var var* ...) exp)
     (call-with-values
       (lambda () exp)
       (lambda (value . rest)
         (<- var value) ;; instead of set! i use the Scheme+ assignment operator
         (set!-values-plus (var* ...) (apply values rest)))))
    ((_ () exp)
     (call-with-values
       (lambda () exp)
       (lambda () (values)))))) ; nothing to do!

;; (define x)
;; (define y)
;; (define z)
;; (set!-values (x y z) (values 0 1 2))
;; (pk x y z)
