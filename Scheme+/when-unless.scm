;; Translated from Scheme+ for Racket (when-unless.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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


(define-module (Scheme+ when-unless)
  #:use-module (Scheme+ racket-compat))

  




;; definitions redefined here only to allow 'define in body as allowed in Scheme+

;; implémenté de base en Racket
;; (define-syntax when
;;   (syntax-rules ()
;;     ;;((when test result1 result2 ...)
;;     ((when test result1  ...)
;;      (if test
;;          ;;(begin result1 result2 ...)))))
;; 	 ;;(let () result1 result2 ...)))))
;; 	 (let () result1 ...)))))

;; deprecated ,seems to exist already now in Racket
(define-syntax unless
  (syntax-rules ()
    ;;((unless test result1 result2 ...)
    ((unless test result1 ...)
     (when (not test)
       	 ;;(let () result1 result2 ...)))))
	   result1 ...))))





(eval-when (expand load eval)
  (module-replace! (current-module) '(unless)))
 ; end library


