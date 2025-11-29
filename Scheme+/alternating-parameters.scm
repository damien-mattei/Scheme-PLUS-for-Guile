;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

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

;; version 17

(define-module (Scheme+ alternating-parameters)

  #:export ( alternating-parameters )

  ) ;; end module declaration

;; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
;; (alternating-parameters '(< 3 < y <= z))
;; '(< < <=)
(define (alternating-parameters lyst)
  (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))

