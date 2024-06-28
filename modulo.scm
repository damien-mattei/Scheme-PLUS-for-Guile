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


(define-module (modulo)

  #:export (%))

;; scheme@(guile-user)> (use-modules (modulo))
;; ;;; note: auto-compilation is enabled, set GUILE_AUTO_COMPILE=0
;; ;;;       or pass the --no-auto-compile argument to disable.
;; ;;; compiling ./modulo.scm
;; ;;; compiled /Users/mattei/.cache/guile/ccache/3.0-LE-8-4.6/Users/mattei/Scheme-PLUS-for-Guile/modulo.scm.go
;; scheme@(guile-user)> {7 % 2}
;; $1 = 1
;; scheme@(guile-user)> modulo
;; $2 = #<procedure modulo (_ _)>
;; scheme@(guile-user)> %
;; $3 = #<procedure modulo (_ _)>

(define % modulo)
