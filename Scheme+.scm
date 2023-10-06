;; Scheme+.scm

;; version 7.1

;; author: Damien MATTEI

;; location: France

;; Guile Scheme version

;; Copyright 2021-2023 Damien MATTEI

;; e-mail: damien.mattei@gmail.com

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

;; install linux:
;; sudo cp *.scm /usr/share/guile/site/3.0


(use-modules (growable-vector)
	     (overload)
  ;;#:use-module (ice-9 local-eval)
	     (ice-9 match)
	     (ice-9 arrays) ;; for array-copy
	     (srfi srfi-1) ;; any,every
	     (srfi srfi-69) ;; Basic hash tables
	     (srfi srfi-31) ;; rec
	     (srfi srfi-26) ;; cut

  ;;#:use-module (srfi srfi-43) ;; WARNING: (Scheme+): `vector-copy' imported from both (growable-vector) and (srfi srfi-43)
  )
  ;; use with scheme-infix.scm included from module (caveit with overloading)
  ;; or with scheme-infix.scm included in main not module (ok)
  



(include "def.scm")
;;(include "array.scm") ;; MUST be included after assignment .....
(include "set-values-plus.scm")

(include "for_next_step.scm") ;; for apply-square-brackets.scm, assignment.scm
;; if you want it at toplevel: (include "for_next_step.scm") or add some export in this file....


(include "declare.scm")
(include "condx.scm")
(include "block.scm")
(include "not-equal.scm")
(include "exponential.scm")
(include "while-do-when-unless.scm")
(include "repeat-until.scm")
(include "modulo.scm")
(include "bitwise.scm")



;; must be included from program file now ! (use "scheme-infix.scm" in included-files of scheme+ directory)



;;(include "scheme-infix-define-macro.scm")




(include "slice.scm")





