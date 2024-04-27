;; Scheme+.scm

;; version 8.7

;; author: Damien MATTEI

;; location: France

;; Guile Scheme version

;; Copyright 2021-2024 Damien MATTEI

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


;; example of use with all infixes optimizations:

;./curly-infix2prefix4guile.scm    --infix-optimize --infix-optimize-slice ../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile+.scm > ../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile-optim-infix-slice.scm



(define-module (Scheme+)
  #:use-module (guile)
  #:use-module ((guile) #:select ((do . do-scheme)
				  (while . while-guile)))
  #:use-module (for_next_step)
  #:use-module (growable-vector)
  ;;#:use-module (ice-9 local-eval)
  #:use-module (infix-operators)
  #:use-module (overload)
  #:use-module (array)
  #:use-module (ice-9 match)
  #:use-module (ice-9 arrays) ;; for array-copy
  #:use-module (srfi srfi-1) ;; any,every
  #:use-module (srfi srfi-69) ;; Basic hash tables
  #:use-module (srfi srfi-31) ;; rec
  ;;#:use-module (srfi srfi-26) ;; cut <>

  ;;#:use-module (srfi srfi-43) ;; WARNING: (Scheme+): `vector-copy' imported from both (growable-vector) and (srfi srfi-43)
 
  ;; use with scheme-infix-define-macro.scm (ok)
  ;;#:export (infix-with-precedence2prefix ! quote-all overload overload-procedure overload-operator overload-function $nfx$ def $bracket-apply$ <- ← -> → <+ ⥆ +> ⥅ declare $ $>  condx ≠ ** <v v> ⇜ ⇝ repeat % << >> & | ) ;; <>

 
  ;; use only with scheme-infix-define-macro.scm enabled
  ;;#:re-export (local-eval the-environment)

  #:re-export ( for
		for-basic
		for-next
		for-basic/break
		for-basic/break-cont
		for/break-cont
		for-each-in
		in-range
		reversed
		
		define-overload-procedure
		overload-procedure
		
		define-overload-existing-procedure
		overload-existing-procedure
		
		define-overload-operator
		overload-operator
		
		define-overload-existing-operator
		overload-existing-operator
		
		define-overload-n-arity-operator
		overload-n-arity-operator

		define-overload-existing-n-arity-operator
		overload-existing-n-arity-operator
		
		overload-function ;; see how to do the same for operator, see the possible problem with infix precedence?


		;;$ovrld-square-brackets-lst$
		
		overload-square-brackets
		;;find-getter-and-setter-for-overloaded-square-brackets
		find-getter-for-overloaded-square-brackets
		find-setter-for-overloaded-square-brackets

		infix-operators-lst
		set-infix-operators-lst!
		replace-operator! ) 

  #:replace (do when unless while)

  #:export ( $nfx$
	     !*prec

	     def
	     $bracket-apply$
	     $bracket-apply$next

	     parse-square-brackets-arguments ; exported for debug
	     
	     <- ← :=
	     -> →
	     <+ ⥆ :+
	     +> ⥅
	     declare
	     $> $+>
	     condx
	     ≠
	     <> ;; is also used as keyword in srfi 26, comment this line if SRFI 26 is used and use the ≠ symbol in your code
	     **
	     <v v>
	     ⇜ ⇝
	     repeat
	     %
	     << >>
	     &
	     : ;;$
	     ∣ ) 

) ;; end module definitions



(include-from-path "def.scm")

;; must know 'for' before use unless that scheme will suppose a procedural call instead of a macro expansion
;; and will issue definition in expression context error

(include-from-path "set-values-plus.scm")

(include-from-path "declare.scm")
(include-from-path "condx.scm")
(include-from-path "block.scm")
(include-from-path "not-equal.scm")
(include-from-path "exponential.scm")
(include-from-path "when-unless.scm")
(include-from-path "while-do.scm")
(include-from-path "repeat-until.scm")
(include-from-path "modulo.scm")
(include-from-path "bitwise.scm")

(include-from-path "scheme-infix.scm")

;;(include-from-path "scheme-infix-define-macro.scm")

(include-from-path "slice.scm")

(include-from-path "assignment.scm")
(include-from-path "apply-square-brackets.scm")



