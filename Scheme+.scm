;; Scheme+.scm

;; version 9.1.1

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

;; mkdir -p /usr/share/guile/site/3.0

;; sudo cp *.scm /usr/share/guile/site/3.0

;; install Mac OS:
;; sudo cp *.scm /usr/local/share/guile/site/3.0

; in case of problem: rm -rf .cache/guile/


;; example:

;; (define T (make-vector 7 1))
;; scheme@(guile-user)> {T[2]}
;; $bracket-apply$ : #'parsed-args=(#<syntax list> #<syntax 2>)
;; 1
;; scheme@(guile-user)> {T[2] <- 7}
;; <- : #'(index ...) = (#<syntax 2>)
;; <- : #'parsed-args=(#<syntax list> #<syntax 2>)
;; scheme@(guile-user)> {T[2]}
;; $bracket-apply$ : #'parsed-args=(#<syntax list> #<syntax 2>)
;; 7


(define-module (Scheme+)

  ;;#:use-module (guile)

  ;; #:use-module ((guile) #:select ((do . do-scheme)
  ;; 				  (while . while-guile)))
                                  ;;(if . if-scheme)))

  #:use-module (def)
  #:use-module (declare)
  #:use-module (block)
  #:use-module (not-equal)
  #:use-module (exponential)
  #:use-module (modulo)
  #:use-module (bitwise)
  #:use-module (when-unless)
  #:use-module (while-do)
  #:use-module (repeat-until)
  #:use-module (slice)
  
  #:use-module (if-then-else)
  #:use-module (for_next_step)

  #:use-module (range)
  
  #:use-module (growable-vector)
  
  ;;#:use-module (ice-9 local-eval)
  
  #:use-module (overload)
  #:use-module (array)
  
  #:use-module (condx)
  
  #:use-module (bracket-apply)
  #:use-module (assignment)
  #:use-module (nfx)
  
  ;;;;#:use-module (srfi srfi-26) ;; cut <>

  ;;;;#:use-module (srfi srfi-43) ;; WARNING: (Scheme+): `vector-copy' imported from both (growable-vector) and (srfi srfi-43)
 
  ;; use with scheme-infix-define-macro.scm (ok)
  ;;;;#:export (infix-with-precedence2prefix ! quote-all overload overload-procedure overload-operator overload-function $nfx$ def $bracket-apply$ <- ← -> → <+ ⥆ +> ⥅ declare $ $>  condx ≠ ** <v v> ⇜ ⇝ repeat % << >> & | ) ;; <>
  
  ;; use only with scheme-infix-define-macro.scm enabled
  ;;#:re-export (local-eval the-environment)


  ;; re-export because they are from modules
  #:re-export ( 
		for
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

		condx
	
		$nfx$
	  
		def
		$bracket-apply$
		;;$bracket-apply$next  ;; DONE: comment it

		;;infix-operators-lst-for-parser-syntax infix-operators-lst-for-parser ;; for debug
	     
		<- ->
		:= =:
		← →
		<+ +>
		:+ +:
		⥆ ⥅
		
		declare
		$> $+> begin-def

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
		:
		∣ 

		) 

  #:re-export-and-replace (if do when unless while)
  
  
  ;;#:replace (do when unless while)

  #:export (rest)) ;; end module definitions



(define rest cdr)


