;; Translated from Scheme+ for Racket (Scheme+.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2023-2025 Damien MATTEI

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

;; version 20.4

;; use: (use-modules (Scheme+))

;; in case of rare bug (only when modifying the language itself, so not for user but developers of Scheme+) like 'undefined something'
;; which is defined and provided remove all the 'compiled' directories from Scheme+ and restart Racket
;; this is not a Scheme+ bug but a Racket bug !


(define-module (Scheme+)
  #:use-module ((Scheme+ racket-compat) #:hide (do))) ; the do of Scheme+ (while-do) is exported

   ; the types nonzero-fixnum? and positive-fixnum? , reserved names


  

  (use-modules
	(Scheme+ def)
	(Scheme+ defvar)
	(Scheme+ def+)
	(Scheme+ def-nfx)
	(Scheme+ declare)
	(Scheme+ block)
	(Scheme+ return)
	(Scheme+ lambda-x)
	(Scheme+ lambda+)
	(Scheme+ defun)
	(Scheme+ rec+)
	(Scheme+ slice)
	(Scheme+ bracket-apply)
	(Scheme+ assignment)
	(Scheme+ nfx)
	(Scheme+ if-then-else)
	(Scheme+ for_next_step)
	(Scheme+ condx)
	(Scheme+ while-do)
	(Scheme+ repeat-until)
	(Scheme+ not-equal)
	(Scheme+ exponentiation)
	(Scheme+ bitwise)
	(Scheme+ modulo)
	(Scheme+ multiply)
	(Scheme+ little-greater-or-equal)
	(Scheme+ conjunction)
	(Scheme+ disjunction)
	(Scheme+ square-and-cubic-root)
	(Scheme+ int)
	(Scheme+ overload)
	(Scheme+ infix-with-precedence-to-prefix)
	(Scheme+ operators-list)
	(Scheme+ factorial)
	((Scheme+ parser-declarations) #:select (declare-postfix-operator declare-prefix-operator))
	(Scheme+ type)
	(Scheme+ flonum-fixnum)
	((Scheme+ multiply) #:select ((· . orig-·) (× . orig-×)))
	((Scheme+ exponentiation) #:select ((** . orig-**)))
	((Scheme+ not-equal) #:select ((≠ . orig-≠) (<> . orig-<>)))
	((guile) #:select ((* . orig-*) (- . orig--) (+ . orig-+) (/ . orig-/) (expt . orig-expt) (< . orig-<) (> . orig->) (<= . orig-<=) (>= . orig->=) (= . orig-=)))) ; end import



  ;; Guile : the version of Scheme+ (in Racket it is read in info.rkt)
  (define version "20.4")

  (define verbose #f) ; otherwise make garbage in curly-infix2prefix4racket
  
  (when verbose
    (newline)
    (display (string-append "Scheme+ v" version " by Damien Mattei.")) (newline)
    (newline))


  

(re-export
	def
	def+
	return
	return-rec
	define+
	lambda-x
	lambda+
	defun
	rec+
	<+
	+>
	⥆
	⥅
	:+
	+:
	declare
	$>
	$+>
	begin-def
	:
	$nfx$
	$bracket-apply$
	$bracket-apply-vector$
	$bracket-apply-string$
	$bracket-apply-vector-natural$
	$bracket-apply-vector-integer$
	$bracket-apply-string-natural$
	$bracket-apply-string-integer$
	$bracket-apply-hash-table$
	$bracket-apply-array$
	$bracket-apply-overloaded$
	<-
	->
	<-vector-vector*
	<-string-string*
	←
	→
	:=
	=:
	:=>
	<v
	v>
	⇜
	⇝
	for
	continue
	break
	for-basic
	for-next
	for-basic/break
	for-basic/break-cont
	for/break-cont
	for-each-in
	condx
	repeat
	<>
	≠
	fx≠
	fl≠
	≤
	≥
	**
	%
	<<
	>>
	&
	∣
	^
	·
	×
	~
	√
	∛
	&&
	∣∣
	int
	!
	overload-procedure
	require-overload-procedure
	define-overload-operator
	overload-operator
	define-overload-existing-operator
	define-overload-existing-operator-annot
	require-overload-operator
	overload-unary-operator
	require-overload-unary-operator
	define-overloaded-operator
	overload-existing-operator
	define-overload-n-ary-associative-operator
	overload-n-ary-operator
	define-overload-existing-n-ary-associative-operator
	define-overload-existing-n-ary-associative-operator-annot
	overload-existing-n-ary-operator
	overload-square-brackets
	require-overload-square-brackets
	find-getter-for-overloaded-square-brackets
	find-setter-for-overloaded-square-brackets
	!*prec-generic-infix-parser-runtime
	infix-operators-lst-for-parser
	declare-postfix-operator
	declare-prefix-operator
	type
	orig-*
	orig--
	orig-·
	orig-×
	orig-/
	orig-**
	orig-expt
	orig-+
	orig-%
	orig-<
	orig->
	orig-<=
	orig->=
	orig-=
	orig-≠
	orig-<>
	flabs
	flmin
	flmax
	flsin
	flcos
	fltan
	flatan
	flexp
	flsqrt
	fllog
	flexpt
	flfloor
	flceiling
	flround
	fltruncate
	fl+
	fl-
	fl*
	fl/
	fl<
	fl>
	fl<=
	fl>=
	fl=
	fx+
	fx-
	fx*
	fxquotient
	fxmodulo
	fxnot
	fxremainder
	fxabs
	fxmin
	fxmax
	fx->fl
	fx<
	fx>
	fx<=
	fx>=
	fx=
	fl+/fx
	fx/fl+
	fl-/fx
	fx/fl-
	fl*/fx
	fx/fl*
	fl//fx
	fx/fl/
	flmin/fx
	fx/flmin
	fl</fx
	fx/fl<
	fl>/fx
	fx/fl>
	fl<=/fx
	fx/fl<=
	fl>=/fx
	fx/fl>=
	fl=/fx
	fx/fl=
	fl≠/fx
	fx/fl≠
	ANNOTATION-ON
	ANNOTATION-OFF
	:print-type
	ℕ
	ℤ
	ℝ
	ℚ
	ℂ
	ℝ⁺
	ℤ*
	ℕ*)
;; Guile : the names of the previous versions of Scheme+ for Guile , kept for the programs written for them
(use-modules ((Scheme+ range) #:select (in-range reversed))
	     ((Scheme+ racket-compat) #:select (rest)))
(define || ∣∣) ; the procedure or (∣∣ is U+2223 , | is reserved in Racket)
(export ||)
(re-export in-range reversed rest
	   define-overload-procedure define-overload-existing-procedure overload-existing-procedure overload-function
	   define-overload-n-arity-operator define-overload-existing-n-arity-operator
	   overload-n-arity-operator overload-existing-n-arity-operator)
;; when and unless with definitions allowed in the body : the ones of (Scheme+ racket-compat)
(eval-when (expand load eval)
  (module-re-export! (current-module) '(when unless) #:replace? #t))

(eval-when (expand load eval)
  (module-re-export! (current-module) '(define if do while) #:replace? #t))
 ; end library


;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript)

;; Using the text editor vim, one can produce subscripted and superscripted numbers by using the digraphs control-k-ns for subscription and control-k-nS for superscription, where n is an Arabic numeral.


