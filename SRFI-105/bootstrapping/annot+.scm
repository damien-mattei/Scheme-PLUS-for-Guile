;; Translated from SRFI 105 for Racket (bootstrapping/annot+.rkt , SRFI-105-for-Racket) for Guile : the source of
;; SRFI-105/annot+.scm , written in Scheme+ (curly infix). In Guile <- and := only assign : a variable is defined
;; with <+.
;; Build , in this directory (SRFI-105/bootstrapping/) : make (the parser curly-infix2prefix4guile.scm converts this
;; file with the module installed , SRFI-105/annot+.scm) , make diff (compare with the installed module) , then
;; make install (copy in SRFI-105/annot+.scm , and remove the compiled files of Guile). After make install a new make
;; must give the same file.


;; Damien Mattei

;; This file is part of Scheme+

;; Copyright Damien MATTEI

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

; this is the annotation and inference type part of the Scheme+ curly infix (SRFI 105) parser.

#| this file needs to be parsed before as it uses bootstrapping with Scheme+
   and curly infix syntax for implementing Scheme+ type annotation and inference |#

;; entry procedure is annot


(define-module (SRFI-105 annot+)
  #:use-module (Scheme+ racket-compat))

  (use-modules
	((srfi srfi-1) #:select (first second third map-in-order any every filter take-while take last find delete-duplicates))
	(srfi srfi-69)
	(Scheme+)
	(Scheme+ operators)
	(Scheme+ type)
	((Scheme+ source-locations) #:select (link-form! link-cell! relink-form! enter-form! leave-form!))
	(SRFI-105 scheme-types)
	(SRFI-105 racket-types)
	((Scheme+ parse-square-brackets) #:select (parse-square-brackets-arguments-lister-syntax))) ; the port of the debug traces when they are off

  


  ; the debug traces of the annotation go to stderr only when the environment variable
  ; SCHEME_PLUS_ANNOT_DEBUG is set (e.g. SCHEME_PLUS_ANNOT_DEBUG=1 racket prog.rkt) , otherwise nowhere
  #;{err-port := (current-error-port)}
  {err-port <+ (if (getenv "SCHEME_PLUS_ANNOT_DEBUG") (current-error-port) (open-output-nowhere))}

  ; by default in parse-expressions:
  {err-flag-type-changed <+ #f} ; set to #t to display debug information with error halting program

  {err-flag-args-changed <+ #f} ; set to #t to display debug information with error halting program

  ; only in find-type
  {err-flag-type-changed-in-find-type <+ #t} ; set to #t to display debug information with error halting program

  {err-flag-args-changed-in-find-type <+ #f} ; set to #t to display debug information with error halting program

  ; enable/disable the recall-annot recomputation of argument types in find-type:
  ; correctness-neutral (does not change the parsed result) but costly (extra find-type pass on annotated args) -> can be disabled for performance
  {use-recall-annot-in-find-type <+ #f}

  {scheme-ht <+ (make-hash-table)} #| scheme static hash table containing scheme R*RS functions with their declaration
                                    (return type and sometimes argument types) |#

  ;; i will create entry in a new hash table for existing procedure, example: 'length
  ; this require a new hash table constructed by AI from R7RS doc but is now in a separate table scheme-type
  ; populate scheme-ht from the static R7RS type table scheme-types (see ../scheme-types.rkt)
  ; each entry of scheme-types has the form (name (return-types ...) procedure?-or-macro? (param-types ...))
  ; here, for each entry, we drop the name and store the rest, ((return-types ...) tag (param-types ...)), in scheme-ht under that name
  (define (populate-scheme-ht! ht entries)
    (for-each (λ (entry) {ht[(first entry)] := (rest entry)})
              entries))

  (populate-scheme-ht! scheme-ht scheme-types) ; load all R7RS (scheme base)/(scheme inexact)/core-syntax entries
  (populate-scheme-ht! scheme-ht racket-types) ; the procedures of Racket not in R7RS (sqr , racket/flonum , racket/fixnum ...) , see ../racket-types.rkt

  ; now in racket-types.rkt
  #;{scheme-ht['sqr] := '((real?) procedure? (real?))} ; sqr is a Racket-specific alias for square, not in R7RS, so not in scheme-types

  ; the first return type of name in scheme-ht , #f when name is not there or when its return type is any? (the
  ; pseudo-predicate of scheme-types.rkt for any value : it is a type of parameter , as a return type it says nothing
  ; and must not replace a known type , as in {k := (if ...)})
  (define (scheme-return-type name)
    (and (hash-table-exists? scheme-ht name)
         ($+> {rv <+ (caar {scheme-ht[name]})}
              (and (not (eq? rv 'any?))
                   rv))))

  ; this macro reinitialise data in procedure in case of re-entring when requiring sub module in a parent module
  ; put here to be shared between SRFI 105 and SRFI 110 for curly infix and with Scheme+
  (define-syntax init-annot-data

    (syntax-rules ()

      ((_ ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)

       (begin

         (display "annot+.rkt : init-annot-data" err-port) (newline err-port)
         (reset-$ovrld-square-brackets-lst$) ; the overloads of the square brackets known by the annotation
         {ovrld-ht := (make-hash-table)} ; this hash table will be used for overloading operators

         ; store the overload for arithmetic operators about numbers (will be improved with float? later)
         ; this create the original list that will contains all the overloads for this operator
         ; this continues with other arithmetic operators
         ; TODO what happens if mixing addition of real and integer? see in action, does it optimise something? or not !
         ; selection of type should have to be made regarding to the internal precision ,ex:  3.0 versus 3
         {ovrld-ht['+] := '([(nonnegative-flonum?) fl+ (nonnegative-flonum? nonnegative-flonum?)] ; ℝ⁺ : a sum or a product of floats not negative is not negative (before the rows of flonums)
                            [(fixnum?) fx+ (fixnum? fixnum?)]
                           [(exact-integer?) orig-+ (exact-integer? fixnum?)]
                           [(exact-integer?) orig-+ (fixnum? exact-integer?)]
                           [(exact-integer?) orig-+ (exact-integer? exact-integer?)]
                           [(integer?) orig-+ (integer? integer?)]
                           [(number?) orig-+ (number? number?)]
                           [(flonum?) fl+ (flonum? flonum?)]
                           [(flonum?) orig-+ (flonum? exact-integer?)]
                           [(flonum?) orig-+ (exact-integer? flonum?)]
                           [(real?) orig-+ (real? real?)]
                           [(rational?) orig-+ (rational? rational?)]
                           [(complex?) orig-+ (complex? complex?)]
                           [(inexact?) orig-+ (inexact? number?)]
                           [(inexact?) orig-+ (number? inexact?)]
                           [(inexact?) orig-+ (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(flonum?) fx/fl+ (fixnum? flonum?)]
                           [(flonum?) fl+/fx (flonum? fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         ; ~ (bitwise not) , unary : in the table of the overloads with an exact number of arguments
         {ovrld-ht['~] := '([(fixnum?) fxnot (fixnum?)]
                            [(exact-integer?) bitwise-not (exact-integer?)])}

         ; the numeric procedures : specialized only for the types where the result is the same , the other calls stay
         ; generic and are typed by scheme-types.rkt ; not sqrt , log , asin , acos of a flonum? : with a negative float (or
         ; outside [-1 , 1]) they return a complex number where flsqrt , fllog ... return +nan.0 ; sqrt , log , expt of a ℝ⁺
         ; (nonnegative-flonum? , a float not negative) are flsqrt , fllog , flexpt ; not sin ... of a fixnum : (sin 0) is
         ; the exact 0
         {ovrld-ht['sin] := '([(flonum?) flsin (flonum?)])}
         {ovrld-ht['cos] := '([(flonum?) flcos (flonum?)])}
         {ovrld-ht['tan] := '([(flonum?) fltan (flonum?)])}
         {ovrld-ht['atan] := '([(flonum?) flatan (flonum?)])} ; one argument only
         {ovrld-ht['exp] := '([(nonnegative-flonum?) flexp (flonum?)])} ; an exponential is not negative : ℝ⁺
         {ovrld-ht['abs] := '([(nonnegative-flonum?) flabs (flonum?)] ; ℝ⁺
                              [(fixnum?) fxabs (fixnum?)])}
         ; of a ℝ⁺ (a float not negative) : the same results as sqrt , log , expt (checked on 0.0 , -0.0 , +nan.0 , +inf.0 :
         ; (log -0.0) is -inf.0) ; √ is sqrt
         {ovrld-ht['sqrt] := '([(nonnegative-flonum?) flsqrt (nonnegative-flonum?)])}
         {ovrld-ht['√] := '([(nonnegative-flonum?) flsqrt (nonnegative-flonum?)])}
         {ovrld-ht['log] := '([(flonum?) fllog (nonnegative-flonum?)])} ; one argument only
         {ovrld-ht['min] := '([(flonum?) flmin (flonum? flonum?)]
                              [(fixnum?) fxmin (fixnum? fixnum?)]
                              ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                              ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given ((max 3 2.0) is 3.0)
                              [(flonum?) fx/flmin (fixnum? flonum?)]
                              [(flonum?) flmin/fx (flonum? fixnum?)]
                              [(flonum?) min (flonum? exact-integer?)]
                              [(flonum?) min (exact-integer? flonum?)]
                              [(exact-integer?) min (fixnum? exact-integer?)]
                              [(exact-integer?) min (exact-integer? fixnum?)]
                              [(exact-integer?) min (exact-integer? exact-integer?)])}
         ; not flmax : on 0.0 and -0.0 , Racket CS 9.2 flmax returns the first or the second argument according to the
         ; compilation (the first when an argument comes from vector-ref : (flmax 0.0 (vector-ref v i)) is 0.0 with v[i] = -0.0)
         ; , max and flmin always the second : the generic max , its return type only
         {ovrld-ht['max] := '([(flonum?) max (flonum? flonum?)]
                              [(fixnum?) fxmax (fixnum? fixnum?)]
                              ; mixed types : the generic procedure , only the return type is given ((max 3 2.0) is 3.0)
                              [(flonum?) max (fixnum? flonum?)]
                              [(flonum?) max (flonum? fixnum?)]
                              [(flonum?) max (flonum? exact-integer?)]
                              [(flonum?) max (exact-integer? flonum?)]
                              [(exact-integer?) max (fixnum? exact-integer?)]
                              [(exact-integer?) max (exact-integer? fixnum?)]
                              [(exact-integer?) max (exact-integer? exact-integer?)])}
         {ovrld-ht['quotient] := '([(fixnum?) fxquotient (fixnum? fixnum?)])}
         {ovrld-ht['remainder] := '([(fixnum?) fxremainder (fixnum? fixnum?)]
                                    ; the rest is smaller than a fixnum divisor : a fixnum
                                    [(fixnum?) remainder (exact-integer? fixnum?)])}
         {ovrld-ht['modulo] := '([(fixnum?) fxmodulo (fixnum? fixnum?)]
                                 ; the rest is smaller than a fixnum divisor : a fixnum
                                 [(fixnum?) modulo (exact-integer? fixnum?)])}
         {ovrld-ht['exact->inexact] := '([(flonum?) fx->fl (fixnum?)])}
         {ovrld-ht['inexact] := '([(flonum?) fx->fl (fixnum?)])}
         ; rounding : of a float a float (flfloor ...) , of a fixnum the same fixnum (the procedure is kept , only the type is
         ; given) ; (floor x) is typed integer? by scheme-types.rkt , too general for the fl or fx operation around it
         {ovrld-ht['floor] := '([(flonum?) flfloor (flonum?)]
                                [(fixnum?) floor (fixnum?)])}
         {ovrld-ht['ceiling] := '([(flonum?) flceiling (flonum?)]
                                  [(fixnum?) ceiling (fixnum?)])}
         {ovrld-ht['round] := '([(flonum?) flround (flonum?)] ; flround rounds to even as round : (flround 2.5) is 2.0
                                [(fixnum?) round (fixnum?)])}
         {ovrld-ht['truncate] := '([(flonum?) fltruncate (flonum?)]
                                   [(fixnum?) truncate (fixnum?)])}
         ; the square : the procedure is kept , only the type is given (as fx* , a square too big for a fixnum is an error
         ; of the next fx operation)
         {ovrld-ht['square] := '([(nonnegative-flonum?) square (flonum?)] ; ℝ⁺
                                 [(fixnum?) square (fixnum?)])}
         {ovrld-ht['sqr] := '([(nonnegative-flonum?) sqr (flonum?)] ; ℝ⁺
                              [(fixnum?) sqr (fixnum?)])}
         ; add1 and sub1 of Racket : the procedure is kept , only the type is given (as fx+ , an add1 too big for a fixnum
         ; is an error of the next fx operation)
         {ovrld-ht['add1] := '([(flonum?) add1 (flonum?)]
                               [(fixnum?) add1 (fixnum?)]
                               [(exact-integer?) add1 (exact-integer?)])}
         {ovrld-ht['sub1] := '([(flonum?) sub1 (flonum?)]
                               [(fixnum?) sub1 (fixnum?)]
                               [(exact-integer?) sub1 (exact-integer?)])}

         ; the specialized procedures themselves , for the type of an argument already rewritten : (fl* (flsin x) 2.0)
         {ovrld-ht['flfloor] := '([(flonum?) flfloor (flonum?)])}
         {ovrld-ht['flceiling] := '([(flonum?) flceiling (flonum?)])}
         {ovrld-ht['flround] := '([(flonum?) flround (flonum?)])}
         {ovrld-ht['fltruncate] := '([(flonum?) fltruncate (flonum?)])}
         {ovrld-ht['flsin] := '([(flonum?) flsin (flonum?)])}
         {ovrld-ht['flcos] := '([(flonum?) flcos (flonum?)])}
         {ovrld-ht['fltan] := '([(flonum?) fltan (flonum?)])}
         {ovrld-ht['flatan] := '([(flonum?) flatan (flonum?)])}
         {ovrld-ht['flexp] := '([(nonnegative-flonum?) flexp (flonum?)])} ; ℝ⁺
         {ovrld-ht['flabs] := '([(nonnegative-flonum?) flabs (flonum?)])} ; ℝ⁺
         {ovrld-ht['fxabs] := '([(fixnum?) fxabs (fixnum?)])}
         {ovrld-ht['flmin] := '([(flonum?) flmin (flonum? flonum?)])}
         {ovrld-ht['flmax] := '([(flonum?) flmax (flonum? flonum?)])}
         {ovrld-ht['fxmin] := '([(fixnum?) fxmin (fixnum? fixnum?)])}
         {ovrld-ht['fxmax] := '([(fixnum?) fxmax (fixnum? fixnum?)])}
         {ovrld-ht['fxquotient] := '([(fixnum?) fxquotient (fixnum? fixnum?)])}
         {ovrld-ht['fxremainder] := '([(fixnum?) fxremainder (fixnum? fixnum?)])}
         {ovrld-ht['fx->fl] := '([(flonum?) fx->fl (fixnum?)])}

         {ovrld-ht['orig-+] := '([(nonnegative-flonum?) fl+ (nonnegative-flonum? nonnegative-flonum?)]
                                 [(fixnum?) fx+ (fixnum? fixnum?)]
                           [(exact-integer?) orig-+ (exact-integer? fixnum?)]
                           [(exact-integer?) orig-+ (fixnum? exact-integer?)]
                           [(exact-integer?) orig-+ (exact-integer? exact-integer?)]
                           [(integer?) orig-+ (integer? integer?)]
                           [(number?) orig-+ (number? number?)]
                           [(flonum?) fl+ (flonum? flonum?)]
                           [(real?) orig-+ (real? real?)]
                           [(rational?) orig-+ (rational? rational?)]
                           [(complex?) orig-+ (complex? complex?)]
                           [(inexact?) orig-+ (inexact? number?)]
                           [(inexact?) orig-+ (number? inexact?)]
                           [(inexact?) orig-+ (inexact? inexact?)]
                           ; as in the table of +
                           [(flonum?) orig-+ (flonum? exact-integer?)]
                           [(flonum?) orig-+ (exact-integer? flonum?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(flonum?) fx/fl+ (fixnum? flonum?)]
                           [(flonum?) fl+/fx (flonum? fixnum?)])}

         {ovrld-ht['-] := '([(fixnum?) fx- (fixnum? fixnum?)]
                           [(exact-integer?) orig-- (exact-integer? fixnum?)]
                           [(exact-integer?) orig-- (fixnum? exact-integer?)]
                           [(exact-integer?) orig-- (exact-integer? exact-integer?)]
                           [(integer?) orig-- (integer? integer?)]
                           [(number?) orig-- (number? number?)]
                           [(flonum?) fl- (flonum? flonum?)]
                           [(real?) orig-- (real? real?)]
                           [(rational?) orig-- (rational? rational?)]
                           [(complex?) orig-- (complex? complex?)]
                           [(inexact?) orig-- (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(flonum?) fx/fl- (fixnum? flonum?)]
                           [(flonum?) fl-/fx (flonum? fixnum?)]
                           [(flonum?) orig-- (flonum? exact-integer?)]
                           [(flonum?) orig-- (exact-integer? flonum?)])} ; unary is only a special case of n-ary

         {ovrld-ht['orig--] := '([(fixnum?) fx- (fixnum? fixnum?)]
                           [(exact-integer?) orig-- (exact-integer? fixnum?)]
                           [(exact-integer?) orig-- (fixnum? exact-integer?)]
                           [(exact-integer?) orig-- (exact-integer? exact-integer?)]
                           [(integer?) orig-- (integer? integer?)]
                           [(number?) orig-- (number? number?)]
                           [(flonum?) fl- (flonum? flonum?)]
                           [(real?) orig-- (real? real?)]
                           [(rational?) orig-- (rational? rational?)]
                           [(complex?) orig-- (complex? complex?)]
                           [(inexact?) orig-- (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(flonum?) fx/fl- (fixnum? flonum?)]
                           [(flonum?) fl-/fx (flonum? fixnum?)]
                           [(flonum?) orig-- (flonum? exact-integer?)]
                           [(flonum?) orig-- (exact-integer? flonum?)])}

         {ovrld-ht['/] := '([(rational?) orig-/ (exact-integer? exact-integer?)] ; (/ 1 2) is 1/2
                            [(real?) orig-/ (integer? integer?)] ; (/ 1 2) is 1/2
                            [(number?) orig-/ (number? number?)]
                            [(flonum?) fl/ (flonum? flonum?)]
                            [(real?) orig-/ (real? real?)]
                            [(rational?) orig-/ (rational? rational?)]
                            [(complex?) orig-/ (complex? complex?)]
                            [(inexact?) orig-/ (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given (a division by an exact 0 is an error , (/ 0 2.0) is the exact 0)
                            [(flonum?) fl//fx (flonum? fixnum?)]
                            [(real?) fx/fl/ (fixnum? flonum?)] ; (/ 0 x) is the exact 0
                            [(flonum?) orig-/ (flonum? exact-integer?)])}

         {ovrld-ht['orig-/] := '([(rational?) orig-/ (exact-integer? exact-integer?)] ; (/ 1 2) is 1/2
                           [(real?) orig-/ (integer? integer?)] ; (/ 1 2) is 1/2
                           [(number?) orig-/ (number? number?)]
                           [(flonum?) fl/ (flonum? flonum?)]
                           [(real?) orig-/ (real? real?)]
                           [(rational?) orig-/ (rational? rational?)]
                           [(complex?) orig-/ (complex? complex?)]
                           [(inexact?) orig-/ (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given (a division by an exact 0 is an error , (/ 0 2.0) is the exact 0)
                           [(flonum?) fl//fx (flonum? fixnum?)]
                           [(real?) fx/fl/ (fixnum? flonum?)] ; (/ 0 x) is the exact 0
                           [(flonum?) orig-/ (flonum? exact-integer?)])}

         {ovrld-ht['%] := '([(fixnum?) fxmodulo (fixnum? fixnum?)]
                           [(exact-integer?) orig-% (exact-integer? exact-integer?)] ;not sure it is a good idea as / is in general not integer
                           [(integer?) orig-% (integer? integer?)]
                           [(number?) orig-% (number? number?)]
                           [(flonum?) orig-% (flonum? flonum?)]
                           [(real?) orig-% (real? real?)]
                           [(rational?) orig-% (rational? rational?)]
                           [(complex?) orig-% (complex? complex?)]
                           [(inexact?) orig-% (inexact? inexact?)]
                           ; the rest is smaller than a fixnum divisor : a fixnum
                           [(fixnum?) orig-% (exact-integer? fixnum?)])}

         {ovrld-ht['orig-%] := '([(fixnum?) fxmodulo (fixnum? fixnum?)]
                           [(exact-integer?) orig-% (exact-integer? exact-integer?)] ;not sure it is a good idea as / is in general not integer
                           [(integer?) orig-% (integer? integer?)]
                           [(number?) orig-% (number? number?)]
                           [(flonum?) orig-% (flonum? flonum?)]
                           [(real?) orig-% (real? real?)]
                           [(rational?) orig-% (rational? rational?)]
                           [(complex?) orig-% (complex? complex?)]
                           [(inexact?) orig-% (inexact? inexact?)]
                           ; the rest is smaller than a fixnum divisor : a fixnum
                           [(fixnum?) orig-% (exact-integer? fixnum?)])}

         ; BEGIN-STRICT-SRFI-105-REGION ; otherwise will set in prefix expressions
         {ovrld-ht['*] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                            [(fixnum?) fx* (fixnum? fixnum?)]
                           [(exact-integer?) orig-* (exact-integer? fixnum?)]
                           [(exact-integer?) orig-* (fixnum? exact-integer?)]
                           [(exact-integer?) orig-* (exact-integer? exact-integer?)]
                           [(integer?) orig-* (integer? integer?)]
                           [(number?) orig-* (number? number?)]
                           [(flonum?) fl* (flonum? flonum?)]
                           [(real?) orig-* (real? real?)]
                           [(rational?) orig-* (rational? rational?)]
                           [(complex?) orig-* (complex? complex?)]
                           [(inexact?) orig-* (inexact? inexact?)]
                           ; a flonum and a fixnum : fl*/fx , fx/fl* of Scheme+/flonum-fixnum , a guard at run time : real? ((* x 0) is the exact 0)
                           [(real?) fl*/fx (flonum? fixnum?)]
                           [(real?) fx/fl* (fixnum? flonum?)])}

         {ovrld-ht['orig-*] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                                 [(fixnum?) fx* (fixnum? fixnum?)]
                           [(exact-integer?) orig-* (exact-integer? fixnum?)]
                           [(exact-integer?) orig-* (fixnum? exact-integer?)]
                           [(exact-integer?) orig-* (exact-integer? exact-integer?)]
                           [(integer?) orig-* (integer? integer?)]
                           [(number?) orig-* (number? number?)]
                           [(flonum?) fl* (flonum? flonum?)]
                           [(real?) orig-* (real? real?)]
                           [(rational?) orig-* (rational? rational?)]
                           [(complex?) orig-* (complex? complex?)]
                           [(inexact?) orig-* (inexact? inexact?)]
                           ; a flonum and a fixnum : fl*/fx , fx/fl* of Scheme+/flonum-fixnum , a guard at run time : real? ((* x 0) is the exact 0)
                           [(real?) fl*/fx (flonum? fixnum?)]
                           [(real?) fx/fl* (fixnum? flonum?)])}

         ; END-STRICT-SRFI-105-REGION
         {ovrld-ht['·] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                            [(fixnum?) fx* (fixnum? fixnum?)]
                           [(exact-integer?) orig-· (exact-integer? fixnum?)]
                           [(exact-integer?) orig-· (fixnum? exact-integer?)]
                           [(exact-integer?) orig-· (exact-integer? exact-integer?)]
                           [(integer?) orig-· (integer? integer?)]
                           [(number?) orig-· (number? number?)]
                           [(flonum?) fl* (flonum? flonum?)]
                           [(real?) orig-· (real? real?)]
                           [(rational?) orig-· (rational? rational?)]
                           [(complex?) orig-· (complex? complex?)]
                           [(inexact?) orig-· (inexact? inexact?)]
                           ; a flonum and a fixnum : fl*/fx , fx/fl* of Scheme+/flonum-fixnum , a guard at run time : real? ((* x 0) is the exact 0)
                           [(real?) fl*/fx (flonum? fixnum?)]
                           [(real?) fx/fl* (fixnum? flonum?)])}

         {ovrld-ht['orig-·] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                                 [(fixnum?) fx* (fixnum? fixnum?)]
                           [(exact-integer?) orig-· (exact-integer? fixnum?)]
                           [(exact-integer?) orig-· (fixnum? exact-integer?)]
                           [(exact-integer?) orig-· (exact-integer? exact-integer?)]
                           [(integer?) orig-· (integer? integer?)]
                           [(number?) orig-· (number? number?)]
                           [(flonum?) fl* (flonum? flonum?)]
                           [(real?) orig-· (real? real?)]
                           [(rational?) orig-· (rational? rational?)]
                           [(complex?) orig-· (complex? complex?)]
                           [(inexact?) orig-· (inexact? inexact?)]
                           ; a flonum and a fixnum : fl*/fx , fx/fl* of Scheme+/flonum-fixnum , a guard at run time : real? ((* x 0) is the exact 0)
                           [(real?) fl*/fx (flonum? fixnum?)]
                           [(real?) fx/fl* (fixnum? flonum?)])}

         ; × is also a Scheme+ multiplication, same overloads as ·
         {ovrld-ht['×] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                            [(fixnum?) fx* (fixnum? fixnum?)]
                           [(exact-integer?) orig-× (exact-integer? fixnum?)]
                           [(exact-integer?) orig-× (fixnum? exact-integer?)]
                           [(exact-integer?) orig-× (exact-integer? exact-integer?)]
                           [(integer?) orig-× (integer? integer?)]
                           [(number?) orig-× (number? number?)]
                           [(flonum?) fl* (flonum? flonum?)]
                           [(real?) orig-× (real? real?)]
                           [(rational?) orig-× (rational? rational?)]
                           [(complex?) orig-× (complex? complex?)]
                           [(inexact?) orig-× (inexact? inexact?)]
                           ; a flonum and a fixnum : fl*/fx , fx/fl* of Scheme+/flonum-fixnum , a guard at run time : real? ((* x 0) is the exact 0)
                           [(real?) fl*/fx (flonum? fixnum?)]
                           [(real?) fx/fl* (fixnum? flonum?)])}

         {ovrld-ht['orig-×] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                                 [(fixnum?) fx* (fixnum? fixnum?)]
                           [(exact-integer?) orig-× (exact-integer? fixnum?)]
                           [(exact-integer?) orig-× (fixnum? exact-integer?)]
                           [(exact-integer?) orig-× (exact-integer? exact-integer?)]
                           [(integer?) orig-× (integer? integer?)]
                           [(number?) orig-× (number? number?)]
                           [(flonum?) fl* (flonum? flonum?)]
                           [(real?) orig-× (real? real?)]
                           [(rational?) orig-× (rational? rational?)]
                           [(complex?) orig-× (complex? complex?)]
                           [(inexact?) orig-× (inexact? inexact?)]
                           ; a flonum and a fixnum : fl*/fx , fx/fl* of Scheme+/flonum-fixnum , a guard at run time : real? ((* x 0) is the exact 0)
                           [(real?) fl*/fx (flonum? fixnum?)]
                           [(real?) fx/fl* (fixnum? flonum?)])}

         {ovrld-ht['**] := '([(flonum?) flexpt (nonnegative-flonum? flonum?)] ; a ℝ⁺ to a float power : a float , not a complex (before the rows of flonums)
                             [(flonum?) orig-** (flonum? nonzero-fixnum?)] ; a float to the power of a ℤ* is a float
                            #;[(fixnum?) orig-** (fixnum? fixnum?)]
                            [(fixnum?) orig-** (fixnum? natural-fixnum?)] ; a ℕ exponent : an integer (the overflow is accepted)
                            [(rational?) orig-** (fixnum? fixnum?)] ; (expt 2 -1) is 1/2 : declare the exponent ℕ for fx+ after
                            [(rational?) orig-** (exact-integer? exact-integer?)] ; (expt 2 -1) is 1/2
                            [(real?) orig-** (integer? integer?)] ; (expt 2 -1) is 1/2
                            [(number?) orig-** (number? number?)]
                            [(inexact?) orig-** (flonum? flonum?)] ; (expt -1.0 1.5) is a complex ; goal is also to return flonum? with orig-** but is it retrieved from here?? to avoid breaking floating point computation instead of real? scheme
                            #;[(flonum?) orig-** (flonum? fixnum?)] ; a flonum to an integer power , as {z ²} : a flonum (the exponent 0 must be written 0.0)
                            [(real?) orig-** (flonum? fixnum?)] ; (expt 2.0 0) is the exact 1 : declare the exponent ℤ* for fl+ after
                            [(number?) orig-** (real? real?)] ; (expt -1 1/2) is +i
                            [(complex?) orig-** (complex? complex?)]
                            [(inexact?) orig-** (inexact? inexact?)]
                            #;[(inexact?) orig-** (inexact? integer?)]
                            [(number?) orig-** (inexact? integer?)] ; (expt 2.0 0) is the exact 1
                            [(number?) orig-** (integer? inexact?)])}


         #|
         {two := 2}
         {three := 3}
         {four := 4}
         (type exact-integer? two three four) ; required because two ** three ** four is overflowing fixnums
         {1 + two ** three ** four}        
         |#

         ; overload with itself because if we recall annot on arguments we can get as result an orig- operator and need its return value type
         {ovrld-ht['orig-**] := '([(flonum?) flexpt (nonnegative-flonum? flonum?)] ; a ℝ⁺ to a float power : a float , not a complex (before the rows of flonums)
                                  [(flonum?) orig-** (flonum? nonzero-fixnum?)] ; a float to the power of a ℤ* is a float
                                  #;[(fixnum?) orig-** (fixnum? fixnum?)]
                                  [(fixnum?) orig-** (fixnum? natural-fixnum?)] ; a ℕ exponent : an integer (the overflow is accepted)
                                  [(rational?) orig-** (fixnum? fixnum?)] ; (expt 2 -1) is 1/2 : declare the exponent ℕ for fx+ after
                                  [(rational?) orig-** (exact-integer? exact-integer?)] ; (expt 2 -1) is 1/2
                                  [(real?) orig-** (integer? integer?)] ; (expt 2 -1) is 1/2
                                  [(number?) orig-** (number? number?)]
                                  [(inexact?) orig-** (flonum? flonum?)] ; (expt -1.0 1.5) is a complex
                                  #;[(flonum?) orig-** (flonum? fixnum?)] ; a flonum to an integer power , as {z ²} : a flonum (the exponent 0 must be written 0.0)
                                  [(real?) orig-** (flonum? fixnum?)] ; (expt 2.0 0) is the exact 1 : declare the exponent ℤ* for fl+ after
                                  [(number?) orig-** (real? real?)] ; (expt -1 1/2) is +i
                                  [(complex?) orig-** (complex? complex?)]
                                  [(inexact?) orig-** (inexact? inexact?)]
                                  #;[(inexact?) orig-** (inexact? integer?)]
                                  [(number?) orig-** (inexact? integer?)] ; (expt 2.0 0) is the exact 1
                                  [(number?) orig-** (integer? inexact?)])}

         ; this continues overloading with others orig-*** operators
        
         {ovrld-ht['expt] := '([(flonum?) flexpt (nonnegative-flonum? flonum?)] ; a ℝ⁺ to a float power : a float , not a complex (before the rows of flonums)
                               [(flonum?) orig-expt (flonum? nonzero-fixnum?)] ; a float to the power of a ℤ* is a float
                              #;[(fixnum?) orig-expt (fixnum? fixnum?)]
                              [(fixnum?) orig-expt (fixnum? natural-fixnum?)] ; a ℕ exponent : an integer (the overflow is accepted)
                              [(rational?) orig-expt (fixnum? fixnum?)] ; (expt 2 -1) is 1/2 : declare the exponent ℕ for fx+ after
                              [(rational?) orig-expt (exact-integer? exact-integer?)] ; (expt 2 -1) is 1/2
                              [(real?) orig-expt (integer? integer?)] ; (expt 2 -1) is 1/2
                              [(number?) orig-expt (number? number?)]
                              [(inexact?) orig-expt (flonum? flonum?)] ; (expt -1.0 1.5) is a complex
                              #;[(flonum?) orig-expt (flonum? fixnum?)] ; a flonum to an integer power , as {z ²} : a flonum (the exponent 0 must be written 0.0)
                              [(real?) orig-expt (flonum? fixnum?)] ; (expt 2.0 0) is the exact 1 : declare the exponent ℤ* for fl+ after
                              [(number?) orig-expt (real? real?)] ; (expt -1 1/2) is +i
                              [(complex?) orig-expt (complex? complex?)]
                              [(inexact?) orig-expt (inexact? inexact?)]
                              #;[(inexact?) orig-expt (inexact? integer?)]
                              [(number?) orig-expt (inexact? integer?)] ; (expt 2.0 0) is the exact 1
                              [(number?) orig-expt (integer? inexact?)])}

         {ovrld-ht['orig-expt] := '([(flonum?) flexpt (nonnegative-flonum? flonum?)] ; a ℝ⁺ to a float power : a float , not a complex (before the rows of flonums)
                                    [(flonum?) orig-expt (flonum? nonzero-fixnum?)] ; a float to the power of a ℤ* is a float
                              #;[(fixnum?) orig-expt (fixnum? fixnum?)]
                              [(fixnum?) orig-expt (fixnum? natural-fixnum?)] ; a ℕ exponent : an integer (the overflow is accepted)
                              [(rational?) orig-expt (fixnum? fixnum?)] ; (expt 2 -1) is 1/2 : declare the exponent ℕ for fx+ after
                              [(rational?) orig-expt (exact-integer? exact-integer?)] ; (expt 2 -1) is 1/2
                              [(real?) orig-expt (integer? integer?)] ; (expt 2 -1) is 1/2
                              [(number?) orig-expt (number? number?)]
                              [(inexact?) orig-expt (flonum? flonum?)] ; (expt -1.0 1.5) is a complex
                              #;[(flonum?) orig-expt (flonum? fixnum?)] ; a flonum to an integer power , as {z ²} : a flonum (the exponent 0 must be written 0.0)
                              [(real?) orig-expt (flonum? fixnum?)] ; (expt 2.0 0) is the exact 1 : declare the exponent ℤ* for fl+ after
                              [(number?) orig-expt (real? real?)] ; (expt -1 1/2) is +i
                              [(complex?) orig-expt (complex? complex?)]
                              [(inexact?) orig-expt (inexact? inexact?)]
                              #;[(inexact?) orig-expt (inexact? integer?)]
                              [(number?) orig-expt (inexact? integer?)] ; (expt 2.0 0) is the exact 1
                              [(number?) orig-expt (integer? inexact?)])}

         
         {ovrld-ht['<] := '([(boolean?) fx< (fixnum? fixnum?)]
                           [(boolean?) orig-< (exact-integer? exact-integer?)]
                           [(boolean?) orig-< (integer? integer?)]
                           [(boolean?) orig-< (number? number?)]
                           [(boolean?) fl< (flonum? flonum?)]
                           [(boolean?) orig-< (real? real?)]
                           [(boolean?) orig-< (complex? complex?)]
                           [(boolean?) orig-< (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(boolean?) fx/fl< (fixnum? flonum?)]
                           [(boolean?) fl</fx (flonum? fixnum?)]
                           [(boolean?) orig-< (fixnum? exact-integer?)]
                           [(boolean?) orig-< (exact-integer? fixnum?)]
                           [(boolean?) orig-< (flonum? exact-integer?)]
                           [(boolean?) orig-< (exact-integer? flonum?)])}

         {ovrld-ht['orig-<] := '([(boolean?) fx< (fixnum? fixnum?)]
                           [(boolean?) orig-< (exact-integer? exact-integer?)]
                           [(boolean?) orig-< (integer? integer?)]
                           [(boolean?) orig-< (number? number?)]
                           [(boolean?) fl< (flonum? flonum?)]
                           [(boolean?) orig-< (real? real?)]
                           [(boolean?) orig-< (complex? complex?)]
                           [(boolean?) orig-< (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(boolean?) fx/fl< (fixnum? flonum?)]
                           [(boolean?) fl</fx (flonum? fixnum?)]
                           [(boolean?) orig-< (fixnum? exact-integer?)]
                           [(boolean?) orig-< (exact-integer? fixnum?)]
                           [(boolean?) orig-< (flonum? exact-integer?)]
                           [(boolean?) orig-< (exact-integer? flonum?)])}

         {ovrld-ht['>] := '([(boolean?) fx> (fixnum? fixnum?)]
                           [(boolean?) orig-> (exact-integer? exact-integer?)]
                           [(boolean?) orig-> (integer? integer?)]
                           [(boolean?) orig-> (number? number?)]
                           [(boolean?) fl> (flonum? flonum?)]
                           [(boolean?) orig-> (real? real?)]
                           [(boolean?) orig-> (complex? complex?)]
                           [(boolean?) orig-> (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(boolean?) fx/fl> (fixnum? flonum?)]
                           [(boolean?) fl>/fx (flonum? fixnum?)]
                           [(boolean?) orig-> (fixnum? exact-integer?)]
                           [(boolean?) orig-> (exact-integer? fixnum?)]
                           [(boolean?) orig-> (flonum? exact-integer?)]
                           [(boolean?) orig-> (exact-integer? flonum?)])}

         {ovrld-ht['orig->] := '([(boolean?) fx> (fixnum? fixnum?)]
                           [(boolean?) orig-> (exact-integer? exact-integer?)]
                           [(boolean?) orig-> (integer? integer?)]
                           [(boolean?) orig-> (number? number?)]
                           [(boolean?) fl> (flonum? flonum?)]
                           [(boolean?) orig-> (real? real?)]
                           [(boolean?) orig-> (complex? complex?)]
                           [(boolean?) orig-> (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(boolean?) fx/fl> (fixnum? flonum?)]
                           [(boolean?) fl>/fx (flonum? fixnum?)]
                           [(boolean?) orig-> (fixnum? exact-integer?)]
                           [(boolean?) orig-> (exact-integer? fixnum?)]
                           [(boolean?) orig-> (flonum? exact-integer?)]
                           [(boolean?) orig-> (exact-integer? flonum?)])}


         {ovrld-ht['<=] := '([(boolean?) fx<= (fixnum? fixnum?)]
                            [(boolean?) orig-<= (exact-integer? exact-integer?)]
                            [(boolean?) orig-<= (integer? integer?)]
                            [(boolean?) orig-<= (number? number?)]
                            [(boolean?) fl<= (flonum? flonum?)]
                            [(boolean?) orig-<= (real? real?)]
                            [(boolean?) orig-<= (complex? complex?)]
                            [(boolean?) orig-<= (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                            [(boolean?) fx/fl<= (fixnum? flonum?)]
                            [(boolean?) fl<=/fx (flonum? fixnum?)]
                            [(boolean?) orig-<= (fixnum? exact-integer?)]
                            [(boolean?) orig-<= (exact-integer? fixnum?)]
                            [(boolean?) orig-<= (flonum? exact-integer?)]
                            [(boolean?) orig-<= (exact-integer? flonum?)])}

         {ovrld-ht['orig-<=] := '([(boolean?) fx<= (fixnum? fixnum?)]
                                  [(boolean?) orig-<= (exact-integer? exact-integer?)]
                                  [(boolean?) orig-<= (integer? integer?)]
                                  [(boolean?) orig-<= (number? number?)]
                                  [(boolean?) fl<= (flonum? flonum?)]
                                  [(boolean?) orig-<= (real? real?)]
                                  [(boolean?) orig-<= (complex? complex?)]
                                  [(boolean?) orig-<= (inexact? inexact?)]
                                  ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                                  ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                                  [(boolean?) fx/fl<= (fixnum? flonum?)]
                                  [(boolean?) fl<=/fx (flonum? fixnum?)]
                                  [(boolean?) orig-<= (fixnum? exact-integer?)]
                                  [(boolean?) orig-<= (exact-integer? fixnum?)]
                                  [(boolean?) orig-<= (flonum? exact-integer?)]
                                  [(boolean?) orig-<= (exact-integer? flonum?)])}

         ; ≤ and ≥ are the same procedures as <= and >= (see Scheme+/little-greater-or-equal)
         {ovrld-ht['≤] := '([(boolean?) fx<= (fixnum? fixnum?)]
                            [(boolean?) orig-<= (exact-integer? exact-integer?)]
                            [(boolean?) orig-<= (integer? integer?)]
                            [(boolean?) orig-<= (number? number?)]
                            [(boolean?) fl<= (flonum? flonum?)]
                            [(boolean?) orig-<= (real? real?)]
                            [(boolean?) orig-<= (complex? complex?)]
                            [(boolean?) orig-<= (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                            [(boolean?) fx/fl<= (fixnum? flonum?)]
                            [(boolean?) fl<=/fx (flonum? fixnum?)]
                            [(boolean?) orig-<= (fixnum? exact-integer?)]
                            [(boolean?) orig-<= (exact-integer? fixnum?)]
                            [(boolean?) orig-<= (flonum? exact-integer?)]
                            [(boolean?) orig-<= (exact-integer? flonum?)])}

         {ovrld-ht['≥] := '([(boolean?) fx>= (fixnum? fixnum?)]
                            [(boolean?) orig->= (exact-integer? exact-integer?)]
                            [(boolean?) orig->= (integer? integer?)]
                            [(boolean?) orig->= (number? number?)]
                            [(boolean?) fl>= (flonum? flonum?)]
                            [(boolean?) orig->= (real? real?)]
                            [(boolean?) orig->= (complex? complex?)]
                            [(boolean?) orig->= (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                            [(boolean?) fx/fl>= (fixnum? flonum?)]
                            [(boolean?) fl>=/fx (flonum? fixnum?)]
                            [(boolean?) orig->= (fixnum? exact-integer?)]
                            [(boolean?) orig->= (exact-integer? fixnum?)]
                            [(boolean?) orig->= (flonum? exact-integer?)]
                            [(boolean?) orig->= (exact-integer? flonum?)])}

         ; ≠ and <> are (not (= x y)) (see Scheme+/not-equal) , fx≠ and fl≠ are their versions for fixnums and flonums,
         ; orig-≠ and orig-<> the original ≠ and <> (backup operators, as ≠ and <> can be overloaded too)
         {ovrld-ht['≠] := '([(boolean?) fx≠ (fixnum? fixnum?)]
                            [(boolean?) orig-≠ (exact-integer? exact-integer?)]
                            [(boolean?) orig-≠ (integer? integer?)]
                            [(boolean?) orig-≠ (number? number?)]
                            [(boolean?) fl≠ (flonum? flonum?)]
                            [(boolean?) orig-≠ (real? real?)]
                            [(boolean?) orig-≠ (complex? complex?)]
                            [(boolean?) orig-≠ (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                            [(boolean?) fx/fl≠ (fixnum? flonum?)]
                            [(boolean?) fl≠/fx (flonum? fixnum?)]
                            [(boolean?) orig-≠ (fixnum? exact-integer?)]
                            [(boolean?) orig-≠ (exact-integer? fixnum?)]
                            [(boolean?) orig-≠ (flonum? exact-integer?)]
                            [(boolean?) orig-≠ (exact-integer? flonum?)])}

         {ovrld-ht['orig-≠] := '([(boolean?) fx≠ (fixnum? fixnum?)]
                                 [(boolean?) orig-≠ (exact-integer? exact-integer?)]
                                 [(boolean?) orig-≠ (integer? integer?)]
                                 [(boolean?) orig-≠ (number? number?)]
                                 [(boolean?) fl≠ (flonum? flonum?)]
                                 [(boolean?) orig-≠ (real? real?)]
                                 [(boolean?) orig-≠ (complex? complex?)]
                                 [(boolean?) orig-≠ (inexact? inexact?)]
                                 ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                                 ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                                 [(boolean?) fx/fl≠ (fixnum? flonum?)]
                                 [(boolean?) fl≠/fx (flonum? fixnum?)]
                                 [(boolean?) orig-≠ (fixnum? exact-integer?)]
                                 [(boolean?) orig-≠ (exact-integer? fixnum?)]
                                 [(boolean?) orig-≠ (flonum? exact-integer?)]
                                 [(boolean?) orig-≠ (exact-integer? flonum?)])}

         {ovrld-ht['<>] := '([(boolean?) fx≠ (fixnum? fixnum?)]
                             [(boolean?) orig-<> (exact-integer? exact-integer?)]
                             [(boolean?) orig-<> (integer? integer?)]
                             [(boolean?) orig-<> (number? number?)]
                             [(boolean?) fl≠ (flonum? flonum?)]
                             [(boolean?) orig-<> (real? real?)]
                             [(boolean?) orig-<> (complex? complex?)]
                             [(boolean?) orig-<> (inexact? inexact?)]
                             ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                             ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                             [(boolean?) fx/fl≠ (fixnum? flonum?)]
                             [(boolean?) fl≠/fx (flonum? fixnum?)]
                             [(boolean?) orig-<> (fixnum? exact-integer?)]
                             [(boolean?) orig-<> (exact-integer? fixnum?)]
                             [(boolean?) orig-<> (flonum? exact-integer?)]
                             [(boolean?) orig-<> (exact-integer? flonum?)])}

         {ovrld-ht['orig-<>] := '([(boolean?) fx≠ (fixnum? fixnum?)]
                                  [(boolean?) orig-<> (exact-integer? exact-integer?)]
                                  [(boolean?) orig-<> (integer? integer?)]
                                  [(boolean?) orig-<> (number? number?)]
                                  [(boolean?) fl≠ (flonum? flonum?)]
                                  [(boolean?) orig-<> (real? real?)]
                                  [(boolean?) orig-<> (complex? complex?)]
                                  [(boolean?) orig-<> (inexact? inexact?)]
                                  ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                                  ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                                  [(boolean?) fx/fl≠ (fixnum? flonum?)]
                                  [(boolean?) fl≠/fx (flonum? fixnum?)]
                                  [(boolean?) orig-<> (fixnum? exact-integer?)]
                                  [(boolean?) orig-<> (exact-integer? fixnum?)]
                                  [(boolean?) orig-<> (flonum? exact-integer?)]
                                  [(boolean?) orig-<> (exact-integer? flonum?)])}


         {ovrld-ht['>=] := '([(boolean?) fx>= (fixnum? fixnum?)]
                            [(boolean?) orig->= (exact-integer? exact-integer?)]
                            [(boolean?) orig->= (integer? integer?)]
                            [(boolean?) orig->= (number? number?)]
                            [(boolean?) fl>= (flonum? flonum?)]
                            [(boolean?) orig->= (real? real?)]
                            [(boolean?) orig->= (complex? complex?)]
                            [(boolean?) orig->= (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                            [(boolean?) fx/fl>= (fixnum? flonum?)]
                            [(boolean?) fl>=/fx (flonum? fixnum?)]
                            [(boolean?) orig->= (fixnum? exact-integer?)]
                            [(boolean?) orig->= (exact-integer? fixnum?)]
                            [(boolean?) orig->= (flonum? exact-integer?)]
                            [(boolean?) orig->= (exact-integer? flonum?)])}

         {ovrld-ht['orig->=] := '([(boolean?) fx>= (fixnum? fixnum?)]
                            [(boolean?) orig->= (exact-integer? exact-integer?)]
                            [(boolean?) orig->= (integer? integer?)]
                            [(boolean?) orig->= (number? number?)]
                            [(boolean?) fl>= (flonum? flonum?)]
                            [(boolean?) orig->= (real? real?)]
                            [(boolean?) orig->= (complex? complex?)]
                            [(boolean?) orig->= (inexact? inexact?)]
                            ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                            ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                            [(boolean?) fx/fl>= (fixnum? flonum?)]
                            [(boolean?) fl>=/fx (flonum? fixnum?)]
                            [(boolean?) orig->= (fixnum? exact-integer?)]
                            [(boolean?) orig->= (exact-integer? fixnum?)]
                            [(boolean?) orig->= (flonum? exact-integer?)]
                            [(boolean?) orig->= (exact-integer? flonum?)])}

         {ovrld-ht['=] := '([(boolean?) fx= (fixnum? fixnum?)]
                           [(boolean?) orig-= (exact-integer? exact-integer?)]
                           [(boolean?) orig-= (integer? integer?)]
                           [(boolean?) orig-= (number? number?)]
                           [(boolean?) fl= (flonum? flonum?)]
                           [(boolean?) orig-= (real? real?)]
                           [(boolean?) orig-= (complex? complex?)]
                           [(boolean?) orig-= (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(boolean?) fx/fl= (fixnum? flonum?)]
                           [(boolean?) fl=/fx (flonum? fixnum?)]
                           [(boolean?) orig-= (fixnum? exact-integer?)]
                           [(boolean?) orig-= (exact-integer? fixnum?)]
                           [(boolean?) orig-= (flonum? exact-integer?)]
                           [(boolean?) orig-= (exact-integer? flonum?)])}

         {ovrld-ht['orig-=] := '([(boolean?) fx= (fixnum? fixnum?)]
                           [(boolean?) orig-= (exact-integer? exact-integer?)]
                           [(boolean?) orig-= (integer? integer?)]
                           [(boolean?) orig-= (number? number?)]
                           [(boolean?) fl= (flonum? flonum?)]
                           [(boolean?) orig-= (real? real?)]
                           [(boolean?) orig-= (complex? complex?)]
                           [(boolean?) orig-= (inexact? inexact?)]
                           ; mixed types : a flonum and a fixnum , fl+/fx , fx/fl+ ... of Scheme+/flonum-fixnum (the same result as the
                           ; generic procedure , a guard at run time) ; an exact integer : the generic procedure , only the return type is given
                           [(boolean?) fx/fl= (fixnum? flonum?)]
                           [(boolean?) fl=/fx (flonum? fixnum?)]
                           [(boolean?) orig-= (fixnum? exact-integer?)]
                           [(boolean?) orig-= (exact-integer? fixnum?)]
                           [(boolean?) orig-= (flonum? exact-integer?)]
                           [(boolean?) orig-= (exact-integer? flonum?)])}


         ; n-ary operator hash table
         {ovrld-ht-n-ary := (make-hash-table)} ; this hash table will be used for overloading n-ary operators

         {ovrld-ht-n-ary['+] := '([(nonnegative-flonum?) fl+ (nonnegative-flonum?)]
                                  [(fixnum?) fx+ (fixnum?)]
                                   [(exact-integer?) orig-+ (exact-integer?)]
                                   [(integer?) orig-+ (integer?)]
                                   [(number?) orig-+ (number?)]
                                   [(flonum?) fl+ (flonum?)]
                                   [(real?) orig-+ (real?)]
                                   [(rational?) orig-+ (rational?)]
                                   [(complex?) orig-+ (complex?)]
                                   [(inexact?) orig-+ (inexact?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['orig-+] := '([(nonnegative-flonum?) fl+ (nonnegative-flonum?)]
                                       [(fixnum?) fx+ (fixnum?)]
                                   [(exact-integer?) orig-+ (exact-integer?)]
                                   [(integer?) orig-+ (integer?)]
                                   [(number?) orig-+ (number?)]
                                   [(flonum?) fl+ (flonum?)]
                                   [(real?) orig-+ (real?)]
                                   [(rational?) orig-+ (rational?)]
                                   [(complex?) orig-+ (complex?)]
                                   [(inexact?) orig-+ (inexact?)])}

         
         {ovrld-ht-n-ary['-] := '([(fixnum?) fx- (fixnum?)]
                                   [(exact-integer?) orig-- (exact-integer?)]
                                   [(integer?) orig-- (integer?)]
                                   [(number?) orig-- (number?)]
                                   [(flonum?) fl- (flonum?)]
                                   [(real?) orig-- (real?)]
                                   [(rational?) orig-- (rational?)]
                                   [(complex?) orig-- (complex?)]
                                   [(inexact?) orig-- (inexact?)])}

         {ovrld-ht-n-ary['orig--] := '([(fixnum?) fx- (fixnum?)]
                                   [(exact-integer?) orig-- (exact-integer?)]
                                   [(integer?) orig-- (integer?)]
                                   [(number?) orig-- (number?)]
                                   [(flonum?) fl- (flonum?)]
                                   [(real?) orig-- (real?)]
                                   [(rational?) orig-- (rational?)]
                                   [(complex?) orig-- (complex?)]
                                   [(inexact?) orig-- (inexact?)])}

         {ovrld-ht-n-ary['/] := '([(rational?) orig-/ (exact-integer?)] ; (/ 1 2) is 1/2
                                    [(real?) orig-/ (integer?)] ; (/ 1 2) is 1/2
                                    [(number?) orig-/ (number?)]
                                    [(flonum?) fl/ (flonum?)]
                                    [(real?) orig-/ (real?)]
                                    [(rational?) orig-/ (rational?)]
                                    [(complex?) orig-/ (complex?)]
                                    [(inexact?) orig-/ (inexact?)])}

         {ovrld-ht-n-ary['orig-/] := '([(rational?) orig-/ (exact-integer?)] ; (/ 1 2) is 1/2
                                    [(real?) orig-/ (integer?)] ; (/ 1 2) is 1/2
                                    [(number?) orig-/ (number?)]
                                    [(flonum?) fl/ (flonum?)]
                                    [(real?) orig-/ (real?)]
                                    [(rational?) orig-/ (rational?)]
                                    [(complex?) orig-/ (complex?)]
                                    [(inexact?) orig-/ (inexact?)])}

         {ovrld-ht-n-ary['*] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                  [(fixnum?) fx* (fixnum?)]
                                    [(exact-integer?) orig-* (exact-integer?)]
                                    [(integer?) orig-* (integer?)]
                                    [(number?) orig-* (number?)]
                                    [(flonum?) fl* (flonum?)]
                                    [(real?) orig-* (real?)]
                                    [(rational?) orig-* (rational?)]
                                    [(complex?) orig-* (complex?)]
                                    [(inexact?) orig-* (inexact?)])}

         {ovrld-ht-n-ary['orig-*] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                       [(fixnum?) fx* (fixnum?)]
                                    [(exact-integer?) orig-* (exact-integer?)]
                                    [(integer?) orig-* (integer?)]
                                    [(number?) orig-* (number?)]
                                    [(flonum?) fl* (flonum?)]
                                    [(real?) orig-* (real?)]
                                    [(rational?) orig-* (rational?)]
                                    [(complex?) orig-* (complex?)]
                                    [(inexact?) orig-* (inexact?)])}

         {ovrld-ht-n-ary['·] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                  [(fixnum?) fx* (fixnum?)]
                                    [(exact-integer?) orig-· (exact-integer?)]
                                    [(integer?) orig-· (integer?)]
                                    [(number?) orig-· (number?)]
                                    [(flonum?) fl* (flonum?)]
                                    [(real?) orig-· (real?)]
                                    [(rational?) orig-· (rational?)]
                                    [(complex?) orig-· (complex?)]
                                    [(inexact?) orig-· (inexact?)])}

         {ovrld-ht-n-ary['orig-·] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                       [(fixnum?) fx* (fixnum?)]
                                    [(exact-integer?) orig-· (exact-integer?)]
                                    [(integer?) orig-· (integer?)]
                                    [(number?) orig-· (number?)]
                                    [(flonum?) fl* (flonum?)]
                                    [(real?) orig-· (real?)]
                                    [(rational?) orig-· (rational?)]
                                    [(complex?) orig-· (complex?)]
                                    [(inexact?) orig-· (inexact?)])}

         ; × is also a Scheme+ multiplication, same overloads as ·
         {ovrld-ht-n-ary['×] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                  [(fixnum?) fx* (fixnum?)]
                                    [(exact-integer?) orig-× (exact-integer?)]
                                    [(integer?) orig-× (integer?)]
                                    [(number?) orig-× (number?)]
                                    [(flonum?) fl* (flonum?)]
                                    [(real?) orig-× (real?)]
                                    [(rational?) orig-× (rational?)]
                                    [(complex?) orig-× (complex?)]
                                    [(inexact?) orig-× (inexact?)])}

         {ovrld-ht-n-ary['orig-×] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                       [(fixnum?) fx* (fixnum?)]
                                    [(exact-integer?) orig-× (exact-integer?)]
                                    [(integer?) orig-× (integer?)]
                                    [(number?) orig-× (number?)]
                                    [(flonum?) fl* (flonum?)]
                                    [(real?) orig-× (real?)]
                                    [(rational?) orig-× (rational?)]
                                    [(complex?) orig-× (complex?)]
                                    [(inexact?) orig-× (inexact?)])}

         {ovrld-ht-n-ary['**] := '(#;[(fixnum?) orig-** (fixnum?)]
                                   [(fixnum?) orig-** (natural-fixnum?)] ; ℕ exponents : an integer (the overflow is accepted)
                                   [(rational?) orig-** (fixnum?)] ; (expt 2 -1) is 1/2
                                     [(rational?) orig-** (exact-integer?)] ; (expt 2 -1) is 1/2
                                     [(real?) orig-** (integer?)] ; (expt 2 -1) is 1/2
                                     [(number?) orig-** (number?)]
                                     [(inexact?) orig-** (flonum?)] ; (expt -1.0 1.5) is a complex
                                     [(number?) orig-** (real?)] ; (expt -1 1/2) is +i
                                     [(complex?) orig-** (complex?)]
                                     [(inexact?) orig-** (inexact?)])}

         {ovrld-ht-n-ary['orig-**] := '(#;[(fixnum?) orig-** (fixnum?)]
                                        [(fixnum?) orig-** (natural-fixnum?)] ; ℕ exponents : an integer (the overflow is accepted)
                                        [(rational?) orig-** (fixnum?)] ; (expt 2 -1) is 1/2
                                     [(rational?) orig-** (exact-integer?)] ; (expt 2 -1) is 1/2
                                     [(real?) orig-** (integer?)] ; (expt 2 -1) is 1/2
                                     [(number?) orig-** (number?)]
                                     [(inexact?) orig-** (flonum?)] ; (expt -1.0 1.5) is a complex
                                     [(number?) orig-** (real?)] ; (expt -1 1/2) is +i
                                     [(complex?) orig-** (complex?)]
                                     [(inexact?) orig-** (inexact?)])}

         ;; expt not n-ary

         ;; the overload below seems only used internally by the parser for args-types-after-annot
         ;; uncomment below if you want args-types-after-annot being displayed

         ; now flonum  overload with itself !!!
         ; only the row of flonums is kept : the other rows gave only the return type flonum? , that racket-types.rkt gives
         ; now for all the arguments (fl+ returns a flonum or raises an error)
         {ovrld-ht['fl+] := '([(nonnegative-flonum?) fl+ (nonnegative-flonum? nonnegative-flonum?)]
                             [(flonum?) fl+ (flonum? flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht['fl-] := '([(flonum?) fl- (flonum? flonum?)])}

         {ovrld-ht['fl/] := '([(flonum?) fl/ (flonum? flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht['fl*] := '([(nonnegative-flonum?) fl* (nonnegative-flonum? nonnegative-flonum?)]
                             [(flonum?) fl* (flonum? flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['fl+] := '([(nonnegative-flonum?) fl+ (nonnegative-flonum?)]
                                     [(flonum?) fl+ (flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['fl-] := '([(flonum?) fl- (flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['fl/] := '([(flonum?) fl/ (flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['fl*] := '([(nonnegative-flonum?) fl* (nonnegative-flonum?)]
                                     [(flonum?) fl* (flonum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         ; fixnums :

         ;; ; now fixnum  overload with itself !!!
         ; no rows of exact integers : they rewrote a fx+ written by the programmer on a bignum in orig-+ ,
         ; a value with the annotation where Racket raises an error without it ; the return type of fx+ is in racket-types.rkt
         {ovrld-ht['fx+] := '([(fixnum?) fx+ (fixnum? fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht['fx-] := '([(fixnum?) fx- (fixnum? fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa
         
      
         {ovrld-ht['fx*] := '([(fixnum?) fx* (fixnum? fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

       
         {ovrld-ht['fxmodulo] := '([(fixnum?) fxmodulo (fixnum? fixnum?)])}

         ; n-ary

         {ovrld-ht-n-ary['fx+] := '([(fixnum?) fx+ (fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['fx-] := '([(fixnum?) fx- (fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-n-ary['fx*] := '([(fixnum?) fx* (fixnum?)])} ; note here[ ] are only ( ) , works with Racket,Guile, not Kawa

         {ovrld-ht-proc := (make-hash-table)} ; this hash table will be used for overloading procedures (not operators), example: random
         ; example :
         ;{ovrld-ht-proc['random] := '([(inexact?) random ()])} ; moved here in ovrld-ht-proc (from ovrld-ht) ,now in user program

         {annot-ht-lst := '()} ; is for the type annotation of variables
         {annot-ht-lst := (cons (make-hash-table) annot-ht-lst)} ; or (list (make-hash-table)), will be used for annotation at toplevel or REPL (empty at beginning)
         ; any nested annotation will be stored in another hash table of the list, so we will have a list of hash tables from toplevel to nested blocks
         ; in reverse order (toplevel at the end) so the first annotation found is the more relevant relative to the local variable environment we are.
         ))))


  ; known-proc hash table is used to map original operator/procedure symbol to the actual procedure for computation of litterals
  {known-proc <+ (make-hash-table)} ; this hash table will be used for known procedures to map with symbols

  ; note that in the parser the procedures + - / * ** · expt % etc... are never overloaded, so we can use them for fast computation
  ; those ones are used:
  {known-proc['+] := +}
  {known-proc['-] := -}
  {known-proc['/] := /}
  {known-proc['*] := *}
  {known-proc['**] := **}
  {known-proc['·] := ·}
  {known-proc['expt] := expt}
  {known-proc['%] := %}



  ; return the procedure bound to 'key in known-proc if it exists, #f otherwise
  (define (known-proc-ref key)
    {(hash-table-exists? known-proc key) and known-proc[key]})



  ; (remove-expr '(() a (b (c d) e) f) (lambda (x) (equal? x '(c d))))
  ; '(() a (b e) f)
  ; warning: predicat p? works on list sexp, with that remove-expr is ok,unless do not remove atoms
  #;(define (remove-expr s p?)
    (cond ((pair? s) (if (p? (car s))
                         (remove-expr (cdr s) p?) ; forget the head
                         (cons (remove-expr (car s) p?)
                               (remove-expr (cdr s) p?))))
          (else s)))

  (define (remove-expr s p?)
    (cond ((pair? s) (if (p? (car s))
                         (link-form! (remove-expr (cdr s) p?) s) ; forget the head
                         (link-cell! (cons (remove-expr (car s) p?) ; the copy keeps the locations of the source
                                           (remove-expr (cdr s) p?))
                                     s)))
          (else s)))


  ; regions of the pragmas ANNOTATION-ON / ANNOTATION-OFF : the reader leaves the pragmas in the code as markers.
  ; A pragma acts on the elements that follow it in the same list (and on all that is nested in them), until the next
  ; pragma or the end of the list, then the state of before comes back (see process-lst-for-annot). A file starts with
  ; the annotation off.
  ; note: the symbols are built from strings, because in a list read by the reader an ANNOTATION-ON written as an
  ; identifier is a pragma (and a quoted one is data , see remove-annotation-markers)
  (define annotation-on-symbol (string->symbol "ANNOTATION-ON"))
  (define annotation-off-symbol (string->symbol "ANNOTATION-OFF"))

  (define (annotation-marker? s)
    (or (eq? s annotation-on-symbol)
        (eq? s annotation-off-symbol)))

  ; #t in an ANNOTATION-ON region ; parameterize (R7RS) gives back the state of before at the end of the list
  (define annotation-active? (make-parameter #f))

  ; replaced by remove-types-and-annotation-markers : remove-expr also removed the pragma symbols of the quoted
  ; expressions ('ANNOTATION-ON became (quote))
  #;(define (type-expression-or-annotation-marker? s)
    (or (is-type-expression? s)
        (annotation-marker? s)))

  ; remove the markers of the pragmas, but not in a quoted expression : 'ANNOTATION-ON is data
  (define (remove-annotation-markers s)
    (cond ((not (pair? s)) s)
          ((or (quote-or-quasi? (car s)) ; (quote ...) , (quasiquote ...)
               (SYNTAX-QUOTE-op? (car s))) ; (syntax ...) , (quasisyntax ...) , (quote-syntax ...)
           s)
          ((annotation-marker? (car s)) (link-form! (remove-annotation-markers (cdr s)) s))
          #;(else (cons (remove-annotation-markers (car s))
                      (remove-annotation-markers (cdr s))))
          (else (link-cell! (cons (remove-annotation-markers (car s)) ; the copy keeps the locations of the source
                                  (remove-annotation-markers (cdr s)))
                            s))))

  ; when the annotation pass does not run : remove the type annotations (as before) and the markers of the pragmas
  (define (remove-types-and-annotation-markers s)
    (remove-annotation-markers (remove-expr s is-type-expression?)))

  ; a macro definition : its patterns and templates are not code of the current scope, and a transformer is code of
  ; the compile phase : the types known here do not apply to them (without this : {a := 1} {b := 2} then
  ; (define-syntax-rule (add a b) (+ a b)) gave (fx+ a b) , and (syntax-rules (+) ...) gave (syntax-rules 0 ...))
  ; define-syntax is standard (R5RS, R6RS, R7RS) , define-syntax-rule , define-for-syntax , begin-for-syntax are Racket
  (define (macro-definition? s)
    (memq s '(define-syntax define-syntax-rule define-for-syntax begin-for-syntax)))

  ; local macro definitions (standard) : the macros are not annotated , the body is
  (define (local-macro-definition? s)
    (memq s '(let-syntax letrec-syntax)))


  ; local bindings : a parameter or a local variable hides the type of the outer variable of the same name
  ; (without this : with x and y typed fixnums outside , (define (g x y) (+ x y)) gave (fx+ x y)).
  ; A form that binds names (see bound-names) writes these names in the table of the level it creates, with the
  ; marker below : the search of a type stops at the first level that knows the name, and the marker gives an
  ; unknown type. A (type ...) or an inference at the same level replaces the marker. The key (local-binding name)
  ; stays : it hides also the types of the elements v[i] of an outer v (see search-annot-hash-tables).
  (define local-binding-without-type 'local-binding-without-type)

  (define (local-binding-key name)
    (list 'local-binding name))

  (define (mark-local-binding! name ht)
    {ht[name] := local-binding-without-type}
    {ht[(local-binding-key name)] := #t})

  ; the variable at the base of a key : x for x , T for ($bracket-apply$ T i) or ($bracket-apply$ ($bracket-apply$ T i) j)
  (define (base-variable s)
    (cond ((symbol? s) s)
          ((and (pair? s)
		(isBRACKET-APPLY*? s)
		(pair? (cdr s)))
	   (base-variable (cadr s)))
          (else #f)))

  ; the value of the key s in the most nested level that knows it (the levels are in annot-ht-lst , the most nested
  ; first) : a local binding without type gives #f , and a local binding of the base variable of s (v for v[i])
  ; hides the keys of the outer levels
  (define (search-annot-hash-tables s annot-ht-lst)
    (define base (base-variable s))
    (define (loop l)
      (cond ((null? l) #f)
            ((hash-table-exists? (car l) s)
             {v <+ (car l)[s]}
             (if (eq? v local-binding-without-type) #f v))
            ((and base
		  (hash-table-exists? (car l) (local-binding-key base)))
	     #f) ; an element of a local variable
            (else (loop (cdr l)))))
    (loop annot-ht-lst))

  ; the names of a list of parameters : (a b) , (a b . rest) , rest ; for the optional arguments of Racket [a 1] the
  ; name a , the keywords #:k are ignored (a name too much would only hide a type : never a wrong type)
  (define (formals-names f)
    (cond ((symbol? f) (list f))
          ((pair? f)
	   (append (formal-name (car f)) (formals-names (cdr f))))
          (else '())))

  (define (formal-name e)
    (cond ((symbol? e) (list e))
          ((and (pair? e) (symbol? (car e)))
	   (list (car e))) ; [a default]
          (else '())))

  ; the parameters of a header of procedure : (f a b . rest) , or curried ((f a) b)
  (define (header-names h)
    (if (pair? (car h))
        (append (header-names (car h))
		(formals-names (cdr h)))
        (formals-names (cdr h))))

  ; (do ((var init step) ...) (test expr ...) body ...) of R7RS , not the (do body ... while pred) of Scheme+
  (define (do-R7RS? s)
    (and (pair? (cdr s))
         (list? (cadr s))
         (every (λ (b) (and (pair? b)
			    (symbol? (car b))))
		(cadr s))
         (pair? (cddr s))
         (list? (caddr s))
         (not (memq 'while s))))

  ; the names of the bindings of a let : ((name init) ...)
  (define (bindings-names bindings)
    (if (list? bindings)
        (apply append (map-in-order (λ (b) (if (and (pair? b)
						    (symbol? (car b)))
                                               (list (car b))
                                               '()))
                                    bindings))
        '()))

  ; the names bound by the form s in the level it creates , '() for the other forms
  ; old comment : (the let forms are not taken in account : Scheme+ uses <- := <+)
  ; the let forms are taken in account : Scheme+ uses <- := <+ , but a let of Scheme code in a region ANNOTATION-ON must
  ; hide the types of the outer variables of the same names (without this : (let ((a 1.5) (b 2.5)) (+ a b)) became
  ; (fx+ a b) when a and b were fixnums outside)
  (define (bound-names s)
    (define head (car s))
    (cond ((and (memq head '(define def def+ define+ defun rec+)) ; a procedure (f a b . rest) with a body
                (pair? (cdr s))
                (pair? (cadr s))
                (pair? (cddr s)))
           (header-names (cadr s)))
          #;((and (memq head '(lambda λ lambda+))
                (pair? (cdr s)))
           (formals-names (cadr s)))
          ((and (memq head '(lambda λ lambda+ lambda-x)) ; lambda+ : lambda with return , lambda-x : the lambda+ of before
                (pair? (cdr s)))
           (formals-names (cadr s)))
          ((eq? head 'case-lambda) ; the parameters of all the clauses
           (apply append (map-in-order (λ (c) (if (pair? c)
						  (formals-names (car c))
						  '()))
                                       (cdr s))))
          ((and (eq? head 'do) (do-R7RS? s)) ; the variables of the loop
           (map-in-order car (cadr s)))
          ((and (eq? head 'receive) (pair? (cdr s))) ; (receive formals expression body ...) of SRFI 8
           (formals-names (cadr s)))
          ((and (memq head '(let let* letrec letrec*))
                (pair? (cdr s)))
           (if (symbol? (cadr s)) ; named let : (let loop ((name init) ...) body ...)
               (cons (cadr s)
                     (if (pair? (cddr s))
                         (bindings-names (caddr s))
                         '()))
               (bindings-names (cadr s)))) ; (let ((name init) ...) body ...)
          ((and (memq head '(let-values let*-values)) ; (let-values (((a b) expression) ...) body ...)
                (pair? (cdr s))
                (list? (cadr s)))
           (apply append (map-in-order (λ (b) (if (pair? b) (formals-names (car b)) '()))
                                       (cadr s))))
          (else '())))

  ; the table of a new level for the list s : the names bound by s are local bindings without type
  (define (new-level-table s)
    (define ht (make-hash-table))
    (for-each (λ (name) (mark-local-binding! name ht))
              (bound-names s))
    ht)

  ; the names defined without a known type at the current level (a declaration, or a definition whose value has an
  ; unknown type) hide the outer variables of the same name , unless the name has already a type at this level
  ; (for example (type flonum? x) written before (define x ...))
  (define (mark-local-definitions! names annot-ht)
    (for-each (λ (name)
                (unless (hash-table-exists? annot-ht name)
                  (mark-local-binding! name annot-ht)))
              names))

  ; the names declared or defined by s at the current level by a form not typed by the inference :
  ; (declare x y) , (def (x y)) , (def x) , (def x e) , (define-values (a b) e)
  (define (declared-names s)
    (define head (car s))
    (cond ((eq? head 'declare)
	   (filter symbol? (cdr s)))
	  
          ((and (memq head '(def def+ define+ defun))
		(pair? (cdr s)))
           (cond ((symbol? (cadr s))
		  (list (cadr s))) ; (def x) , (def x e)
                 ((and (list? (cadr s))
		       (null? (cddr s)))
		  (filter symbol? (cadr s))) ; (def (x y)) : a declaration
                 (else '())))
	  
          ((and (eq? head 'define-values)
		(pair? (cdr s)))
           (formals-names (cadr s)))
	  
          (else '())))

  #| scan a module's body for (provide ...) forms and collect the exported names : plain symbols, and
  whether (all-defined-out) appears (which exports every top-level name of the module). Other provide-specs
  (rename-out, except-out, struct-out, all-from-out, ...) are not handled yet (TODO? probably no) : the names they would
  export are simply not propagated to the outer annotation table, nothing else is affected. |#
  (define (collect-provided-names body)
    (declare names all?)
    {names := '()}
    {all? := #f}
    (for-each (λ (f)
                (when (and (pair? f) (eq? (car f) 'provide))
                  (for-each (λ (spec)
                              (cond ((symbol? spec) {names := (cons spec names)})
                                    ((and (pair? spec)
					  (eq? (car spec) 'all-defined-out))
				     {all? := #t})
                                    (else (void)))) ; rename-out, except-out, ... : not handled yet
                            (cdr f))))
              body)
    (values names all?))

  #| copy, from a module's own annotation table (mod-ht) into the outer one (outer-ht), the type entries of
  the names legally exported by the module ((provide name ...) and/or (all-defined-out)) : mirrors, for
  static typing, exactly what Scheme already does for runtime visibility (require) ; non-exported (private)
  bindings stay confined to mod-ht, untouched, exactly as before this mechanism existed.
  with (all-defined-out), every entry of mod-ht is copied (both the direct key for a name and the
  ($bracket-apply$ name) element-type key for a typed container live as ordinary keys of the same table, so
  one walk picks up both) ; mod-ht only ever holds names bound directly at the module's top level in the
  first place (anything nested pushes its own, deeper table), so there is nothing further to filter here. |#
  (define (export-provided-types! mod-ht outer-ht names all?)
    (if all?
        (hash-table-walk mod-ht
                         (λ (k v) (unless (eq? v local-binding-without-type)
                                    {outer-ht[k] := v})))
      else
        (for-each (λ (name)
                    (when (and (hash-table-exists? mod-ht name)
                               (not (eq? {mod-ht[name]} local-binding-without-type)))
                      {outer-ht[name] := {mod-ht[name]}})
                    (declare key)
                    {key := (element-key name)}
                    (when (and key (hash-table-exists? mod-ht key))
                      {outer-ht[key] := {mod-ht[key]}}))
                  names)))

  ; the symbols of the left hand side of a definition : (c b a) or ((x y) (a b)) for multiple values
  (define (lhs-symbols L)
    (apply append (map-in-order (λ (x) (cond ((symbol? x) (list x))
                                             ((and (list? x)
						   (not (isBRACKET-APPLY*? x)))
					      (filter symbol? x))
                                             (else '())))
                                L)))

  ; the pragmas written at the top level of the file, outside a module form, stay in the code as markers too.
  ; forms : the top level forms of the file. When the code is a single (module name language body ...) form, the markers
  ; written before it are put at the beginning of its body, where the annotation pass finds them, and the ones written
  ; after it are dropped (no code follows them). Otherwise the forms will be put in a module by the caller and the
  ; markers stay between the forms.
  (define (insert-top-level-markers-in-module forms)
    {code <+ (filter (λ (f) (not (annotation-marker? f))) forms)}
    (cond ((null? code) '()) ; only pragmas , no code
          ((and (null? (cdr code))
                (pair? (car code))
                (eq? 'module (car (car code))))
           {modul <+ (car code)}
           {markers-before <+ (take-while annotation-marker? forms)}
           #;(list (append (list (first modul) (second modul) (third modul)) ; (module name language
                         markers-before
                         (cdddr modul))) ; body ...)
           (list (relink-form! (append (list (first modul) (second modul) (third modul)) ; (module name language
                                       markers-before
                                       (cdddr modul)) ; body ...)
                               modul)))
          (else forms)))


  ; DEPRECATED (désactivé, pas supprimé) : plus appelée depuis que recreate-define-overload-existing-generic
  ; (plus bas) est elle-même désactivée (voir la clause is-define-overload? désactivée dans parse-expressions) ;
  ; define-overload-existing-operator / define-overload-existing-n-ary-associative-operator sont une famille
  ; "two steps" explicitement dépréciée ("kept for compatibility but no more documented", Scheme-PLUS-for-Racket/README.md).
  ; + --> orig-+
  #;(define (create-original-symbol s)
    (string->symbol (string-append "orig-" (symbol->string s))))

  ; example (+ 1 2.0 3) -> (orig-+ 1 2.0 3)
  ; the problem was : 2.0 is flonum? and 3 is integer? so we should use fl+ instead of orig-+ but the type of 3 at parsing time could be bignum or fixnum and we should use the more powerful but it is also the slower one and we want to optimize with type annotation

  ; compute litteral expression when possible or specialize or use original scheme operator/procedure symbol (like +) and compute at runtime with type dispatch
  ; FR : voir l'analyse comparative (FR/EN/IT) au-dessus de operator-associativity-recursion-and-get-return-type-of-procedure
  ;      (plus bas dans ce fichier) — cette fonction n'est utilisée que pour le cas binaire (arité fixe 2, pas
  ;      d'associativité à gérer) ; pour le cas n-aire, la même décision précalcul/spécialisation/dispatch est
  ;      dupliquée en ligne dans operator-associativity-recursion.
  ; EN : see the comparative analysis (FR/EN/IT) above operator-associativity-recursion-and-get-return-type-of-procedure
  ;      (further down in this file) — this function is only used for the binary case (fixed arity 2, no
  ;      associativity to handle); for the n-ary case, the same literal-folding/specialization/dispatch decision
  ;      is duplicated inline inside operator-associativity-recursion.
  ; IT : vedi l'analisi comparativa (FR/EN/IT) sopra operator-associativity-recursion-and-get-return-type-of-procedure
  ;      (più in basso in questo file) — questa funzione è usata solo per il caso binario (arità fissa 2, nessuna
  ;      associatività da gestire); per il caso n-ario, la stessa decisione precalcolo/specializzazione/dispatch
  ;      è duplicata in linea dentro operator-associativity-recursion.
  ; the computation of literals at the reading : (list value) , or #f when it raises an error ({2.5 / 0} is not computed :
  ; the error happens at run time , as without the annotation) ; a list because the value can be #f ({2 < 1})
  (define (fold-literals kp argus)
    (with-handlers ((exn:fail? (λ (e) #f)))
      (list (apply kp argus))))

  (define (rewrite/compute-operational-expression proc proc-base argus argus-types) ; proc is function matching types if found (example: fl+, proc-orig is the original operator/procedure symbol (like +), argus is the list of arguments, argus-types is the list of types of arguments

    {kp <+ (known-proc-ref proc-base)} ; known-proc-ref returns the procedure bound to 'key in known-proc if it exists, #f otherwise
    {lla <+ (litteral-list? argus)} ; check if all arguments are litterals, example: (1 2.0 3) is a litteral list,  (1 "2" 3) is also a litteral list
    (display "annot+.rkt : rewrite/compute-operational-expression : kwnown procedure : kp =" err-port) (display kp err-port) (newline err-port)
    (display "annot+.rkt : rewrite/compute-operational-expression : argus is (or not) a litteral list : lla =" err-port) (display lla err-port) (newline err-port)

    {folded <+ (and proc kp lla (all-numbers? argus) (fold-literals kp argus))} ; (value) , #f if not computed
    (cond #;({proc and kp and lla and (all-numbers? argus)} ; all arguments are litterals, we can compute the result at parsing time
           (apply kp argus)) ; operator found in hash table of known procedures,example (fl+ 1.0 2.0), this is the fastest pre-runtime computation as we evaluate the litterals arguments
          (folded ; all arguments are litterals : the result computed at parsing time (not when the computation raises an error)
           (car folded))

          (proc ; specialization at parsing but computation at runtime, example: (fl+ 1.0 2.0), this is the fastest runtime computation
            (display "annot+.rkt : rewrite/compute-operational-expression : (cons proc argus) =" err-port) (display (cons proc argus) err-port) (newline err-port)
            (cons proc argus)) ; operator specialisation found in hash tables ,example (fl+ 1.0 2.0), this is the fastest runtime computation

          ; proc is #f, we have no specialization for the operator/procedure, we will use the original operator/procedure symbol (like +) and compute at runtime with type dispatch
          (else ; example: (+ 1 "2" 3) , this the most powerful computation, but the slowest one (type dispatch at run time)
            (cons proc-base argus))))


  ; DEPRECATED (désactivé, pas supprimé) : plus appelée que par du code lui-même désactivé ci-dessous ;
  ; voir la note au-dessus de create-original-symbol et la clause is-define-overload? désactivée dans
  ; parse-expressions.
  ; procedure for returning symb and orig-symb
  #;(define (return-symb-and-orig-symb s)
    (define symb (second s))
    (define orig-symb (create-original-symbol symb))
    (values symb orig-symb))


  ; (define-overload-existing-operator +) --> (define-overload-existing-operator-annot + orig-+)
  ; (define-overload-existing-operator +) or (define-overload-existing-operator · Scheme+/multiply)
  ; DEPRECATED : now generic
  #; (define (recreate-define-overload-existing-operator s) ; ovrld-ht)
       (define len-s (length s))
       (when {len-s ≠ 2 and len-s ≠ 3}
         (error "annot : recreate-define-overload-existing-operator : bad overload syntax in : " s))

       {(symb orig-symb) := (return-symb-and-orig-symb s)}

       ; create en entry for symb in the current hash table see above overload arithmetic operators with number?
       ;{ovrld-ht[symb] := (list orig-symb)} ; example: ovrld-ht (alist) : ((* orig-*))
       ; (+ → ((vector-append (vector? vector?)) orig-+))
       ;(display "annot+.rkt : recreate-define-overload-existing-operator : ovrld-ht, alist : ") (display (hash-table->alist ovrld-ht)) (newline)

       ; rewrite the parsed code (in case default overloaded procedure is used)
       (if {len-s = 2}
           `(define-overload-existing-operator-annot ,symb ,orig-symb)
         else
           (define modul (third s))
           `(define-overload-existing-operator-annot ,symb ,orig-symb ,modul)))

  ; DEPRECATED (désactivé, pas supprimé) : define-overload-existing-operator / define-overload-existing-n-ary-associative-operator
  ; sont une famille "two steps" explicitement dépréciée ("kept for compatibility but no more documented",
  ; Scheme-PLUS-for-Racket/README.md). Un fichier qui n'utilise jamais ANNOTATION-ON n'est jamais affecté par
  ; ce retrait (get-annot-on-read, SRFI-105-curly-infix.rkt, n'ouvre annot que si ANNOTATION-ON est présent) ;
  ; seul un fichier qui mélangerait ANNOTATION-ON et ces macros dépréciées perdrait la réécriture ci-dessous.
  ; plus appelée : voir la clause is-define-overload? désactivée plus bas dans parse-expressions.
  ; (define-overload-existing-operator +) --> (define-overload-existing-operator-annot + orig-+)
  ;  generic with symbol creation (AI generated !)
  ; (define-overload-existing-operator +) or (define-overload-existing-operator · Scheme+/multiply)
  #;(define (recreate-define-overload-existing-generic s) ; ovrld-ht)
    (define len-s (length s))
    (when {len-s ≠ 2 and len-s ≠ 3}
      (error "annot : recreate-define-overload-existing-generic : bad overload syntax in : " s))

    (define tag (first s))
    (define tag-annot (string->symbol (string-append (symbol->string tag) "-annot")))
    {(symb orig-symb) := (return-symb-and-orig-symb s)}

    ; create en entry for symb in the current hash table see above overload arithmetic operators with number?
    ;{ovrld-ht[symb] := (list orig-symb)} ; example: ovrld-ht (alist) : ((* orig-*))
    ; (+ → ((vector-append (vector? vector?)) orig-+))
    ;(display "annot+.rkt : recreate-define-overload-existing-generic : ovrld-ht, alist : ") (display (hash-table->alist ovrld-ht)) (newline)

    ; rewrite the parsed code (in case default overloaded procedure is used)
    (if {len-s = 2}
        `(,tag-annot ,symb ,orig-symb)
      else
        {modul := (third s)}
        `(,tag-annot ,symb ,orig-symb ,modul)))


  ; store the overload data of the operator/procedure in the hash table
  ; example: (overload-existing-operator + vector-append (vector? vector?))
  (define (process-overload s ovrld-ht)
    ;(error "process-overload reached !")
    (define len-s (length s))
    (when {len-s ≠ 4 and len-s ≠ 5} ; could exist case with more than 4 values (5 it seems, example : (overload-existing-operator + (vector?) vector-append (vector? vector?))) when we also have a return value type => this should be the norm with annotation type
      (display "len-s=") (display len-s) (newline)
      (error "annot : process-overload : bad overload syntax in : " s))

    {op <+ (cadr s)} ; operator or function, procedure, example: +
    {new-ovrld <+ (map (λ (e) (if (pair? e) (canonical-type e) e)) ; the lists of predicates : ℕ ℤ ℝ --> fixnum? flonum?
                       (cddr s))} ; new overload , example: ((vector?) vector-append (vector? vector?))  or  (vector-append (vector? vector?))
    {op-ovrld-lst <+ '()}
    (if (hash-table-exists? ovrld-ht op) ; when not existing? we must have created? it before when parsing define-overload-procedure/operator
        {op-ovrld-lst := ovrld-ht[op]} ; retrieve list of the overloaded
      else
        {ovrld-ht[op] := '()}) ; create one entry in hash table
    {updated-op-ovrld-lst <+ (cons new-ovrld op-ovrld-lst)} ; update overload list
    {ovrld-ht[op] := updated-op-ovrld-lst} ; update the hash table
    
    ; create entry also for the overload operator itself in case of parsing then the annotation parser can know the type also of the already parsed expressions (important in operator-associativity-recursion at least!
    ; we must extract the procedure name
    {len-new-ovrld <+ (length new-ovrld)}
    (declare proc-name) ; procedure name
    (if {len-new-ovrld = 3}
        {proc-name := (cadr new-ovrld)} ; ((vector?) vector-append (vector? vector?))
        {proc-name := (car new-ovrld)}) ; (vector-append (vector? vector?))
    {op-ovrld-lst := '()}
    (if (hash-table-exists? ovrld-ht proc-name) ; when not existing? we must have created? it before when parsing define-overload-procedure/operator
        {op-ovrld-lst := ovrld-ht[proc-name]} ; retrieve list of the overloaded
      else
        {ovrld-ht[proc-name] := '()}) ; create one entry in hash table
    {updated-op-ovrld-lst := (cons new-ovrld op-ovrld-lst)} ; update overload list
    {ovrld-ht[proc-name] := updated-op-ovrld-lst})


  ; overload-operator (Scheme+/overload) defines the overloaded operator if needed and adds an
  ; overload to it, n-ary with one predicate, binary with two :
  ; (overload-operator + string-append (string?))
  ; (overload-operator + (vector?) vector-append (vector? vector?))
  ; (overload-operator · Scheme+/multiply multiply-matrix-matrix (matrix-vect? matrix-vect?))
  ; (overload-operator · Scheme+/multiply (matrix-vect?) multiply-matrix-matrix (matrix-vect? matrix-vect?))
  ; The module name is dropped (not needed here) and the overload is stored by process-overload
  ; in the n-ary or the binary table according to the number of predicates, not always in the
  ; binary one as for overload-existing-operator.
  ; for overload-operator / overload-procedure and their require-* forms :
  ; (op-macro op [module-name] [(ret-pred ...)] funct (pred ...)) -> ([(ret-pred ...)] funct (pred ...))
  ; the module name is dropped, not needed here
  (define (overload-without-module-name s)

    (define args (cddr s)) ; everything after the operator
    (define len-args (length args))

    ; same rule as the overload-operator macro in overload.rkt : a module name is an identifier,
    ; a string or a require spec such as (except-in Scheme+/multiply ×) ; any other list is the
    ; list of return values
    (define (module-name? x)
      (or (symbol? x)
          (string? x)
          (and (pair? x)
               (memq (car x) '(only-in except-in prefix-in rename-in combine-in relative-in only-meta-in only-space-in
                               for-syntax for-template for-label for-meta for-space submod file lib planet quote))
               #t)))

    ; the overload without the module name : (funct (pred ...)) or ((ret-pred ...) funct (pred ...))
    (cond ({len-args = 2} args)
          ((and {len-args = 3}
		(module-name? (car args)))
	   (cdr args))
          ({len-args = 3} args)
          ({len-args = 4} (cdr args))
          (else (error "annot : overload-without-module-name : bad overload syntax in : " s))))


  ; overload-unary-operator (unary? #t) : one predicate , but exactly one argument , stored with the binary overloads in the table
  ; of the overloads with an exact number of arguments : (overload-unary-operator - (mat?) mneg (mat?))
  (define* (process-overload-operator s ovrld-ht ovrld-ht-n-ary #:optional (unary? #f))

    (define ovrld (overload-without-module-name s))

    (define nb-preds (length (car (reverse ovrld))))

    (when (and unary? {nb-preds ≠ 1})
      (error "annot : process-overload-operator : expected 1 predicate (unary overload) in : " s))

    (process-overload (cons (car s) (cons (cadr s) ovrld)) ; (overload-operator op . ovrld) as process-overload expects it
                      (cond (unary? ovrld-ht)
                            ({nb-preds = 1} ovrld-ht-n-ary)
                            ({nb-preds = 2} ovrld-ht)
                            (else (error "annot : process-overload-operator : expected 1 predicate (n-ary overload) or 2 predicates (binary overload) in : " s)))))

  ; overload-procedure (Scheme+/overload) : like overload-operator but a procedure has only one
  ; table, whatever the number of predicates :
  ; (overload-procedure area area-rect (number? number?))
  ; (overload-procedure uniform (flonum?) uniform-interval (flonum? flonum?))
  (define (process-overload-procedure s ovrld-ht-proc)
    (process-overload (cons (car s) (cons (cadr s) (overload-without-module-name s))) ; (overload-procedure proc . ovrld) as process-overload expects it
                      ovrld-ht-proc))


  ; overload-square-brackets (Scheme+/overload) and require-overload-square-brackets (same form, in a module importing
  ; the object) : (overload-square-brackets [(return-type)] getter setter (pred-obj pred-index ...))
  ; examples : (overload-square-brackets matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
  ;            (overload-square-brackets (flonum?) matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))

  ; the overloads of the square brackets known by the annotation (as $ovrld-square-brackets-lst$ in Scheme+/overload) :
  ; ((getter setter pred-obj nb-indexes return-type) ...) , return-type is #f when not given ,
  ; example : ((matrix-vect-ref matrix-vect-set! matrix-vect? 2 flonum?))
  ; emptied by init-annot-data at the beginning of each file (and once at the start of the REPL)
  (define $ovrld-square-brackets-lst$ '())

  (define (reset-$ovrld-square-brackets-lst$)
    {$ovrld-square-brackets-lst$ := '()})

  ; store the overload (getter setter pred-obj nb-indexes return-type) , once :
  ; (overload-square-brackets (flonum?) matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
  ; --> (matrix-vect-ref matrix-vect-set! matrix-vect? 2 flonum?)
  (define (process-overload-square-brackets s)
    (define args (cdr s)) ; ([(return-type)] getter setter (pred-obj pred-index ...))
    (unless (and (or {(length args) = 3}
                     (and {(length args) = 4} (list? (car args))))
                 (pair? (last args)))
      (error "annot : process-overload-square-brackets : bad syntax, expected (overload-square-brackets [(return-type)] getter setter (pred-obj pred-index ...)) in : " s))
    (define return-type (canonical-type (and {(length args) = 4} ; ℕ ℤ ℝ --> fixnum? flonum?
                                             (pair? (car args))
                                             (car (car args)))))
    (define overload (if {(length args) = 4} (cdr args) args)) ; (getter setter (pred-obj pred-index ...))
    (define preds (canonical-type (third overload)))
    (define new-ovrld (list (first overload) (second overload) (car preds) (length (cdr preds)) return-type))
    ; overload-square-brackets and require-overload-square-brackets of the same overload in a file : stored once , with
    ; the return type if one of them gives it
    (define (same-overload? o) (equal? (take o 4) (take new-ovrld 4)))
    (define old-ovrld (find same-overload? $ovrld-square-brackets-lst$))
    {$ovrld-square-brackets-lst$ := (cons (if (old-ovrld and not return-type) #;(and old-ovrld (not return-type))
                                              old-ovrld
                                              new-ovrld)
                                          (filter (λ (o) (not (same-overload? o))) $ovrld-square-brackets-lst$))})

  ; the number of indexes of an access ($bracket-apply$ object token ...) , as the square brackets macros cut them ,
  ; #f for a slice : {M[i j]} --> 2 , {M[k (j + 1)]} --> 2 , {M[i + 1]} --> 1 , {v[1 : 3]} --> #f
  ; the kind of the indexes of an access s = ($bracket-apply$ object token ...) to a vector or a string , for the
  ; accesses specialized by the type of the indexes : 'natural when they are all of type ℕ (natural-fixnum?) or literal
  ; fixnums >= 0 , 'integer when they are all ℕ or ℤ (fixnum?) or literal fixnums , #f otherwise (a slice , an index of
  ; unknown type) ; {v[i]} with (type ℕ i) --> natural , {v[i + 1]} --> integer (the type of (+ i 1) is fixnum?)
  ; the sum and the product of naturals are naturals (recursively) : {v[2 * (i + 2) + 1]} with (type ℕ i) --> natural ;
  ; this rule is used only for the indexes , the type of (+ i 1) stays fixnum? elsewhere
  (define (indexes-kind s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define indexes (parse-square-brackets-arguments-lister-syntax (cddr s) '()))
    (define (kind-by-type index) ; by the type of the expression : ℕ , ℤ or unknown
      {t <+ (find-type index ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
      (cond #;((eq? t 'natural-fixnum?) 'natural)
            ((memq t '(natural-fixnum? positive-fixnum?)) 'natural)
            #;((eq? t 'fixnum?) 'integer)
            ((memq t '(fixnum? nonzero-fixnum?)) 'integer)
            (else #f)))
    (define (kinds-of-operands kinds) ; all naturals --> natural , all naturals or integers --> integer
      (cond ((memq #f kinds) #f)
            ((every (λ (k) (eq? k 'natural)) kinds) 'natural)
            (else 'integer)))
    (define (kind index)
      (cond ((fixnum? index) (if {index >= 0} 'natural 'integer)) ; a literal
            ((and (pair? index) (not (operator? (car index)))) ; an infix expression alone between parenthesis : (j + 1)
             {parsed <+ (parse-square-brackets-arguments-lister-syntax index '())}
             (if (and (pair? parsed) (null? (cdr parsed)) (not (equal? (car parsed) index)))
                 (kind (car parsed))
                 (kind-by-type index)))
            ((and (pair? index) (pair? (cdr index))
                  (or (ADD-op? (car index)) (MULTIPLY-op? (car index)))) ; ℕ + ℕ , ℕ × ℕ are naturals
             (kinds-of-operands (map kind (cdr index))))
            (else (kind-by-type index))))
    (and (pair? indexes)
         (not (memq ': indexes))
         (kinds-of-operands (map kind indexes))))

  ; M[i j] and M[i][j] are the same access to a vector of vectors : the type of an access s with n >= 2 indexes is the
  ; type of the nested access (only for the type , the code is not changed) , #f with one index , a slice or an object
  ; with overloaded square brackets : {M[0 1]} with (type #(#(flonum?)) M) --> flonum? (#(flonum?) before : one level)
  (define (multi-indexes-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define indexes (parse-square-brackets-arguments-lister-syntax (cddr s) '()))
    (and (pair? indexes)
         (pair? (cdr indexes))
         (not (memq ': indexes))
         (not (overloaded-square-brackets-access? s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
         (find-type (foldl (λ (index access) (list '$bracket-apply$ access index)) ; M[i j] --> M[i][j]
                           (cadr s)
                           indexes)
                    ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))

  (define (square-brackets-nb-indexes s)
    (define indexes (parse-square-brackets-arguments-lister-syntax (cddr s) '()))
    (if (memq ': indexes)
        #f
        (length indexes)))

  ; the type returned by the getter of the overload of the square brackets of the object of the access s , for its
  ; number of indexes , when the overload gives it : {Mv[i j]} --> flonum? with
  ; (overload-square-brackets (flonum?) matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?)) , otherwise #f
  (define (overloaded-square-brackets-return-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define ovrlds (overloaded-square-brackets-for (type-base (find-type (cadr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))))
    (define nb (and (pair? ovrlds)
                    (square-brackets-nb-indexes s)))
    (define same-nb (if nb
                        (filter (λ (o) (equal? (cadddr o) nb)) ovrlds)
                        '()))
    (and {(length same-nb) = 1}
         (list-ref (car same-nb) 4)))

  ; the type of an access s to a string : {s[i]} --> char? , a slice {s[i : j]} --> string? , otherwise #f
  (define (string-access-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (and (eq? (type-base (find-type (cadr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)) 'string?)
         ($+> {nb <+ (square-brackets-nb-indexes s)}
              (cond ((not nb) 'string?)
                    ({nb = 1} 'char?)
                    (else #f)))))

  ; the overloads of the square brackets for an object of type tip : those whose predicate of the object matches
  (define (overloaded-square-brackets-for tip)
    (if (and tip
             (not (eq? tip 'sexpr)))
        (filter (λ (ovrld) (type-match? tip (third ovrld)))
                $ovrld-square-brackets-lst$)
        '()))

  ; x is an access {C[...]} to an object C whose square brackets are overloaded : C[i] and C[i j] are then different
  ; things (a line , an element ...) and the type of C[] can not be stored or used as for a vector
  (define (overloaded-square-brackets-access? x ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (and (isBRACKET-APPLY*? x)
         (pair? (overloaded-square-brackets-for (type-base (find-type (cadr x) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))))))

  ; old versions : the overloads were stored in the table of the procedures , under the key overload-square-brackets
  #;(define (process-overload-square-brackets s ovrld-ht-proc)
    (unless (and {(length s) = 4}
                 (pair? (cadddr s)))
      (error "annot : process-overload-square-brackets : bad syntax, expected (overload-square-brackets getter setter (pred-obj pred-index ...)) in : " s))
    (define preds (cadddr s))
    (define new-ovrld (list (second s) (third s) (car preds) (length (cdr preds))))
    (define ovrlds (hash-table-ref/default ovrld-ht-proc 'overload-square-brackets '()))
    (unless (member new-ovrld ovrlds) ; overload-square-brackets and require-overload-square-brackets of the same overload in a file
      (hash-table-set! ovrld-ht-proc 'overload-square-brackets (cons new-ovrld ovrlds))))

  #;(define (overloaded-square-brackets-for tip ovrld-ht-proc)
    (if (and tip
             (not (eq? tip 'sexpr)))
        (filter (λ (ovrld) (type-match? tip (third ovrld)))
                (hash-table-ref/default ovrld-ht-proc 'overload-square-brackets '()))
        '()))


  ; (rational-strict? 3.0)
  ; #f
  #; (define (rational-strict? s)
       {(exact? s) and (rational? s)})

  (define (check-exist-and-get-value-in-hash-table ht s)
    {(hash-table-exists? ht s) and ht[s]})
    
  ; search expression in annotations hash tables hierarchy (first is more nested lexical context)
  ; example: search {z[i]} and return type , not the full declaration
  ; so for ((inexact?) procedure? (inexact? inexact?)) this will return inexact?
  (define (search-in-annot-hash-tables-list-the-type-of-return s annot-ht-lst)
    ; note there could be multiple annotation for a single expression but the first should be the good one as we 'cons the hash tables in the list
    #;{decl := (any (λ (ht) (check-exist-and-get-value-in-hash-table ht s)) ; we must check first that expression exists in hash tables unless we get an error
                    annot-ht-lst)}
    {decl <+ (search-annot-hash-tables s annot-ht-lst)} ; the most nested level that knows s , a local binding hides the outer levels
    (if (and (list? decl) (not (list-type? decl))) ; something like: ((inexact?) procedure? (inexact?)) ; a list type (T) is a type, not a signature
      then
        (unless (and {(length decl) = 3}
                     (eq? (second decl) 'procedure?))
          (error "annot+ : search-in-annot-hash-tables-list : list but not a domain/codomain function declaration, see : " decl))
        (first (first decl)) ; return first return value type
      else decl)) ; return simple type (ex: inexact?)

  ; UNUSED:
  ; when searching if f is a procedure?
  (define (search-in-annot-hash-tables-list-the-type s annot-ht-lst)
    ; note there could be multiple annotation for a single expression but the first should be the good one as we 'cons the hash tables in the list
    #;{decl := (any (λ (ht) (check-exist-and-get-value-in-hash-table ht s)) ; we must check first that expression exists in hash tables unless we get an error
                    annot-ht-lst)}
    {decl <+ (search-annot-hash-tables s annot-ht-lst)} ; the most nested level that knows s , a local binding hides the outer levels
    (if (and (list? decl) (not (list-type? decl))) ; something like: ((inexact?) procedure? (inexact?)) ; a list type (T) is a type, not a signature
      then
        (unless (and {(length decl) = 3}
                     (eq? (second decl) 'procedure?))
          (error "annot+ : search-in-annot-hash-tables-list-the-type : list but not a domain/codomain function declaration, see : " decl))
        (second decl) ; return the type
      else decl)) ; just a symbol type (ex: inexact?)

  ; when procedure return multiple values
  (define (search-in-annot-hash-tables-list-the-type-of-return-values s annot-ht-lst)
    ; note there could be multiple annotation for a single expression but the first should be the good one as we 'cons the hash tables in the list
    #;{decl := (any (λ (ht) (check-exist-and-get-value-in-hash-table ht s)) ; we must check first that expression exists in hash tables unless we get an error
                    annot-ht-lst)}
    {decl <+ (search-annot-hash-tables s annot-ht-lst)} ; the most nested level that knows s , a local binding hides the outer levels
    (if (and (list? decl) (not (list-type? decl))) ; something like: ((inexact?) procedure? (inexact?)) ; a list type (T) is a type, not a signature
      then
        (unless (and {(length decl) = 3}
                     (eq? (second decl) 'procedure?))
          (error "annot+ : search-in-annot-hash-tables-list-the-type-of-values : list but not a domain/codomain function declaration, see : " decl))
        (first decl) ; return all the return value types
      else decl)) ; just a symbol type (ex: inexact?)


  ; {s := 'T[i][j]}
  ; (remove-indexes s)
  ; '($bracket-apply$ ($bracket-apply$ T))
  ;
  ; (remove-indexes {'T[i][j][k]})
  ; '($bracket-apply$ ($bracket-apply$ ($bracket-apply$ T)))
  ; Warning: argument must be of the form ($bracket-apply....)
  (define (remove-indexes s)
    {T <+ (cadr s)}
    (if (isBRACKET-APPLY*? T) ; recurse ,list was supposed to be bracket-apply now checked,case container is returned by a function! or something else
        `{(unquote (remove-indexes (cadr s)))[]} ; pretty curly-infix/prefix syntax ?
        ; we add[] because we are allready in the case s is of the form ($bracket-apply....)
        `{(unquote (cadr s))[]})) #| or (list (first s) (second s)) , search for example: '($bracket-apply$ T)
                                    i'm force to use 'unquote' and not just ',' because i just want to unquote
                                    (cadr s) but parser is greedy and would eat also the[]
                                    also works for {T[][]...[]} as result |#


  #| composite types : a homogeneous container is typed with the type of its elements :
       #(T) : vector of elements of type T       example: (type #(flonum?) v1 v2)
       (T)  : list of elements of type T         example: (type (flonum?) L)
       #()  : a vector, the type of its elements not given (the same as vector?) , example: (type #() w)
     composable : #(#(flonum?)) is a vector of vectors of flonums.
     No ambiguity with a procedure signature ((ret ...) procedure? (param ...)) : 3 elements,
     and its return part is always the list of the return types, ex: ((flonum?)) returns a list of flonums.
     The bracket notation stays allowed and equivalent : (type vector? z {z[]}) (type flonum? {z[][]})
     is the same as (type #(#(flonum?)) z). |#

  (define (vector-type? t) ; #(T)
    (and (vector? t) {(vector-length t) = 1}))

  (define (list-type? t) ; (T)
    (declare e)
    (and (pair? t) (null? (cdr t))
         {e := (car t)}
         (or (symbol? e) (vector-type? e) (list-type? e))))

  (define (composite-type? t)
    (or (vector-type? t) (list-type? t)))

  ; the type of the container : #(T) -> vector? , (T) -> list? , otherwise t itself
  (define (type-base t)
    (cond ((vector-type? t) 'vector?)
          ((list-type? t) 'list?)
          #;((eq? t 'natural-fixnum?) 'fixnum?) ; ℕ : a fixnum for the overloads , {i + 1} --> (fx+ i 1)
          ; the types of a domain (Scheme+/type) : ℕ is a fixnum for the overloads , {i + 1} --> (fx+ i 1) , ℝ⁺ a flonum
          ((domain-type-base t)) ; its base type , #f for the other types
          (else t)))

  ; the type of the elements of a composite type, or #f
  (define (composite-element-type t)
    (cond ((vector-type? t) (vector-ref t 0))
          ((list-type? t) (car t))
          (else #f)))

  ; an argument type matches a parameter type if they are equal, or if the parameter is a simple type equal to the
  ; container type of the argument : #(flonum?) matches vector? (all the existing overloads on vector? keep working)
  (define (type-match? arg-type param-type)
    (or (equal? arg-type param-type)
        (and (symbol? param-type)
             #;(equal? (type-base arg-type) param-type)
             (or (equal? (type-base arg-type) param-type)
                 (memq param-type (domain-type-parents arg-type))))))

  (define (types-match? arg-types param-types)
    (and (list? arg-types) (list? param-types)
         {(length arg-types) = (length param-types)}
         (every type-match? arg-types param-types)))

  ; the key under which the type of the elements of expr is stored with the bracket notation :
  ; v -> ($bracket-apply$ v) ,  {z[i]} -> ($bracket-apply$ ($bracket-apply$ z)) (the element of z[] is z[][])
  (define (element-key expr)
    (cond ((symbol? expr) (list '$bracket-apply$ expr))
          ((isBRACKET-APPLY*? expr) (list '$bracket-apply$ (remove-indexes expr)))
          (else #f)))

  ; the type of the elements of expr : from its composite type, or else from the bracket notation annotation, or #f
  (define (element-type expr ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define t (find-type expr ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define key (element-key expr))
    (or (composite-element-type t)
        (and key
             (search-in-annot-hash-tables-list-the-type key annot-ht-lst))))

  ; with a composite type t for expr, store also the type of its elements with the bracket notation, recursively,
  ; so that {v[i]} is typed the same way with (type #(flonum?) v) as with (type flonum? {v[]})
  (define (store-element-types t expr annot-ht)
    (define elt (composite-element-type t))
    (define key (element-key expr))
    (when (and elt key)
      {annot-ht[key] := elt}
      (store-element-types elt key annot-ht)))

  ; the type of a literal vector : #(T) when all its elements are literal data of the same type, otherwise vector?
  ; (only self-evaluating data : the elements of a literal vector are not evaluated, a symbol in it is not a variable)
  (define (literal-vector-type v)
    (define (literal-type e)
      (cond ((fixnum? e) 'fixnum?)
            ((exact-integer? e) 'exact-integer?)
            ((flonum? e) 'flonum?)
            ((string? e) 'string?)
            ((boolean? e) 'boolean?)
            ((vector? e) (literal-vector-type e))
            (else #f)))
    (define types (map-in-order literal-type (vector->list v)))
    (if (and (pair? types)
             (car types)
             (every (λ (t) (equal? t (car types))) types))
        (vector (car types))
        'vector?))

  ; in an assignment/definition, is the type of the right hand side the list of the types of multiple values
  ; (from values) rather than a list type (T) ? multiple values when it is not a list type, or when the targets are
  ; lists of variables : {(a b) <- (values 1 2)}
  (define (multiple-values-type? t LHS)
    (and (list? t)
         (or (not (list-type? t))
             (every list? LHS))))

  #| return type of a callable f (a symbol) applied to arguments of the given types, from what is already known :
     overloaded operator (n-ary and binary tables, left associativity), overloaded procedure, user annotated
     signature, scheme-ht ; #f if unknown (lambda, ...). n-ary-only? : only the n-ary overloads (when the number of
     arguments is unknown, as with apply) |#
  (define (return-type-of-callable f types n-ary-only? ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (cond ((not (symbol? f)) #f)
          ((or (hash-table-exists? ovrld-ht-n-ary f)
               (and (not n-ary-only?) (hash-table-exists? ovrld-ht f)))
           (operator-associativity-recursion-and-get-return-type-of-procedure f types
                                                                              (and (hash-table-exists? ovrld-ht-n-ary f)
										   {ovrld-ht-n-ary[f]})
                                                                              (and (not n-ary-only?)
										   (hash-table-exists? ovrld-ht f)
										   {ovrld-ht[f]})))
          (n-ary-only? #f) ; procedures have a fixed number of parameters : unknown with apply
          ((hash-table-exists? ovrld-ht-proc f)
           (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure {ovrld-ht-proc[f]} types))
          (else
           (or (search-in-annot-hash-tables-list-the-type-of-return f annot-ht-lst)
               #;(and (hash-table-exists? scheme-ht f)
                    (caar {scheme-ht[f]}))
               (scheme-return-type f)))))

  ; the rule of a list or vector procedure : container-procedures-types of scheme-types.rkt (R7RS and SRFI 1) , then
  ; racket-container-procedures-types of racket-types.rkt (Racket only) , #f if none
  (define (container-entry proc/mac)
    (or (assq proc/mac container-procedures-types)
        (assq proc/mac racket-container-procedures-types)))

  #| typing rules of higher order and container procedures : the type of the result depends on the types of the
     elements and on the procedure passed, not only on a fixed signature from scheme-types (vector-map -> vector?) :
       (vector-map f v1 ... vn) -> #(R) , R return type of f on the element types of v1 ... vn ; (map f l ...) -> (R)
       (vector->list v) -> (E) , (list->vector l) -> #(E)
       (vector-ref v i) -> E , (list-ref l i) -> E , E element type of v/l (symmetric to vector->list/list->vector)
       the procedures of container-procedures-types (scheme-types.rkt) and racket-container-procedures-types (racket-types.rkt) : (car l) (first l) (last l) (vector-ref v i) -> E ,
       (cdr l) (reverse l) (filter p l) (append l ...) (vector-copy v) (sort l <) -> (E) or #(E) ,
       (make-vector n x) -> #(T) , (make-list n x) -> (T) , T type of x ,
       (build-vector n f) -> #(R) , (build-list n f) -> (R) , R return type of f on an index ℕ
       (vector a ...) -> #(T) , (list a ...) -> (T) when all the arguments have the same known type T
       (apply f a ... l) -> return type of f on (type(a) ... E E), n-ary overloads only (length of l unknown)
     #f when a type is missing (then find-type goes on as before) |#
  (define (higher-order-type proc/mac argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define (ft a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (et a) (element-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (known? t) (and t (not (eq? t 'sexpr))))
    (define (rt f types n-ary-only?) (return-type-of-callable f types n-ary-only? ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (declare e elts f r types rev fixed) ; shared local variables used by the clauses below (:= acts as set!, no let needed)
    (define nb (length argus))

    ; the rules of the table container-procedures-types of scheme-types.rkt , entry : (name rule position container)
    (define (container-procedure-type entry)
      (declare e r f types)
      (define rule (second entry)) ; element , same , fill , build , fold or reduce
      (define pos (third entry)) ; position of the container (of the procedure for build) , all : every argument
      (define container (cadddr entry)) ; list or vector
      (define (wrap t) (if (eq? container 'vector) (vector t) (list t)))
      ; the type of the elements of a , #f if a is known to be another container ((last v) of a vector v)
      (define (elements-of a)
        (define t (ft a))
        (and (or (not (known? t)) ; container type unknown : only the bracket notation {L[]}
                 (eq? (type-base t) (if (eq? container 'vector) 'vector? 'list?)))
             (et a)))
      (cond ((eq? pos 'all) ; append , vector-append : the same type of elements for all the arguments
             (and {nb ≥ 1}
                  {types := (map-in-order elements-of argus)}
                  (every known? types)
                  (every (λ (t) (equal? t (car types))) types)
                  (wrap (car types))))
            ({nb ≤ pos} #f)
            ((eq? rule 'element) ; car , first , last , vector-ref ... : an element
             (and {e := (elements-of (list-ref argus pos))}
                  (known? e)
                  e))
            ((eq? rule 'same) ; cdr , reverse , filter , vector-copy ... : a container of the same elements
             (and {e := (elements-of (list-ref argus pos))}
                  (known? e)
                  (wrap e)))
            ((eq? rule 'fill) ; make-list , make-vector : the type of the fill value
             (and {e := (ft (list-ref argus pos))}
                  (known? e)
                  (wrap e)))
            ((eq? rule 'build) ; build-list , build-vector : the values of the procedure on an index ℕ
             (and {f := (list-ref argus pos)}
                  {r := (if (lambda-form? f)
                            (and (list? (cadr f))
                                 {(length (cadr f)) = 1}
                                 (lambda-return-type f '(natural-fixnum?) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                            (rt f '(natural-fixnum?) #f))}
                  (known? r)
                  (wrap r)))
            ((memq rule '(fold reduce)) ; foldl , foldr , fold , fold-right , reduce , reduce-right
             (and {types := (fold-procedure-types proc/mac argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                  types ; := does not return the value
                  {f := (car argus)}
                  {r := (if (lambda-form? f)
                            (and {(length (cadr f)) = (length (car types))}
                                 (lambda-return-type f (car types) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                            (rt f (car types) #f))}
                  (equal? r (cdr types)) ; f returns the type of the accumulator : it keeps its type
                  (or (eq? rule 'fold)
                      (equal? (ft (list-ref argus pos)) r)) ; reduce : ridentity , the result for an empty list
                  r))
            (else #f)))
    (cond ((memq proc/mac '(vector-map map))
           (and {nb ≥ 2}
                {elts := (map-in-order et (cdr argus))}
                {f := (car argus)}
                {r := (if (lambda-form? f)
                          (and {(length (cadr f)) = (length elts)} ; lambda : type of its body with its parameters typed by the elements
                               (lambda-return-type f elts ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                          (and (every known? elts)
                               (rt f elts #f)))}
                (and (known? r)
                     (if (eq? proc/mac 'vector-map) (vector r) (list r)))))
          ((memq proc/mac '(vector->list list->vector))
           (and {nb = 1}
                {e := (et (car argus))}
                (and (known? e)
                     (if (eq? proc/mac 'vector->list) (list e) (vector e)))))
          ; car , cdr , filter , append , build-vector ... (vector-ref and list-ref are now in this table)
          #;((assq proc/mac container-procedures-types) => container-procedure-type)
          ((container-entry proc/mac) => container-procedure-type)
          #;((memq proc/mac '(vector-ref list-ref))
           (and {nb = 2}
                {e := (et (car argus))}
                (known? e)
                e))
          ((memq proc/mac '(vector list))
           (and {nb ≥ 1}
                {types := (map-in-order ft argus)}
                (and (every known? types)
                     (every (λ (t) (equal? t (car types))) types)
                     (if (eq? proc/mac 'vector) (vector (car types)) (list (car types))))))
          ((eq? proc/mac 'apply)
           (and {nb ≥ 2}
                {rev := (reverse (cdr argus))}
                {e := (et (car rev))} ; elements of the last argument, the list
                {fixed := (map-in-order ft (reverse (cdr rev)))} ; the arguments before the list
                {types := (append fixed (list e e))}
                (and (every known? types)
                     {r := (rt (car argus) types #t)}
                     (known? r)
                     r)))
          (else #f)))

  #| the procedure f passed to a fold or a reduce (container-procedures-types of scheme-types.rkt) : the types of its
     arguments and the type its return must have , (param-types . return-type) , #f if a type is unknown
       (foldl f init l1 ... ln) , foldr , fold , fold-right : f on (E1 ... En A) must return A , A type of init
       (reduce f ridentity l) , reduce-right : f on (E E) must return E
     f must return the type of the accumulator : otherwise the accumulator would change of type after the first call ,
     and f specialized for the first call would fail at the second |#
  (define (fold-procedure-types proc/mac argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (declare a elts e)
    #;(define entry (assq proc/mac container-procedures-types))
    (define entry (container-entry proc/mac))
    (define (known? t) (and t (not (eq? t 'sexpr))))
    (define (et l) (element-type l ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (and entry
         (memq (second entry) '(fold reduce))
         {(length argus) ≥ (third entry) + 2} ; the procedure , ... , the initial value , a list at least
         (if (eq? (second entry) 'fold)
             (and {a := (find-type (list-ref argus (third entry)) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                  (known? a)
                  {elts := (map-in-order et (list-tail argus {(third entry) + 1}))}
                  (every known? elts)
                  (cons (append elts (list a)) a))
             (and {(length argus) = (third entry) + 2}
                  {e := (et (last argus))}
                  (known? e)
                  (cons (list e e) e)))))

  ; the procedures of container-procedures-types that call a procedure passed : folds , reduces , build-list , build-vector
  (define (container-higher-order? proc/mac)
    #;(define entry (assq proc/mac container-procedures-types))
    (define entry (container-entry proc/mac))
    (and entry
         (memq (second entry) '(fold reduce build))
         #t))

  #| specialization, at parsing time, of the procedure passed to vector-map, map or apply, when it is an overloaded
     operator/procedure and the types of the elements are known :
       (vector-map * v1 v2) with flonum elements -> (vector-map fl* v1 v2)
       (apply + (vector->list ...)) with flonum elements -> (apply fl+ ...) (n-ary overloads only, the length of the list is unknown)
     returns the rewritten expression, or #f (then the expression is parsed as before, the procedure is dispatched at runtime) |#
  (define (specialize-higher-order s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (declare fpt)
    (define proc/mac (car s))
    (define argus (cdr s))
    (define (ft a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (et a) (element-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (known? t) (and t (not (eq? t 'sexpr))))
    (define f (and (pair? argus) (car argus)))
    (define (n-ary-spec types) (and (hash-table-exists? ovrld-ht-n-ary f)
                                    (find-if-exists-matching-types-with-ovrld-n-ary {ovrld-ht-n-ary[f]} types)))
    (define spec
      (and {(length argus) ≥ 2}
           (symbol? f)
           (cond ((memq proc/mac '(vector-map map)) ; f is called with one element of each sequence
                  {elts <+ (map-in-order et (cdr argus))}
                  (and (every known? elts)
                       (or (n-ary-spec elts)
                           (and {(length elts) = 2}
                                (hash-table-exists? ovrld-ht f)
                                (find-if-exists-matching-types-with-ovrld {ovrld-ht[f]} elts))
                           (and (hash-table-exists? ovrld-ht-proc f)
                                (find-if-exists-matching-types-with-ovrld {ovrld-ht-proc[f]} elts)))))
                 ((eq? proc/mac 'apply) ; the number of arguments is unknown : n-ary overloads only
                  {rev <+ (reverse (cdr argus))}
                  {e <+ (et (car rev))}
                  {fixed <+ (map-in-order ft (reverse (cdr rev)))}
                  {types <+ (append fixed (list e e))}
                  (and (every known? types)
                       (n-ary-spec types)))
                 ; foldl , foldr , fold , fold-right , reduce , reduce-right : f is called on the elements and the
                 ; accumulator , specialized only when it returns the type of the accumulator
                 ((and {fpt := (fold-procedure-types proc/mac argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                       fpt) ; := does not return the value
                  (and (equal? (return-type-of-callable f (car fpt) #f ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                               (cdr fpt))
                       (or (n-ary-spec (car fpt))
                           (and {(length (car fpt)) = 2}
                                (hash-table-exists? ovrld-ht f)
                                (find-if-exists-matching-types-with-ovrld {ovrld-ht[f]} (car fpt)))
                           (and (hash-table-exists? ovrld-ht-proc f)
                                (find-if-exists-matching-types-with-ovrld {ovrld-ht-proc[f]} (car fpt))))))
                 (else #f))))
    (and spec
         (not (equal? spec f))
         (cons proc/mac
               (cons spec
                     (recall-annot (cdr argus) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))))

  #| lambdas passed to vector-map, map, for-each, vector-for-each : no special syntax to type a lambda, its
     parameters are typed by the context (the types of the elements of the sequences) or by (type ...) annotations
     written in its body, as for a named procedure, and its return type is the type of the last expression of its body.
     (lambda (x y) (* x y)) in (vector-map ... v1 v2) with v1 v2 : #(flonum?) -> (lambda (x y) (fl* x y)) , returns flonum? |#

  ; a lambda whose parameters are a proper list of symbols, with a body : (lambda (x y) body ...) or (λ (x y) body ...)
  (define (lambda-form? e)
    (and (pair? e)
         (memq (car e) '(lambda λ))
         (pair? (cdr e))
         (list? (cadr e))
         (every symbol? (cadr e))
         (pair? (cddr e))))

  ; the annotation level of the lambda : a new hash table chained in front of annot-ht-lst, as for every level of the
  ; sexpr (see recall-annot), so the types of the lambda stay at its level and the outer levels are still visible ;
  ; the parameters are first typed there with the types given by the context (#f or sexpr : unknown, not stored)
  (define (lambda-level params param-types annot-ht-lst)
    (define lvl (cons (make-hash-table) annot-ht-lst))
    ; the parameters are local bindings : they hide the outer variables of the same name (a parameter typed by the
    ; context below replaces its marker)
    (for-each (λ (p) (mark-local-binding! p (car lvl)))
              params)
    (when param-types
      (for-each (λ (p t)
                  (when (and t (not (eq? t 'sexpr)))
                    (store-types t (list p) (car lvl))))
                params param-types))
    lvl)

  ; return type of the lambda : type of the last expression of its body, with its parameters typed by the context
  ; and by the (type ...) annotations written in its body ; no side effect (used by find-type) ; #f if unknown
  (define (lambda-return-type lam param-types ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define lvl (lambda-level (cadr lam) param-types annot-ht-lst))
    (define body (cddr lam))
    (for-each (λ (e)
                (when (and (pair? e) (is-type? (car e)))
                  (store-types (second e) (cddr e) (car lvl)))) ; annotations of the body, at the level of the lambda
              body)
    (define r (find-type (car (reverse body)) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc lvl))
    (and r (not (eq? r 'sexpr)) r))

  ; the lambda with its body annotated, its parameters typed by the context : (* x y) -> (fl* x y)
  ; the body is annotated at the level of the lambda, as the 'else of parse-expressions does for any list
  (define (annot-lambda-with-param-types lam param-types ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define lvl (lambda-level (cadr lam) param-types annot-ht-lst))
    (cons (car lam)
          (cons (cadr lam) ; parameters unchanged
                (process-lst-for-annot (cddr lam) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc lvl))))

  ; (vector-map (lambda (x y) ...) v1 v2) and the same with map, for-each, vector-for-each : the lambda is annotated
  ; with its parameters typed by the types of the elements of the sequences (when the number of parameters is the
  ; number of sequences) ; #f otherwise (then the expression is parsed as before)
  (define (specialize-higher-order-lambda s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define proc/mac (car s))
    (define argus (cdr s))
    (declare elts fpt)
    (or (and (memq proc/mac '(vector-map map for-each vector-for-each))
             {(length argus) ≥ 2}
             (lambda-form? (car argus))
             {(length (cadr (car argus))) = (length (cdr argus))}
             {elts := (map-in-order (λ (a) (element-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                          (cdr argus))}
             (cons proc/mac
                   (cons (annot-lambda-with-param-types (car argus) elts ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                         (recall-annot (cdr argus) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))))
        ; foldl , foldr , fold , fold-right , reduce , reduce-right : the lambda typed by the elements and the
        ; accumulator , annotated only when it returns the type of the accumulator
        (and {fpt := (fold-procedure-types proc/mac argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
             fpt ; := does not return the value
             (lambda-form? (car argus))
             {(length (cadr (car argus))) = (length (car fpt))}
             (equal? (lambda-return-type (car argus) (car fpt) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                     (cdr fpt))
             (cons proc/mac
                   (cons (annot-lambda-with-param-types (car argus) (car fpt) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                         (recall-annot (cdr argus) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))))
        ; build-list , build-vector : the lambda called on an index ℕ
        (and (container-higher-order? proc/mac)
             (eq? (second #;(assq proc/mac container-procedures-types) (container-entry proc/mac)) 'build)
             {(length argus) = 2}
             (lambda-form? (second argus))
             {(length (cadr (second argus))) = 1}
             (list proc/mac
                   (car (recall-annot (list (first argus)) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                   (annot-lambda-with-param-types (second argus) '(natural-fixnum?) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))))







  ; return type (or #f) or returned value(s)
  ; we search the return type of the evaluated expression
  ; comments:
  ; 1 : we are at parsing stage so it is not always possible to know as evaluation is at runtime
  ; 2 : unless we know, for example if is litteral or annotated , (unless we know) we return 'sexpr or #f
  ;  examples: for a (lambda (...) ...) we will return 'procedure? which is at runtime give procedure? predicate
  ;            for a litteral vector #(1 2 3) we will return 'vector? , will be vector? procedure at runtime
  ;  for a ((lambda (param1 param2 ...) body ...) arg1 arg2 ...) we should return the return type of the lambda which require type annotation for lambda too IF it is possible but then it should be annotated by ((begin (type-lambda ret-type-value1 ...) (lambda (param1 ...) body ...)) arg1 ...) which is heavy !
  ; the expressions that give the value of an if : (if test a b) gives (a b) , the if of Scheme+
  ; (if test then e1 ... else e2 ...) or (if test e1 ... else e2 ...) the last expression of each block ; #f for an if
  ; without else (its value can be void)
  (define (if-branches s)
    (define blocks (cddr s)) ; after the test
    (cond ((memq 'else blocks)
           {then-block <+ (let loop ((l blocks)) ; the expressions before else , without then
                            (cond ((eq? (car l) 'else) '())
                                  ((eq? (car l) 'then) (loop (cdr l)))
                                  (else (cons (car l) (loop (cdr l))))))}
           {else-block <+ (cdr (memq 'else blocks))}
           (and (pair? then-block)
                (pair? else-block)
                (list (last then-block) (last else-block))))
          ((memq 'then blocks) #f) ; then without else
          ((and (pair? blocks) (pair? (cdr blocks)) (null? (cddr blocks))) ; (if test a b)
           blocks)
          (else #f)))

  ; the type of an if : the type of its branches when it is the same , their base type when they have the same base
  ; (#(flonum?) and vector? : vector?) , else #f (unknown)
  (define (if-branches-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define branches (if-branches s))
    (define types (and branches
                       (map-in-order (λ (e) (find-type e ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                     branches)))
    (define (known? t) (and t (not (memq t '(sexpr any?)))))
    (cond ((not (and types (every known? types))) #f)
          ((every (λ (t) (equal? t (car types))) types) (car types))
          ((every (λ (t) (equal? (type-base t) (type-base (car types)))) types) (type-base (car types)))
          (else #f)))

  (define (find-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst) ; perheaps first search if type of arg has been stored
    ; could be a literal,variable known,or a procedure call (search ovrld-ht)
    (display "annot+.rkt : find-type : s=" err-port) (display s err-port) (newline err-port)

    ; test types: string, numbers,etc
    ; initially the important difference was between exact and inexact, because we always review all that under the angle of optimisation
    ; flonum VS fixnum
    ; Scheme numerical tower
    (cond ((symbol? s) ; must test symbol first because after it could cause error ?
           ; search if symbol has a type in annot hash tables list
           ; we search it in user annotation table list first because this should preempt all other annotation information
           (search-in-annot-hash-tables-list-the-type s annot-ht-lst)) ; could be a defined variable or a defined procedure also

          ; an access already rewritten for square brackets overloaded with overload-square-brackets :
          ; ($bracket-apply-overloaded$ ((getter setter nb-indexes) ...) object index ...) has the type of ($bracket-apply$ object index ...)
          ; (the type declared with (type flonum? {M_i_o[]}) for example)
          ((and (pair? s)
                (eq? (car s) '$bracket-apply-overloaded$)
                (pair? (cdr s)))
           (find-type (cons '$bracket-apply$ (cddr s)) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))

          ; for now the return are useless, remove them later perheaps
          ((boolean? s) ; literal boolean
           'boolean?)

          ((fixnum? s) ; literal integer of type fixnum
           'fixnum?)

          ((exact-integer? s) ; literal integer
           'exact-integer?) ; exact , example : 3 must return exact-integer?

          ; Warning: integer are not specific (2.0 a computer real is considered integer ! by scheme)
          ({(integer? s) and not (inexact? s)} #;{(integer? s) and (not (inexact? s))}
           'integer?)

         ; rational was commented here : before flonum? the floats would be rational? (1.5 is rational?) , it is now after flonum?

          ((flonum? s) ; literal flonum
           'flonum?)

          ; after the integers and the floats only the exact fractions arrive here : 1/2 , -3/4 (was real?)
          ((rational? s) ; literal exact rational
           'rational?)

          ((real? s) ; literal real
           'real?) ; should be real and inexact ,inexact-real? (note: inexact could be complex !)

          ((complex? s) ; literal complex
           'complex?) ; exact or inexact

          ;; no more usefull as real? is tested before and integer are tested before too as exact-integer
          ;; never reached : every number is complex? in Scheme (1/2 , 1.5 , 2^100 too) , complex? is tested before
          #;({(number? s) and (inexact? s)} ; is number? usefull here ?! yes because instead of other predicates as integer? , inexact? issue an error on string for example
           'inexact?)

            #| rational are counter intuitive with mathematic notion ==> i will not use them .
        Almost all computer number in scheme, as finite numbers in memory are rational? even Pi or sqrt(2) !!!. |#
          #;((rational? s) ; literal rational
           'rational?)

          ;; never reached : every number is complex? (tested before)
          #;((number? s) ; literal number, this case should never be true because of integer,real,complex first
           'number?)

          ((string? s) ; literal string
           'string?)
          ((char? s) ; literal character , example : #\a
           'char?)
          ((vector? s) ; literal vector : #(T) when homogeneous literal data, else vector?
           (literal-vector-type s))
          ((hash? s) ; literal hash table
           'hash?)

          ; code or litteral list
          ((list? s) ; code , example: s = '{T[i]} = '($bracket-apply$ T i)     or  litteral list
           ; replaced 'if with a 'condx
           (condx ((null? s) 'null?) ; test null list by security (could perheaps happens)

                  (exec {proc/mac <+ (car s)}) ; get the procedure/operator but is already modified -> solved this problem in parse-expressions

                  ((memq proc/mac '(lambda λ)) 'procedure?) ; lambda case, ex: (procedure? (lambda (x) (* x x))) --> #t

                  ; the walrus operator returns the value assigned : (:=> x expr) has the type of expr
                  #;({(eq? proc/mac ':=>) and (pair? (cdr s))}
                   (find-type (last s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                  ({(WALRUS-op? proc/mac) and (pair? (cdr s))}
                   (find-type (last s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))

                  ; an if : the type of its branches when they have the same known type ({k := (if ... (quotient k 2)
                  ; {3 * k + 1})} : k stays a fixnum) , else the general case below (the type any? of the macro if)
                  (exec {if-type <+ (and (eq? proc/mac 'if)
                                         (if-branches-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))})
                  (if-type if-type)

                  ; the square of a float is not negative : ℝ⁺ (a table compares only the types , not the value 2 of the
                  ; exponent : {x ³} can be negative) , see square-of-float?
                  ((square-of-float? s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst) 'nonnegative-flonum?)
          (exec {literal-power-type <+ (power-with-literal-exponent-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}) ; {x ** 3} : a float , without any declaration of the exponent
          (literal-power-type literal-power-type)

                  ; an integer literal with a float : the type of the expression with the literal written as a float ,
                  ; {x * 2} is a flonum? as (fl* x 2.0) (see float-literal)
                  (exec {with-float-literal <+ (float-literal s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)})
                  (with-float-literal (find-type with-float-literal ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))

                  ; check first for literal lists possibility:
                  ; (list ...)
                  ; (quote (...))
                  ; (quasiquote (...))
                  ; probably not unquote and unquote-splicing
                  ((eq? 'list proc/mac) ; (list a ...) : (T) when all the elements have the same known type T
                   (or (higher-order-type 'list (cdr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                       'list?))

                  ((and (quote-or-quasi? proc/mac)
                        (list? (cadr s)))
                   'list?)

                  ; search for  '{T[]} and  '{T[2]} for example
                  ; attention on recherche pas le type du contenant mais le type du contenu !
                  ((isBRACKET-APPLY*? s) ; add hash table too (but not string)
                   (or (search-in-annot-hash-tables-list-the-type s
                                                                  annot-ht-lst)
                       ; several indexes to a vector of vectors : M[i j] typed as M[i][j]
                       (multi-indexes-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                       (search-in-annot-hash-tables-list-the-type (remove-indexes s)
                                                                  annot-ht-lst)
                       ; an object with overloaded square brackets : the return type given in the overload
                       (overloaded-square-brackets-return-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                       ; an access to a string : {s[i]} is a character , {s[i : j]} a string
                       (string-access-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                       ; fallback : the container is a compound expression, not a stored variable (ex: {(vector-map * v1 v2)[0]})
                       ; (cadr s) is always the container, for $bracket-apply$ and its specialized variants ($bracket-apply-vector$, ...)
                       (element-type (cadr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ; check operators (overloaded) existence in hash tables
                  ; we have a call of an operator/procedure on arguments (1,2,3 or more args)
                  ; example: (+ #(1 2 3) #(4 5))

                  ; now deep recursively in the sexpr to find its type using annot-ht: could be another procedure call,even one overloaded
                  (exec {argus <+ (cdr s)} ; get the arguments list ,recall-annot on args too ???
                        {lgt <+ (length argus)})

                  ; same priority problem between binary and n-ary
                  
                  ; n-ary and binary (1 or 2 args) : operator exists in binary table and n-ary table ; the table of the overloads
                  ; with an exact number of arguments contains also the unary ones (overload-unary-operator) : (- M) with 1 argument
                  
                  ((and (hash-table-exists? ovrld-ht-n-ary proc/mac)
                        {(lgt = 1 or lgt = 2) and (hash-table-exists? ovrld-ht proc/mac)}) ; old version : {lgt = 2 and ...}
                   
                   {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                       argus)} ; first find the type of arguments
                   
                   {ovrld-n-ary <+ ovrld-ht-n-ary[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
                   {ovrld-binary <+ ovrld-ht[proc/mac]}
                   
                   ; use annot parsed arguments
                   (declare argus-after-annot args-types-after-annot)
                   (if use-recall-annot-in-find-type
                      then
                       {argus-after-annot := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                       {args-types-after-annot := (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                                       argus-after-annot)}
                      else
                       {argus-after-annot := argus}
                       {args-types-after-annot := args-types})

                   {ret-type <+ (operator-associativity-recursion-and-get-return-type-of-procedure proc/mac args-types-after-annot ovrld-n-ary ovrld-binary)}
                   
                   (display "annot+.rkt : find-type n-ary and binary : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary and binary : argus=" err-port) (display argus err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary and binary : args-types=" err-port) (display args-types err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary and binary : ret-type=" err-port) (display ret-type err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary and binary : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary and binary : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)
           
                   
                   (when (not (equal? argus argus-after-annot)) ; check and action if types differs
                     (when err-flag-args-changed-in-find-type
                       (error "annot+.rkt : find-type n-ary and binary : warning args changed after annot."))
                     (display "annot+.rkt : find-type n-ary and binary : warning args changed after annot." err-port) (newline err-port))

                   (when (not (equal? args-types args-types-after-annot))
                     (when err-flag-type-changed-in-find-type
                       (error "annot+.rkt : find-type n-ary and binary : warning args types changed after annot."))
                     (display "annot+.rkt : find-type n-ary and binary : warning args types changed after annot." err-port) (newline err-port))
                   
                   ; no entry for these types : the general type given by scheme-types.rkt or racket-types.rkt , if any , as in
                   ; the binary branch below : (fl+ a b) with a and b of type real? is flonum?
                   #;ret-type
                   (or ret-type
                       (scheme-return-type proc/mac)))

                  ; n-ary : exist only in n-ary table
                  ((hash-table-exists? ovrld-ht-n-ary proc/mac)
                   (display "annot+.rkt : find-type n-ary : argus=" err-port) (display argus err-port) (newline err-port)
                   {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                       argus)} ; first find the type of arguments
                   (display "annot+.rkt : find-type n-ary : args-types=" err-port) (display args-types err-port) (newline err-port)
                   {ovrld <+ ovrld-ht-n-ary[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
                   (display "annot+.rkt : find-type n-ary : s=" err-port) (display s err-port) (newline err-port)

                   ; use annot parsed arguments
                   (declare argus-after-annot args-types-after-annot)
                   (if use-recall-annot-in-find-type
                      then
                       {argus-after-annot := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                       {args-types-after-annot := (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                                       argus-after-annot)}
                      else
                       {argus-after-annot := argus}
                       {args-types-after-annot := args-types})

                   {ret-type <+ (operator-associativity-recursion-and-get-return-type-of-procedure proc/mac args-types-after-annot ovrld #f)}
                   (display "annot+.rkt : find-type n-ary : ret-type=" err-port) (display ret-type err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
                   (display "annot+.rkt : find-type n-ary : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)

                   (when (not (equal? argus argus-after-annot)) ; check and action if types differs
                     (when err-flag-args-changed-in-find-type
                       (error "annot+.rkt : find-type n-ary : warning args changed after annot."))
                     (display "annot+.rkt : find-type n-ary : warning args changed after annot." err-port) (newline err-port))

                   (when (not (equal? args-types args-types-after-annot))
                     (when err-flag-type-changed-in-find-type
                       (error "annot+.rkt : find-type n-ary : warning args types changed after annot."))
                     (display "annot+.rkt : find-type n-ary : warning args types changed after annot." err-port) (newline err-port))

                   ; no entry for these types : the general type of scheme-types.rkt or racket-types.rkt , if any
                   #;ret-type
                   (or ret-type
                       (scheme-return-type proc/mac)))


                  ; binary or unary : exist only in the table of the overloads with an exact number of arguments , 2 or 1 arguments
                  ({(lgt = 1 or lgt = 2) and (hash-table-exists? ovrld-ht proc/mac)} ; old version : {lgt = 2 and ...}
                   {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                       argus)} ; first find the type of arguments
                   {ovrld <+ ovrld-ht[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
                   (display "annot+.rkt : find-type binary only : s=" err-port) (display s err-port) (newline err-port)

                   ; use annot parsed arguments
                   (declare argus-after-annot args-types-after-annot)
                   (if use-recall-annot-in-find-type
                      then
                       {argus-after-annot := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                       {args-types-after-annot := (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                                       argus-after-annot)}
                      else
                       {argus-after-annot := argus}
                       {args-types-after-annot := args-types})

                   {ret-type <+ (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure ovrld args-types-after-annot)}
                   (display "annot+.rkt : find-type binary-only : ret-type=" err-port) (display ret-type err-port) (newline err-port)
                   (display "annot+.rkt : find-type binary-only : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
                   (display "annot+.rkt : find-type binary-only : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)

                   (when (not (equal? argus argus-after-annot)) ; check and action if types differs
                     (when err-flag-args-changed-in-find-type
                       (error "annot+.rkt : find-type binary-only : warning args changed after annot."))
                     (display "annot+.rkt : find-type binary-only : warning args changed after annot." err-port) (newline err-port))

                   (when (not (equal? args-types args-types-after-annot))
                     (when err-flag-type-changed-in-find-type
                       (error "annot+.rkt : find-type binary-only : warning args types changed after annot."))
                     (display "annot+.rkt : find-type binary-only : warning args types changed after annot." err-port) (newline err-port))

                   ; no entry for these types : the general type given by scheme-types.rkt , if any , as when the procedure is not in
                   ; the tables : (sin x) with x of unknown type is number? (only (sin x) of a flonum is flsin)
                   (or ret-type
                       #;(and (hash-table-exists? scheme-ht proc/mac)
                            (caar {scheme-ht[proc/mac]}))
                       (scheme-return-type proc/mac)))

                  ; for procedures (overloaded procedures)
                  ((hash-table-exists? ovrld-ht-proc proc/mac)
                   {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                       argus)} ; first find the type of arguments
                   (display "annot+.rkt : find-type : hash-table-exists? ovrld-ht-proc : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
                   {ovrld <+ ovrld-ht-proc[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
                   (display "annot+.rkt : find-type procedure : s=" err-port) (display s err-port) (newline err-port)

                   ; use annot parsed arguments
                   (declare argus-after-annot args-types-after-annot)
                   (if use-recall-annot-in-find-type
                      then
                       {argus-after-annot := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
                       {args-types-after-annot := (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                                       argus-after-annot)}
                      else
                       {argus-after-annot := argus}
                       {args-types-after-annot := args-types})

                   {ret-type <+ (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure ovrld args-types-after-annot)}
                   (display "annot+.rkt : find-type procedure : ret-type=" err-port) (display ret-type err-port) (newline err-port)
                   (display "annot+.rkt : find-type procedure : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
                   (display "annot+.rkt : find-type procedure : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)

                   (when (not (equal? argus argus-after-annot)) ; check and action if types differs
                     (when err-flag-args-changed-in-find-type
                       (error "annot+.rkt : find-type procedure : warning args changed after annot."))
                     (display "annot+.rkt : find-type procedure : warning args changed after annot." err-port) (newline err-port))

                   (when (not (equal? args-types args-types-after-annot))
                     (when err-flag-type-changed-in-find-type
                       (error "annot+.rkt : find-type procedure : warning args types changed after annot."))
                     (display "annot+.rkt : find-type procedure : warning args types changed after annot." err-port) (newline err-port))

                   ;this returns the first of the list  , TODO? scheme multiple values are out of topic for now as poorly composable
                   ret-type)

                  ; check for 'values procedure
                  ; (values arg1 arg2 ...) returns multiple values,
                  ; so the type is the list of the types of each argument
                  ((isVALUES? s)
                   {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                       argus)}
                   args-types)

                  ; search in list of annot hash table if the user has defined some types for procedures in the parsed code
                  ; tip is type
                  ; example:
                  #| DONE: tested if sin overloaded with flonum return type will work with other flonum operator,ex: (fl+ 1.0 (sin 0.3)), test should run in tip section
                will only work (for now) in code,not with overload because it does not include a return type for now,
                and not REPL (i suppose it would work in REPL if you put back all the definitions in nested blocks of code
                update: works also in REPL :-O even if i can not really figure how? :-) probably because in REPL annot-ht-lst
                has not a forced re-init at each REPL entry at toplevel (or not?) in the REPL loop),works at REPL or in code when both type annotation and expression are in the same (REPL or file) you can not share one in file,type annotation and expect then
                it will work at REPL after, but that it is unclear why there is such a limitation and not with other feature: 
                So only works with:
                (type[(flonum?) procedure? (flonum?)] sin)
                {2.3 + (sin 0.3)}
                parsed result: (fl+ 2.3 (sin 0.3)) |#
                  (exec {tip <+ (search-in-annot-hash-tables-list-the-type-of-return proc/mac annot-ht-lst)} ;variants also search-in-annot-hash-tables-list* for a type of function (not symbol), values, procedure? type
                        (display "annot+.rkt : tip : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
                        (display "annot+.rkt : tip=" err-port) (display tip err-port) (newline err-port)
                        )

                  (tip tip) ; if type exist return type

                  ; typing rules of higher order and container procedures (vector-map, map, apply, vector->list, ...) :
                  ; the type of the result depends on the procedure passed and on the types of the elements
                  (exec {ho-type <+ (higher-order-type proc/mac argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)})

                  (ho-type ho-type)

                  ; search in scheme static hash table containing scheme R*RS functions with their declaration (return type and sometimes argument types)
                  ;  type inference on scheme documented and existing procedure return type,example: string-append
                  #; (exec (display "annot+.rkt : find-type : before scheme-ht test" err-port)
                           (newline err-port))

                  ((hash-table-exists? scheme-ht proc/mac)
                   (display "annot+.rkt : find-type : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
                   (display "annot+.rkt : find-type : find proc/mac in scheme-ht:" err-port) (display {scheme-ht[proc/mac]} err-port) (newline err-port)
                   #;{rv := (caar scheme-ht[proc/mac])} ; we take only the first returned value (no values support as it is hardly function composable)
                   {rv <+ (scheme-return-type proc/mac)} ; the first returned value , #f for any? (unknown)
                   (display "annot+.rkt : find-type : rv=" err-port) (display rv err-port) (newline err-port)
                   (or rv 'sexpr)) ; unknown as in the else below

                  (else
                    'sexpr))) #| probably not  literal list but code as in Scheme code expressions and lists are homoiconic |#

          ; else is following
          (else #f))) ; unknown type


  
  ; match procedures:
  ;   binary
  ;   n-ary
  
  ; check for matching between 1 overloaded procedure in the list and given types

  ; for binary operators we need to check the types of both arguments
  ; return the overloaded procedure if types matches the types of overloaded procedure (check only one)
  (define (match-type-with-one-ovrld? one-ovrld types) ; one-ovrld : (vector-append (vector? vector?)) or orig-+
    ;(display "annot+.rkt : match-type-with-one-ovrld? : one-ovrld=" err-port) (display one-ovrld err-port) (newline err-port)
    ;(display "annot+.rkt : match-type-with-one-ovrld? : types=" err-port) (display types err-port) (newline err-port)
    {len-one-ovrld <+ (length one-ovrld)} #| could be 2 or 3 , historically overload* had did not specified return type but just parameters types
  example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?))
  now that there is needed type annotation we use also a return type as in this example:
  (overload-existing-operator + (vector?) vector-append (vector? vector?)) |#
    ;(display "annot+.rkt : match-type-with-one-ovrld? : len-one-ovrld=" err-port) (display len-one-ovrld err-port) (newline err-port)
    (declare proc one-ovrld-types)
    (if {len-one-ovrld = 2} then ; example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?)) --> (multiply-flomat-vector (flomat? vector?))
        {proc := (first one-ovrld)}
        {one-ovrld-types := (second one-ovrld)}
      else ; example: (overload-existing-operator + (vector?) vector-append (vector? vector?)) --> ((vector?) vector-append (vector? vector?))
        {proc := (second one-ovrld)}
        {one-ovrld-types := (third one-ovrld)})

    ;; Example:	 Overload hash table : ovrld-ht :
    ;;  (- → (((number?) orig-- (number? number?))))
    ;;  (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
    ;;  (* → (((number?) orig-* (number? number?))))
    ;;  (· → (((number?) orig-· (number? number?))))

    ;(display "annot+.rkt : match-type-with-one-ovrld? : proc=" err-port) (display proc err-port) (newline err-port)
    ;(newline err-port)

    (if (types-match? types one-ovrld-types) ; composite types (#(T), (T)) match their base type (vector?, list?)
        proc ;  must return a proc
        #f))

  
  
  ; for n-ary operators here
  ; return the overloaded procedure if types matches the types of overloaded procedure (check only one list of types)
  (def (match-type-with-one-ovrld-n-ary? one-ovrld types) ; one-ovrld : (vector-append (vector? vector?)) or orig-+
       ;(display "annot+.rkt : match-type-with-one-ovrld-n-ary? : one-ovrld=" err-port) (display one-ovrld err-port) (newline err-port)
       {len-one-ovrld <+ (length one-ovrld)} #| could be 2 or 3 , historically overload* had did not specified return type but just parameters types
                                       example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?))
                                       now that there is needed type annotation we use also a return type as in this example:
                                       (overload-existing-operator + (vector?) vector-append (vector? vector?)) |#
       ;(display "annot+.rkt : match-type-with-one-ovrld-n-ary? : len-one-ovrld=" err-port) (display len-one-ovrld err-port) (newline err-port)
       ; first find procedure and overload types before checking match
       (declare proc one-ovrld-types)

       ; done: see only-one-type/one-type guard below, same as match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure?
       (if {len-one-ovrld = 2} then ; example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?)) --> (multiply-flomat-vector (flomat? vector?))
           {proc := (first one-ovrld)}
           {one-ovrld-types := (second one-ovrld)}
         else ; example: (overload-existing-operator + (vector?) vector-append (vector? vector?)) --> ((vector?) vector-append (vector? vector?))
           {proc := (second one-ovrld)}
           {one-ovrld-types := (third one-ovrld)})

       ;; Example:	 Overload hash table : ovrld-ht :
       ;;  (- → (((number?) orig-- (number? number?))))
       ;;  (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
       ;;  (* → (((number?) orig-* (number? number?))))
       ;;  (· → (((number?) orig-· (number? number?))))

       ;(display "annot+.rkt : match-type-with-one-ovrld-n-ary? : proc=" err-port) (display proc err-port) (newline err-port)
       ;(newline err-port)
       (when (null? one-ovrld-types)
         (if (null? types) ; types and one-ovrld-types match because they are all null?
             (return proc) ;  this would be an n-ary operator that have no arguments at this moment, example: (+) ---> 0
             (return #f)))

       ; first we must check if all types match all args (strict mode) , later if not we will relax constraints
       (when (types-match? types one-ovrld-types) ; composite types (#(T), (T)) match their base type (vector?, list?)
          (return proc)) ;  must return a proc
      
       ; no exact match before,so we try now to relax constraints
       {only-one-type <+ (length one-ovrld-types) = 1} ; if only one type is provided for n-ary operator/procedure (same guard, same variable names as match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure?)
       {one-type <+ (first one-ovrld-types)}
       ; in this case: {5 * #(1 2 3) * #(4 5 6) * #(7 8 9 10)} , where * is numeric operator , vector dot product and multiplicative of number and vector ,2 solutions:
       ;               (* 5 (* #(1 2 3) #(4 5 6) #(7 8 9 10))) ? it is computed with binary arity and the annotation is ok.
       ; in fact we should set parenthesis,because even with operator precedence this is mathematically ambiguous and could have no sense if #(4 5 6) * #(7 8 9 10) is tried to be computed
       ;    then       {5 * (#(1 2 3) * #(4 5 6)) * #(7 8 9 10)}
       ;               (* 5 #(1 2 3) #(4 5 6))    it is computed at runtime for now

       (when {only-one-type and (every (λ (t) (type-match? t one-type)) ; adapted from n-ary operators like in overload.rkt
                                        types)}
         (return proc)) ;  must return a proc

       #f)


  ; 2 procedures match-type-with-one-ovrld-*-and-get-return-type-of-procedure? :
  ; match-type-with-one-ovrld-and-get-return-type-of-procedure?
  ; match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure?
  ;
  ; binary case
  ; return the return type of overloaded procedure if types matches the types of overloaded procedure (check only one)
  (define (match-type-with-one-ovrld-and-get-return-type-of-procedure? one-ovrld types)

    {len-one-ovrld <+ (length one-ovrld)} #| could be 2 or 3 , historically overload* had did not specified return type but just parameters types
                                       example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?))
                                       now that there is needed type annotation we use also a return type as in this example:
                                       (overload-existing-operator + (vector?) vector-append (vector? vector?)) |#
    (declare proc one-ovrld-types ret-types one-ret-type) ; unecessary declaration
    (if {len-one-ovrld = 2} then ; example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?)) --> (multiply-flomat-vector (flomat? vector?))
        {proc := (first one-ovrld)}
        ;(error "match-type-with-one-ovrld-and-get-return-type-of-procedure? : no return type provided to the overloading of procedure : proc=" proc)
        #f ; as we have no return type for proc
      else ; example: (overload-existing-operator + (vector?) vector-append (vector? vector?)) --> ((vector?) vector-append (vector? vector?))
        {proc := (second one-ovrld)}
        {one-ovrld-types := (third one-ovrld)}
        {ret-types := (first one-ovrld)}
        {one-ret-type := (first ret-types)} ; from the list of return types, we take the first one (no multiple value support for now)
        (if (types-match? types one-ovrld-types) ; composite types (#(T), (T)) match their base type (vector?, list?)
            one-ret-type ;  must return the return type of proc, if multiple values, return the first one for now TODO? multiple ? well multiple values does not follow the normal call of procedures, you can not really pipe (compose) multiple values functions, so will keep the code like that for now!
            #f)))

  ;; Example:	 Overload hash table : ovrld-ht :
  ;;  (- → (((number?) orig-- (number? number?))))
  ;;  (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
  ;;  (* → (((number?) orig-* (number? number?))))
  ;;  (· → (((number?) orig-· (number? number?))))



  ; n-ary case
  (define (match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure? one-ovrld types) ; ovrld)

    ; first analyze the overload data we have:
    {len-one-ovrld <+ (length one-ovrld)} #| could be 2 or 3 , historically overload* had did not specified return type but just parameters types
                                       example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?))
                                       now that there is needed type annotation we use also a return type as in this example:
                                       (overload-existing-operator + (vector?) vector-append (vector? vector?)) |#
    ;(declare proc one-ovrld-types ret-types one-ret-type)
    (condx ({len-one-ovrld = 2}  ; do not exist a declared return type for proc , example: (overload-existing-operator * multiply-flomat-vector (flomat? vector?)) --> (multiply-flomat-vector (flomat? vector?))
            ;{proc := (first one-ovrld)}
            ;(error "match-type-with-one-ovrld-and-get-return-type-of-procedure? : no return type provided to the overloading of procedure : proc=" proc)
            #f) ; as we have no return type for proc
           
           (exec ; exist return type for proc , example: (overload-existing-operator + (vector?) vector-append (vector? vector?)) --> ((vector?) vector-append (vector? vector?))
            {proc <+ (second one-ovrld)} ; the specialized procedure of the overloaded procedure/operator ,ex: vector-append
            {one-ovrld-types <+ (third one-ovrld)} ; the types of the arguments of the overloaded procedure/operator , ex: (vector? vector?)
            {ret-types <+ (first one-ovrld)} ; all the return types of the procedure/operator,ex: (vector?)
            {one-ret-type <+ (first ret-types)} ; from the list of return types, we get the first one, ex: vector?
            ; {one-type := (list (first types))} ; with n-ary all types are generally equal so we only get the first one
            {only-one-type <+ (length one-ovrld-types) = 1} ; if only one type is provided for n-ary operator/procedure
            {one-type <+ (first one-ovrld-types)})

           #| Then apply a strategy to find the returning type depanding of args types only. Strategy fo n-ary is to check types for:
           1) if only one type is provided for the n-ary operator/procedure, then all argument types must be equal to this single type
           2) if multiple types are provided for the n-ary operator/procedure, then an exact match is searched (i do not think n-ary operator use more
           than 2 different arguments so this case is in reality a binary operator case.
           3) no more TODO : try to find possible match by dividing recursively (if necessary) the expression using its LEFT associativity property,
           example: (+ a b c) = (+1 (+2 a b) c) , and check if the types of the divided expression match with the types of the n-ary operator/procedure
           REASON : this is now handled one level above, by operator-associativity-recursion-and-get-return-type-of-procedure, which calls
           find-if-exists-matching-types-with-ovrld-n-ary-and-get-return-type-of-procedure (hence this function) again with the
           subset of types (a b), then combines the resulting type with the rest of the arguments, recursively
           4) if no match is found here (nor by the recursive division above), then we return #f
           |#

           ((or {only-one-type and (every (λ (x) (type-match? x one-type)) ; 1) all argument types are equal to the single (only 1) type of the n-ary types of the overloaded procedure/operator
                                          types)}
		(types-match? types one-ovrld-types)) ; 2) all arguments types are equal to the n-ary types of the overloaded procedure/operator (composite types match their base type)

            one-ret-type) ;  must return the return type of proc, if multiple values, return the first one for now TODO multiple ? well multiple values does not follow the normal call of procedures, you can not really pipe (compose) multiple values functions, so will keep the code like that for now!

	   ; nothing more TODO 3) 
	   
           (else ; 4) : no match is found, then we return #f
               #f)))

  ;; Example:	 Overload hash table : ovrld-ht :
  ;;  (- → (((number?) orig-- (number? number?))))
  ;;  (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
  ;;  (* → (((number?) orig-* (number? number?))))
  ;;  (· → (((number?) orig-· (number? number?))))



  ; process list recursively, recalling annot because list is code and each element of list
  ; must have its own annotation hash table separate from the others as we dive in different blocks of nested code
  (define (process-lst-for-annot L ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (cond ((null? L) '())
          ; a pragma ANNOTATION-ON / ANNOTATION-OFF (a marker left by the reader) : it acts on the rest of the list and
          ; on all that is nested in it, the state of before comes back at the end of the list (parameterize).
          ; The marker itself is removed from the code.
          ((annotation-marker? (first L))
           (parameterize ((annotation-active? (eq? (first L) annotation-on-symbol)))
             (process-lst-for-annot (rest L) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))
          (else ; it should be a non empty list
            (define fst (first L))
            (define rst (rest L))
            (define prsed (annot fst
                                 ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                 annot-ht-lst)) ; recurse deeply with annot here , with the current hash table for this level, so we do not change the passed argument
            (if (void? prsed) ; remove it from result even if it works by adding true void return value, not litteral #<void> but it displays as
                ; if not it could be almost annihilated by a macro in Scheme+/type later
                ; that returns void but it is at runtime and could take time in computation, better remove it here !
                (process-lst-for-annot rst ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                (cons prsed
                      (process-lst-for-annot rst ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))))) ; continue with the current hash table as we read the elements of the list at same level


  ; check the full list of "overloads" one by one with map to find if one matches with given types and return procedure name
  (define (find-if-exists-matching-types-with-ovrld ovrld args-types) ; ovrld = ((multiply-matrix-vector (matrix-vect? vector?)) (multiply-matrix-matrix (matrix-vect? matrix-vect?)) orig-*)
    ;(display "annot+.rkt : find-if-exists-matching-types-with-ovrld : ovrld=" err-port) (display ovrld err-port) (newline err-port)
    (display "annot+.rkt : find-if-exists-matching-types-with-ovrld : args-types=" err-port) (display args-types err-port) (newline err-port)
    (any (λ (one-ovrld) (match-type-with-one-ovrld? one-ovrld args-types))
         ovrld))

  (define (find-if-exists-matching-types-with-ovrld-n-ary ovrld args-types) ; ovrld = ((multiply-matrix-vector (matrix-vect? vector?)) (multiply-matrix-matrix (matrix-vect? matrix-vect?)) orig-*)
    ;(display "annot+.rkt : find-if-exists-matching-types-with-ovrld-n-ary : ovrld=" err-port) (display ovrld err-port) (newline err-port)
    (display "annot+.rkt : find-if-exists-matching-types-with-ovrld-n-ary : args-types=" err-port) (display args-types err-port) (newline err-port)
    (any (λ (one-ovrld) (match-type-with-one-ovrld-n-ary? one-ovrld args-types))
         ovrld)) ; all overloads (not only one)


  ; check the full list of "overloads" one by one with map to find if one matches with given types (and return first type of list if multiple values returned)

  (define (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure ovrld args-types) ; ovrld = ((multiply-matrix-vector (matrix-vect? vector?)) (multiply-matrix-matrix (matrix-vect? matrix-vect?)) orig-*)
    (any (λ (one-ovrld) (match-type-with-one-ovrld-and-get-return-type-of-procedure? one-ovrld args-types))
           ovrld))

  (define (find-if-exists-matching-types-with-ovrld-n-ary-and-get-return-type-of-procedure ovrld args-types) ; ovrld = ((multiply-matrix-vector (matrix-vect? vector?)) (multiply-matrix-matrix (matrix-vect? matrix-vect?)) orig-*)
    (any (λ (one-ovrld)
             (match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure? one-ovrld args-types))
           ovrld))


  #| type-only counterpart of operator-associativity-recursion (see below, used by parse-expressions to rewrite
  expressions): here we do not rewrite/reconstruct any expression, we only need the return TYPE of a left-associative
  call to an n-ary (and possibly also binary) overloaded operator, dividing the argument types recursively by
  associativity (schema: (op a b c ...) = (op1 (op2 a b) c ...)) when no direct/flat match is found for all the
  argument types at once.
  This implements the "case 3)" which was left as a TODO in
  match-type-with-one-ovrld-n-ary-and-get-return-type-of-procedure? above.
  ovrld-n-ary and/or ovrld-binary can be #f when the operator/procedure does not exist in that table. |#

  #| ANALYSE (FR) : comment find-type, parse-expressions et le dispatch runtime de Scheme+ (overload.rkt)
   se répartissent le problème du typage/calcul d'un appel à un opérateur n-aire associatif (ex: +, *, - à
   plus de 2 arguments) :

   Deux couches existent déjà, chacune en deux variantes (une qui rend une PROCÉDURE pour la réécriture de
   code, une qui rend un TYPE pour l'inférence) :
     - recherche directe/à plat : find-if-exists-matching-types-with-ovrld[-n-ary] (réécriture, renvoie un
       proc) et find-if-exists-matching-types-with-ovrld[-n-ary]-and-get-return-type-of-procedure (typage,
       renvoie un type) — ne savent traiter QUE le cas où un seul overload matche directement tous les
       arguments à la fois.
     - orchestration associative : operator-associativity-recursion (réécriture, plus bas dans ce fichier)
       et operator-associativity-recursion-and-get-return-type-of-procedure (ici) — ajoutent par-dessus la
       division associative GAUCHE (op a b c ...) = (op1 (op2 a b) c ...) quand le match direct échoue avec
       ≥ 3 arguments.

   La seule différence substantielle entre les deux orchestrateurs : operator-associativity-recursion doit en
   plus décider QUEL CODE émettre pour le sous-résultat (précalcul littéral / appel spécialisé / dispatch
   dynamique — logique historiquement factorisée dans rewrite/compute-operational-expression, maintenant
   dupliquée en ligne dans operator-associativity-recursion) et doit rappeler find-type sur l'expression
   reconstruite expr2 pour connaître son type avant de poursuivre la récursion. Ici, le type du sous-résultat
   est déjà connu directement (c'est la valeur retournée par l'appel récursif) : pas de code à choisir, pas de
   find-type à rappeler. C'est pourquoi ni rewrite/compute-operational-expression ni cette étape de choix de
   code n'ont leur place dans le typage pur — le type produit est le même quelle que soit la stratégie de
   génération de code choisie.
   Cette fonction ajoute en revanche une garde absente de operator-associativity-recursion : si le type du
   sous-résultat (expr2-type) est #f, on arrête (return #f) au lieu de poursuivre la récursion, parce qu'il
   n'existe pas, côté typage pur, d'équivalent au "dispatch dynamique à l'exécution" qui permettrait de
   continuer malgré un type inconnu — alors que côté réécriture, une expr2 non résolue reste du code valide
   (elle sera dispatchée dynamiquement à l'exécution), donc operator-associativity-recursion peut se permettre
   de poursuivre sans garde.

   Enfin, ce même algorithme (match direct puis division associative gauche récursive) est aussi celui du
   dispatch RUNTIME de Scheme+, dans overload.rkt (define-overload-existing-n-ary-associative-operator[-annot]) :
   c'est la procédure concrète appelée quand aucune spécialisation statique n'a pu être trouvée ici. Elle fait
   le même travail mais sur des VALEURS réelles via des prédicats Racket (number?, vector?, ...) au lieu de
   symboles de type, et récursivement sur des valeurs calculées au lieu de types. annot+.rkt (statique) et
   overload.rkt (dynamique) sont donc deux étages d'une même stratégie : résoudre le plus possible à l'analyse,
   et laisser le second étage, au runtime, garantir un résultat correct pour tout le reste. |#

  #| ANALYSIS (EN) : how find-type, parse-expressions and Scheme+'s runtime dispatch (overload.rkt) split the
   work of typing/computing a call to an associative n-ary operator (e.g. +, *, - with more than 2 arguments):

   Two layers already exist, each in two flavours (one returns a PROCEDURE for code rewriting, the other
   returns a TYPE for type inference):
     - flat/direct lookup: find-if-exists-matching-types-with-ovrld[-n-ary] (rewriting, returns a proc) and
       find-if-exists-matching-types-with-ovrld[-n-ary]-and-get-return-type-of-procedure (typing, returns a
       type) — only handle the case where a single overload entry matches ALL arguments at once.
     - associative orchestration: operator-associativity-recursion (rewriting, further down in this file) and
       operator-associativity-recursion-and-get-return-type-of-procedure (here) — add LEFT-associative
       splitting on top, (op a b c ...) = (op1 (op2 a b) c ...), when the flat match fails with ≥ 3 arguments.

   The only real difference between the two orchestrators: operator-associativity-recursion also has to decide
   WHICH CODE to emit for the sub-result (literal folding / specialized call / dynamic dispatch — logic
   historically factored out as rewrite/compute-operational-expression, now inlined directly into
   operator-associativity-recursion), and must call find-type on the reconstructed expr2 expression to learn
   its type before continuing the recursion. Here, the sub-result's type is already known directly (it's the
   value returned by the recursive call): there is no code to choose, no find-type to call back. That is
   exactly why neither rewrite/compute-operational-expression nor that codegen-choice step belong in pure type
   inference — the resulting type is the same regardless of which codegen strategy would have been picked.
   This function does add one guard that operator-associativity-recursion lacks: if the sub-result's type
   (expr2-type) is #f, it stops (return #f) instead of continuing the recursion, because pure type inference
   has no equivalent to "dynamic dispatch at runtime" that would let it carry on despite an unknown type —
   whereas on the rewriting side, an unresolved expr2 is still valid code (it will simply dispatch dynamically
   at runtime), so operator-associativity-recursion can safely proceed without that guard.

   Finally, this very algorithm (flat match, then recursive left-associative split) is also exactly what
   Scheme+'s RUNTIME dispatch does, in overload.rkt (define-overload-existing-n-ary-associative-operator[-annot]):
   it is the actual procedure invoked when no static specialization could be found here. It does the same work
   but on real VALUES via Racket predicates (number?, vector?, ...) instead of type symbols, and recurses on
   computed values instead of types. annot+.rkt (static) and overload.rkt (dynamic) are therefore two tiers of
   one strategy: resolve as much as possible at analysis time, and let the second tier, at runtime, guarantee
   a correct result for everything else. |#

  #| ANALISI (IT) : come find-type, parse-expressions e il dispatch a runtime di Scheme+ (overload.rkt) si
   dividono il compito di tipizzare/calcolare una chiamata a un operatore n-ario associativo (es. +, *, - con
   più di 2 argomenti) :

   Esistono già due livelli, ciascuno in due varianti (una che restituisce una PROCEDURA per la riscrittura
   del codice, l'altra che restituisce un TIPO per l'inferenza) :
     - ricerca diretta/piatta : find-if-exists-matching-types-with-ovrld[-n-ary] (riscrittura, restituisce
       una proc) e find-if-exists-matching-types-with-ovrld[-n-ary]-and-get-return-type-of-procedure
       (tipizzazione, restituisce un tipo) — gestiscono SOLO il caso in cui un singolo overload corrisponde a
       tutti gli argomenti in una volta.
     - orchestrazione associativa : operator-associativity-recursion (riscrittura, più in basso in questo
       file) e operator-associativity-recursion-and-get-return-type-of-procedure (qui) — aggiungono sopra la
       divisione associativa SINISTRA (op a b c ...) = (op1 (op2 a b) c ...) quando la corrispondenza diretta
       fallisce con ≥ 3 argomenti.

   L'unica differenza sostanziale tra i due orchestratori : operator-associativity-recursion deve inoltre
   decidere QUALE CODICE emettere per il sotto-risultato (precalcolo del letterale / chiamata specializzata /
   dispatch dinamico — logica storicamente isolata in rewrite/compute-operational-expression, ora duplicata in
   linea dentro operator-associativity-recursion) e deve richiamare find-type sull'espressione ricostruita
   expr2 per conoscerne il tipo prima di continuare la recursione. Qui, il tipo del sotto-risultato è già noto
   direttamente (è il valore restituito dalla chiamata recursiva) : non c'è codice da scegliere, non c'è
   find-type da richiamare. Questo è esattamente il motivo per cui né rewrite/compute-operational-expression né
   quella fase di scelta del codegen appartengono all'inferenza di tipo pura — il tipo prodotto è lo stesso
   indipendentemente dalla strategia di codegen scelta.
   Questa funzione aggiunge invece una guardia assente in operator-associativity-recursion : se il tipo del
   sotto-risultato (expr2-type) è #f, si interrompe (return #f) invece di continuare la recursione, perché
   nell'inferenza di tipo pura non esiste un equivalente al "dispatch dinamico a runtime" che permetterebbe di
   continuare nonostante un tipo sconosciuto — mentre dal lato della riscrittura, una expr2 non risolta resta
   comunque codice valido (verrà semplicemente dispatchata dinamicamente a runtime), quindi
   operator-associativity-recursion può proseguire senza questa guardia.

   Infine, questo stesso algoritmo (corrispondenza diretta, poi divisione associativa sinistra recursiva) è
   anche esattamente ciò che fa il dispatch RUNTIME di Scheme+, in overload.rkt
   (define-overload-existing-n-ary-associative-operator[-annot]) : è la procedura effettivamente invocata
   quando qui non è stato possibile trovare alcuna specializzazione statica. Fa lo stesso lavoro ma su VALORI
   reali tramite predicati Racket (number?, vector?, ...) invece che su simboli di tipo, e recursa su valori
   calcolati invece che su tipi. annot+.rkt (statico) e overload.rkt (dinamico) sono quindi due livelli di
   un'unica strategia : risolvere il più possibile in fase di analisi, e lasciare che il secondo livello, a
   runtime, garantisca un risultato corretto per tutto il resto. |#

  (def (operator-associativity-recursion-and-get-return-type-of-procedure proc/mac args-types ovrld-n-ary ovrld-binary)
    {nb-argus <+ (length args-types)}

    ; 1) try a direct/flat match against all argument types at once first (fast path, no associativity needed)
    ; the overloads with an exact number of arguments (binary , unary) first , as the runtime dispatcher of overload.rkt : with
    ; (overload-operator - msub (mat? mat?)) and (overload-unary-operator - mneg (mat?)) , {A - B} is (msub A B) , not (mneg A B)
    {ret-type <+ (or (ovrld-binary and (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure ovrld-binary args-types)) ; exact number of arguments
                     (ovrld-n-ary and (find-if-exists-matching-types-with-ovrld-n-ary-and-get-return-type-of-procedure ovrld-n-ary args-types)))} ; n-ary
    #;{ret-type := (or (ovrld-n-ary and (find-if-exists-matching-types-with-ovrld-n-ary-and-get-return-type-of-procedure ovrld-n-ary args-types)) ; n-ary
                     (ovrld-binary and nb-argus = 2 and (find-if-exists-matching-types-with-ovrld-and-get-return-type-of-procedure ovrld-binary args-types)))} ; binary
    (when ret-type
      (return ret-type))

    ; 2) no direct match: try to divide the expression using its LEFT associativity property, recursively
    (when {nb-argus ≥ 3} ; when we have a n-ary expression
      {args-types-expr2 <+ (list (first args-types)
				 (second args-types))} ; types of (op2 a b) = expr2

      ; first (op2 a b) = expr2 , recursion :
      {expr2-type <+ (operator-associativity-recursion-and-get-return-type-of-procedure proc/mac args-types-expr2 ovrld-n-ary ovrld-binary)}

      (when expr2-type
        ; then (op1 expr2 c ...)
        {c-rst-types <+ (rest (rest args-types))} ; c-rst-types = (c-type d-type e-type ...)
        {expr1-type <+ (operator-associativity-recursion-and-get-return-type-of-procedure proc/mac (cons expr2-type c-rst-types) ovrld-n-ary ovrld-binary)}
        (when expr1-type
          (return expr1-type)))) ; if expr1-type has been computed with success then we return it

    ; could not determine a type via associativity either
    #f)


  ; store types in the hash table
  ; the reserved names of Scheme+ for the types : ℕ and ℤ are fixnum? , ℝ is flonum? , ℝ⁺ nonnegative-flonum? (defined in Scheme+/type) ; the
  ; annotation knows only fixnum? and flonum? : a type is put in this canonical form when it enters the tables ,
  ; recursively : #(ℝ) , (ℕ) , a signature ((ℝ) procedure? (ℝ ℝ))
  (define (canonical-type t)
    (cond #;((memq t '(ℕ ℤ)) 'fixnum?)
          #;((eq? t 'ℝ) 'flonum?)
          #;((fixnum-type-name? t) 'fixnum?)
          #;((flonum-type-name? t) 'flonum?)
          #;((or (natural-type-name? t) (integer-type-name? t)) 'fixnum?)
          ((natural-type-name? t) 'natural-fixnum?) ; Scheme+/type : ℕ , a fixnum >= 0 (an index without negative value)
          ((integer-type-name? t) 'fixnum?) ; ℤ
          ((nonzero-integer-type-name? t) 'nonzero-fixnum?) ; ℤ* , a float to its power is a float
          ((positive-integer-type-name? t) 'positive-fixnum?) ; ℕ*
          ((real-type-name? t) 'flonum?)
          ((nonnegative-real-type-name? t) 'nonnegative-flonum?) ; ℝ⁺ , a float not negative (sqrt , log , expt of floats)
          ((pair? t) (cons (canonical-type (car t)) (canonical-type (cdr t))))
          ((and (vector? t) {(vector-length t) = 0}) 'vector?) ; #() : a vector , the type of its elements not given
          ((vector? t) (list->vector (map canonical-type (vector->list t))))
          (else t)))

  (define (store-types t L annot-ht)
    ; convert type in canonical name real -> real? , integer -> exact-integer?
    (define ct (canonical-type t)) ; ℕ ℤ ℝ --> fixnum? fixnum? flonum?
    (for-each (λ (s)
                {annot-ht[s] := ct}
                (store-element-types ct s annot-ht)) ; composite type #(T) or (T) : also the type of the elements (bracket notation)
              L))


  (define (update-type t elem annot-ht)
    ; {T[3] := "hello"} , ok T[3] is string
    ; {T[i] := "hello"} , as 'i change we can not know the type of T[i] in general
    ;
    ; {y := (vector 1 2 3 4 5)}
    ; {x := y[1 :] := #(1 2 3)}
    ; it is in prefix:
    ; (:= x ($bracket-apply$ y 1 :) #(1 2 3))
    ; do not store it : it is a copy of container's elements ,no info on type here ,above all if #(1 2 3) is not a literal but stored in a variable:
    ;  {x := y[1 :] := z}
    ;
    ;; If elem is a bracket-apply form like ($bracket-apply$ ...)
    ; then remove indexes:
    (when (and (isBRACKET-APPLY*? elem)
               (not (every number? (cddr elem)))) ; we keep sort of ($bracket-apply$ y 1) or ($bracket-apply$ y 3 2) but not ($bracket-apply$ y i) or ($bracket-apply$ y 3 :)
      {elem := (remove-indexes elem)})

    ; check when elem =[ ]  , elem =  ($bracket-apply$ T 3), example: T[3]
    ;; accept it only when it has exactly less than 3 elements (eg: ($bracket-apply$ T 3)).
    ;; Otherwise do nothing.
    ;(unless {(list? elem) and (equal? (first elem) '$bracket-apply$) and (length elem) ≠ 3} ; updated regarding to comment above about slicing
    ;; If an entry for elem already exists, print a warning then overwrite it.
    (when (hash-table-exists? annot-ht elem) ; display warning if matching type differs from previously known type and currently infered type
      {prev <+ annot-ht[elem]}
      (display "annot+.rkt : update-type : warning : overwriting existing type for element : " err-port)
      (display elem err-port)
      (display " , previous type: " err-port)
      (display prev err-port)
      (display " , new type: " err-port)
      (display t err-port)
      (newline err-port))

    {annot-ht[elem] := t} ;)
    (store-element-types t elem annot-ht) ; composite type #(T) or (T) : also the type of the elements (bracket notation)

    ;; (newline err-port)
    ;; (hash-table-walk annot-ht
    ;;                  (lambda (k v)
    ;;                    (fprintf err-port "annot-ht (~s → ~s)\n" k v)))
    ;; (newline err-port)
    )

  (define (display-hashtable ht output)
    (hash-table-walk ht
                     (lambda (k v)
                       (fprintf output " (~s → ~s)\n" k v))))

  (define (display-hashtable-list L output)
    (map-in-order (λ (ht) (display-hashtable ht output)
           (newline output)
           (newline output))
         L))

  
  ;  a recursive procedure for associative operator
  ; FR : voir l'analyse comparative (FR/EN/IT) au-dessus de operator-associativity-recursion-and-get-return-type-of-procedure,
  ;      plus haut dans ce fichier — cette procédure fait le même travail pour la réécriture de code, avec en plus
  ;      le choix de la stratégie de génération (précalcul/spécialisation/dispatch dynamique) et le rappel à find-type
  ;      sur l'expression reconstruite.
  ; EN : see the comparative analysis (FR/EN/IT) above operator-associativity-recursion-and-get-return-type-of-procedure,
  ;      earlier in this file — this procedure does the same job for code rewriting, plus choosing the codegen
  ;      strategy (literal folding/specialization/dynamic dispatch) and calling back into find-type on the
  ;      reconstructed expression.
  ; IT : vedi l'analisi comparativa (FR/EN/IT) sopra operator-associativity-recursion-and-get-return-type-of-procedure,
  ;      più in alto in questo file — questa procedura fa lo stesso lavoro per la riscrittura del codice, con in
  ;      più la scelta della strategia di generazione (precalcolo/specializzazione/dispatch dinamico) e la
  ;      chiamata a find-type sull'espressione ricostruita.
  (def (operator-associativity-recursion proc/mac argus-after-annot args-types-after-annot
                                         ovrld-n-ary ovrld-binary
                                         ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst) 

    ; first we prepare the data and try some simple finding/matching
    (display "annot+.rkt : operator-associativity-recursion (n-ary and binary) : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
    {nb-argus <+ (length argus-after-annot)}
    
    ; search if there exist some matching types -> return a procedure
    ; the overloads with an exact number of arguments (binary , unary) first , as the runtime dispatcher of overload.rkt
    {proc-after-annot <+ (or (ovrld-binary and (find-if-exists-matching-types-with-ovrld ovrld-binary args-types-after-annot)) ; exact number of arguments
                             (find-if-exists-matching-types-with-ovrld-n-ary ovrld-n-ary args-types-after-annot))} ; n-ary
    #;{proc-after-annot := (or (find-if-exists-matching-types-with-ovrld-n-ary ovrld-n-ary args-types-after-annot)
                             (ovrld-binary and nb-argus = 2 and (find-if-exists-matching-types-with-ovrld ovrld-binary args-types-after-annot)))}
    (display "annot+.rkt : operator-associativity-recursion (n-ary and binary) : proc-after-annot=" err-port) (display proc-after-annot err-port) (newline err-port)
   
    ; now rewrite expression
    ; computing numerical litterals ! this could be done also for other litterals (string, string-append overloading + for example)
    ; or even string-append with litteral strings but it does not in Racket bring any time optimisation.
    ; rewrite also because arguments could have changed

    ; proc/mac is function matching types if found (example: fl+, proc-orig is the original operator/procedure symbol (like +), argus-after-annot is the list of arguments

    {kp <+ (known-proc-ref proc/mac)} ; known-proc-ref returns the procedure bound to 'key in known-proc if it exists, #f otherwise
    {lla <+ (litteral-list? argus-after-annot)} ; check if all arguments are litterals, example: (1 2.0 3) is a litteral list,  (1 "2" 3) is also a litteral list
    (display "annot+.rkt : operator-associativity-recursion (was rewrite/compute-operational-expression) : kwnown procedure : kp =" err-port) (display kp err-port) (newline err-port)
    (display "annot+.rkt : operator-associativity-recursion (was rewrite/compute-operational-expression) : argus-after-annot is (or not) a litteral list : lla =" err-port) (display lla err-port) (newline err-port)
    
    #;(when {proc-after-annot and kp and lla and (all-numbers? argus-after-annot)} ; all arguments are litterals, we can compute the result at parsing time, TODO? could we apply this to other litterals like string, string-append overloading + for example, or even string-append with litteral strings but does in Racket it brings any time optimisation?
      (return (apply kp argus-after-annot))) ; operator found in hash table of known procedures,example (fl+ 1.0 2.0), this is the fastest pre-runtime computation as we evaluate the litterals arguments
    ; all arguments are litterals : the result computed at parsing time , not when the computation raises an error ({2.5 / 0})
    {folded <+ (and proc-after-annot kp lla (all-numbers? argus-after-annot) (fold-literals kp argus-after-annot))}
    (when folded
      (return (car folded)))
           
    (when proc-after-annot ; specialization at parsing time and computation at runtime, example: (fl+ 1.0 2.0), this is the fastest runtime computation
     (display "annot+.rkt : operator-associativity-recursion : (cons proc-after-annot argus-after-annot) =" err-port) (display (cons proc-after-annot argus-after-annot) err-port) (newline err-port)
     (return (cons proc-after-annot argus-after-annot))) ; operator specialisation found in hash tables ,example (fl+ 1.0 2.0), this is the fastest runtime computation

    ; proc-after-annot is #false

    #| this is the recursive part of the algo: if result is #f we should try to divide the expression by associativity ,
    find the sub operators/procedures and reconstruct the toplevel expression accordingly ,
    schema: (op a b c ...) = (op1 (op2 a b) c ...) , so it is possible if we know,
    of course the types of a,b,c,... and we can infer the type of op2 from a,b,... and then possibly the type of op1
    from the return type of op2 and from the type of c ..., so perheaps we can find the right procedures/operators
    for op1 and op2 and reconstruct the expression accordingly |#

    (when {nb-argus ≥ 3} ; we need at least 2 arguments to divide the expression by associativity

      ; we use annotated args
      {a <+ (first argus-after-annot)}
      {b <+ (second argus-after-annot)} 
      {args-expr2 <+ (list a b)} ; args for expr2
      {a-type <+ (first args-types-after-annot)}
      {b-type <+ (second args-types-after-annot)} 
      {args-types-expr2 <+ (list a-type b-type)} ; args-types for expr2

      ; first (op2 a b ...) = expr2 ,  recursion :
      {expr2 <+ (operator-associativity-recursion proc/mac args-expr2 args-types-expr2 
                                                  ovrld-n-ary ovrld-binary
                                                  ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
      
      ; problem if expr2 is not known in the hashtable (we must have included also: it is now done in process-overload)
      {expr2-type <+ (find-type expr2 ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)} ; find the type

      ; then we do (op1 (op2 a b) c ...)
      {c-rst <+ (rest (rest argus-after-annot))} ; c-rst = (c d e ...)
      {c-rst-types <+ (rest (rest args-types-after-annot))} ; c-rst-types = (c-type d-type e-type ...)
      {expr1 <+ (operator-associativity-recursion proc/mac (cons expr2 c-rst) (cons expr2-type c-rst-types)
                                                  ovrld-n-ary ovrld-binary
                                                  ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
    
      (when expr1
        (return expr1))) ; if expr1 has been computed with success then we return expr1

    ; expr1 is #f or not computed, we have not been able to find specialization for the operator/procedure, we will use the original operator/procedure symbol (like +) and compute at runtime with type dispatch
    ; example: (+ 1 "2" 3) , this the most powerful computation, but the slowest one (type dispatch at run time), other example : (- x)
    (cons proc/mac argus-after-annot))

  
  

  ; do we have an interesting thing to parse and modify,store as type in hash tables or list
  ; or should we parse for rewriting computation and/or generate type dispatching
  ; s = (s1 s2 ...)
  ; the square of a float written with the exact integer exponent 2 in the code : {x ²} , {x ** 2} , (expt x 2) of a
  ; flonum x --> (fl* x x) , faster than expt and more precise (the exact square rounded once , IEEE 754 , as gcc does
  ; for pow(x,2.0)) ; #f otherwise (the expression follows its usual path). Not the exponents 3 or more : by
  ; multiplications they are less precise than expt (wrong by one ulp for 26 % of the floats with x * (x * x)) ; not 2.0.
  ; A base that is not a variable is computed only once. Not used when the user overloads ** or expt for the floats.
  (define (square-of-flonum s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define op (first s))
    (and #;(memq op '(** expt))
         (EXPONENTIATION-op? op) ; ** or expt
         {(length s) = 3}
         (eqv? (third s) 2) ; the literal exact integer 2
         (not (number? (second s))) ; a literal base is left to the computation of the constants
         #;(eq? (find-type (second s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst) 'flonum?)
         (eq? (type-base (find-type (second s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)) 'flonum?) ; also a ℝ⁺
         (hash-table-exists? ovrld-ht op)
         (memq (find-if-exists-matching-types-with-ovrld {ovrld-ht[op]} '(flonum? fixnum?)) '(orig-** orig-expt)) ; no overload of the user
         ($+> {b <+ (annot (second s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
              (if (symbol? b)
                  (list 'fl* b b)
                  (list 'let (list (list '$power-base$ b)) '(fl* $power-base$ $power-base$))))))

  ; the type of (** base literal) , #f if the literal does not tell it : a literal exponent has a known sign , so a float to
  ; a literal not zero is a float , and a fixnum to a literal not negative an integer (the overflow is accepted). Only the
  ; exponent which is a literal : the type of a variable that has received a literal stays fixnum? (it can change of value).
  (define (power-with-literal-exponent-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (and (list? s)
         {(length s) = 3}
         (symbol? (first s))
         (or (EXPONENTIATION-op? (first s))
             (memq (first s) '(orig-** orig-expt))) ; the form can have been rewritten by the annotation
         (fixnum? (third s))
         (hash-table-exists? ovrld-ht (first s))
         (memq (find-if-exists-matching-types-with-ovrld {ovrld-ht[(first s)]} '(flonum? fixnum?)) '(orig-** orig-expt)) ; no overload of the user
         ($+> {base <+ (type-base (find-type (second s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))}
              (cond ((and (eq? base 'flonum?) (not {(third s) = 0})) 'flonum?)
                    ((and (eq? base 'fixnum?) {(third s) >= 0}) 'fixnum?)
                    (else #f)))))

  ; the square of a float : {x ²} , {x ** 2} , (expt x 2) of a float x (the literal exact integer 2 , without an overload
  ; of the user as in square-of-flonum) , or {x * x} , (fl* x x) of a variable x (fl* x x is written by square-of-flonum) :
  ; its type is ℝ⁺ , not only flonum? (x * x is not negative , or +nan.0)
  (define (square-of-float? s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define (float? e) (eq? (type-base (find-type e ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)) 'flonum?))
    (define op (first s))
    (and {(length s) = 3}
         (or (and (EXPONENTIATION-op? op) ; ** or expt
                  (eqv? (third s) 2)
                  (float? (second s))
                  (hash-table-exists? ovrld-ht op)
                  (memq (find-if-exists-matching-types-with-ovrld {ovrld-ht[op]} '(flonum? fixnum?)) '(orig-** orig-expt)) ; no overload of the user
                  #t)
             (and #;(eq? op 'fl*) ; the product of a variable by itself
                  ; the product of a variable by itself : an operator whose product of two floats is fl* (* · × orig-* fl* ,
                  ; not an overload of the user)
                  (hash-table-exists? ovrld-ht op)
                  (eq? (find-if-exists-matching-types-with-ovrld {ovrld-ht[op]} '(flonum? flonum?)) 'fl*)
                  (symbol? (second s))
                  (eq? (second s) (third s))
                  (float? (second s))))))

  ; the procedures of two floats that give the same result as the generic procedure on a float and an integer converted
  ; exactly to a float : (+ x 1) and (fl+ x 1.0) , (< x 1) and (fl< x 1.0) (not the exact 0 : (* 0 2.0) is the exact 0 ,
  ; (+ -0.0 0) is -0.0 but (fl+ -0.0 0.0) is 0.0)
  #;(define float-literal-procedures '(fl+ fl- fl* fl/ flmin flmax fl< fl> fl<= fl>= fl= fl≠))
  (define float-literal-procedures '(fl+ fl- fl* fl/ flmin fl< fl> fl<= fl>= fl= fl≠)) ; not flmax (see the table of max)
  ; among them the arithmetic : (+ a b c) is computed (+ (+ a b) c) by Racket , from the left
  (define float-literal-folds '(fl+ fl- fl* fl/))

  ; the operations of a flonum and a fixnum of Scheme+/flonum-fixnum (the rows of the mixed types) : the same results as
  ; the generic procedures (a guard at run time) , not an overload of the user
  (define float-fixnum-procedures '(fl+/fx fx/fl+ fl-/fx fx/fl- fl*/fx fx/fl* fl//fx fx/fl/ flmin/fx fx/flmin
                                    fl</fx fx/fl< fl>/fx fx/fl> fl<=/fx fx/fl<= fl>=/fx fx/fl>= fl=/fx fx/fl= fl≠/fx fx/fl≠))

  ; an integer literal with a float , {x * 2} --> (fl* x 2.0) , {x + 1} --> (fl+ x 1.0) , {x < 1} --> (fl< x 1.0) :
  ; the expression with the literal written as a float , or #f. The literal is converted at the reading , as the generic
  ; procedure would convert it at run time : not 0 , and |n| <= 2^53 (exactly a float). Two arguments only (in
  ; (+ 2^53 1 x) the integers are added before , exactly). Not when the user has overloaded the operator for these types
  ; (the procedure for a float and an integer must be the generic one) , nor for ** , expt , % (their procedure of two
  ; floats is not in float-literal-procedures). With more arguments , + - * / with an integer literal and a float are
  ; written from the left , (* 3 x 2) --> (* (* 3 x) 2) , as Racket computes them : each product then follows this rule.
  (define (float-literal s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (define (type-of e) (find-type e ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
    (define (float? e) (eq? (type-base (type-of e)) 'flonum?))
    (define (convertible? n) (and (exact-integer? n) (not (zero? n)) {(abs n) <= (expt 2 53)}))
    (define op (first s))
    (define (generic? types) ; the procedure chosen for these types is the generic one (no overload of the user)
      (define rows {ovrld-ht[op]})
      (define p (find-if-exists-matching-types-with-ovrld rows types))
      (or (not p)
          (eq? p op)
          (memq p float-fixnum-procedures) ; the literal written as a float is faster than the guard : (fl* x 2.0)
          (eq? p (find-if-exists-matching-types-with-ovrld rows '(number? number?)))
          (eq? p (find-if-exists-matching-types-with-ovrld rows '(real? real?)))))
    (define (fold-left-binary args) ; (a b c d) --> (op (op (op a b) c) d)
      (for/fold ((acc (first args))) ((e (rest args)))
        (list op acc e)))
    (and (hash-table-exists? ovrld-ht op)
         (memq (find-if-exists-matching-types-with-ovrld {ovrld-ht[op]} '(flonum? flonum?)) float-literal-procedures)
         (if {(length s) > 3} ; n-ary : from the left , only for the arithmetic with a literal and a float
             (and (memq (find-if-exists-matching-types-with-ovrld {ovrld-ht[op]} '(flonum? flonum?)) float-literal-folds)
                  (any convertible? (rest s))
                  (any float? (rest s))
                  (fold-left-binary (rest s)))
         (and {(length s) = 3}
         ($+> {a <+ (second s)}
              {b <+ (third s)}
              (cond ((and (convertible? a) (float? b) (generic? (list (type-of a) (type-of b))))
                     (list op (exact->inexact a) b))
                    ((and (float? a) (convertible? b) (generic? (list (type-of a) (type-of b))))
                     (list op a (exact->inexact b)))
                    (else #f)))))))

  (define (parse-expressions s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)

    (newline err-port)
    (display "annot+.rkt : parse-expressions : s=" err-port) (display s err-port) (newline err-port)
    ;{a-lists := (map hash-table->alist annot-ht-lst)}
    ;(display "annot+.rkt : parse-expressions : annot-ht-lst=" err-port) (display a-lists err-port) (newline err-port)

    ;{ovrld-ht-n-ary-a-list := (hash-table->alist ovrld-ht-n-ary)}
    ;(display "annot+.rkt : parse-expressions : ovrld-ht-n-ary-a-list=" err-port) (display ovrld-ht-n-ary-a-list err-port) (newline err-port)

    (define proc/mac (first s)) ; s = (proc/oper arg1 arg2 ...)

    (display "annot+.rkt : parse-expressions : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)

    ; now we should do different things according to situation:
    ; - save information in hash tables and rewrite or not some expressions (example:recreate-define-overload)
    ; - or just use saved information in hash tables to rewrite some expressions later (calculus for example)
    ; example below, deals with overload-existing-operator
    (define annot-ht (car annot-ht-lst)) ; get back the hash table of this level
    (condx ((quote-or-quasi? proc/mac) s) ; it is symbolic expression, could not be something interesting to parse inside as it is not for evaluation later

          ; and could create error if it contains symbolic code such as:
          ; (length '(overload-existing-operator + (vector?) vector-append (vector? vector?)))
          ; = (length (quote (overload-existing-operator + (vector?) vector-append (vector? vector?))))
          ; error (even if i ca not remember the special cas it is )  when dealing with this

          ; a syntax quote #'(...) , #`(...) , (quote-syntax ...) is data too (without this : #'(+ 1 2) became #'3 and
          ; #'(+ - * /) became #'(+ (+ - *) /))
          ((SYNTAX-QUOTE-op? proc/mac) s)

          ; a macro definition is not annotated (see macro-definition?) : only the type annotations and the markers of
          ; the pragmas are removed, as when the pass is off (a (type ...) left in a template would display a warning)
          ((macro-definition? proc/mac)
           (remove-types-and-annotation-markers s))

          ; (let-syntax ((name transformer) ...) body ...) : the local macros are not annotated, the body is, in a new level
          ((local-macro-definition? proc/mac)
           (cons (first s)
                 (cons (remove-types-and-annotation-markers (second s))
                       (process-lst-for-annot (cddr s)
                                              ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                              (cons (make-hash-table)
                                                    annot-ht-lst)))))

          #| DEPRECATED (désactivé, pas supprimé) : define-overload-existing-operator / define-overload-existing-n-ary-associative-operator
             sont une famille "two steps" explicitement dépréciée ("kept for compatibility but no more documented",
             Scheme-PLUS-for-Racket/README.md). Un fichier qui n'utilise jamais ANNOTATION-ON n'est jamais affecté par
             ce retrait (get-annot-on-read, SRFI-105-curly-infix.rkt, n'ouvre annot que si ANNOTATION-ON est présent) ;
             seul un fichier qui mélangerait ANNOTATION-ON et ces macros dépréciées perdrait la réécriture ci-dessous.
             is-define-overload? ne matche que les deux formes "-existing-" ci-dessus (le cas "nouveau symbole
             inconnu" visé par les deux TODO n'est plus atteignable : les variantes non-"existing" de define-overload-*
             sont déjà désactivées dans Scheme+/operators.rkt, remplacées par overload-operator/overload-procedure). |#
        
	
          ; which one of the two belows should have priority??? binary or n-ary (i suppose n-ary is better BUT.... beware of n-ary it can apply also in binary and that not what we want !!!)
          ; here we just store them ? so priority has no sense ?? conclusion: right !

          ; require-overload-operator restates, in a module importing the operator, an overload done in another module
          ((or (is-overload-operator? proc/mac)
               (is-require-overload-operator? proc/mac)) (process-overload-operator s ovrld-ht ovrld-ht-n-ary) ; table chosen from the number of predicates
                                                         s) ; but return the original sexpr,no annotation allowed in elements of s !

          ; overload-unary-operator : exactly one argument , in the table of the overloads with an exact number of arguments
          ((or (is-overload-unary-operator? proc/mac)
               (is-require-overload-unary-operator? proc/mac)) (process-overload-operator s ovrld-ht ovrld-ht-n-ary #t)
                                                               s)

          ; DEPRECATED (désactivé, pas supprimé) : overload-existing-operator / overload-existing-n-ary-operator sont la
          ; même famille "two steps" dépréciée que define-overload-existing-* ci-dessus (voir sa note et le README de
          ; Scheme-PLUS-for-Racket) ; process-overload reste utilisée par process-overload-operator/-procedure, ne pas la retirer.
          #;((is-overload-existing-operator? proc/mac) (process-overload s ovrld-ht) ; process the overloading expression (overload* ....)
                                                     s) ; but return the original sexpr,no annotation allowed in elements of s !

          #;((is-overload-existing-n-ary-operator? proc/mac) (process-overload s ovrld-ht-n-ary) ; process the overloading expression (overload* ....)
                                                           s) ; but return the original sexpr,no annotation allowed in elements of s !


          ; overload-existing-procedure and require-overload-existing-procedure are replaced by overload-procedure / require-overload-procedure
          ((or (is-overload-procedure? proc/mac)
               (is-require-overload-procedure? proc/mac)) (process-overload-procedure s ovrld-ht-proc) ; module name dropped
                                                          s)

          ; overload of the square brackets , kept for the accesses of an object of known type (see the case of $bracket-apply$)
          ((or (is-overload-square-brackets? proc/mac)
               (is-require-overload-square-brackets? proc/mac)) (process-overload-square-brackets s)
                                                                s)

          ; is-type? -> process-type
          ; examples: (type number? x)
          ;           (type vector? {z[]} y z)
          ((is-type? proc/mac) (store-types (second s) (cddr s) annot-ht)) ; create entries for each variables (could be vector indexed) in the current annotation hash table
          ; of pattern (expression -> type) in the hash table
          ; note: this return probably an almost invisible #<void> that is indeed removed in process-lst-for-annot

          #| (module name language body ...) : gets its own isolated annotation level, same as the clause
          (not (annotation-active?)) just below -- but the names it (provide ...)s are then copied back into the OUTER table
          (annot-ht), so a variable or a function typed inside a file stays typed when that file's module is
          loaded and the same name is then used at the REPL, or required from elsewhere : exactly what Scheme
          already does for the runtime binding itself (provide/require), just extended here to the static
          type. Non-exported (private) bindings stay confined to the module's own table, exactly as before
          this clause existed -- so nested or sibling modules can not leak private names into each other. |#
          ; before the clause (not (annotation-active?)) : ANNOTATION-ON is written in the body of the module , the
          ; annotation is not active yet when (module ...) is reached
          ((eq? proc/mac 'module)
           {mod-level-ht <+ (new-level-table s)}
           {mod-body <+ (cdddr s)}
           {processed <+ (process-lst-for-annot s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                                (cons mod-level-ht annot-ht-lst))}
           {(provided-names provided-all?) <+ (collect-provided-names mod-body)}
           (export-provided-types! mod-level-ht annot-ht provided-names provided-all?)
           processed)

          ; outside an ANNOTATION-ON region (see process-lst-for-annot) : the declarations above (overloads, types) are
          ; still recorded, but the code is not rewritten (no specialization, no inference, no constant folding) : we only
          ; go through the sub-expressions, they could contain an ANNOTATION-ON region, type annotations to remove or
          ; overload declarations
          ((not (annotation-active?))
           (mark-local-definitions! (declared-names s) annot-ht) ; (declare x) , (def x) ... at the current level
           (process-lst-for-annot s
                                  ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                  #;(cons (make-hash-table)
                                        annot-ht-lst)
                                  (cons (new-level-table s) ; the parameters , the variables of a do loop ... are local bindings
                                        annot-ht-lst)))

          ; (case key ((datum ...) expr ...) ... (else expr ...)) : the lists of datums are data, only the key and the
          ; expressions are annotated (without this : (case op ((+) 'add) ((*) 'mul)) became (case op (0 'add) (1 'mul)))
          ((and (CASE-op? proc/mac)
                (pair? (cdr s)))
           (cons proc/mac
                 (cons (annot (second s)
                              ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                              (cons (make-hash-table) annot-ht-lst)) ; the key
                       (map-in-order (λ (clause) ; (datums expr ...) or (else expr ...)
                                       (if (pair? clause)
                                           (cons (first clause) ; the list of datums (or else) : not annotated
                                                 (process-lst-for-annot (rest clause)
                                                                        ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                                                        (cons (make-hash-table) annot-ht-lst)))
                                           clause))
                                     (cddr s)))))

          ; $bracket-apply$ dispatching with type , example: ($bracket-apply$ T i1)
          ; note: in an expression assignment like {V[i] <- W[j]} this should work both for LHS (V) and RHS (W)
          ((BRACKET-APPLY-op? proc/mac)
           ; a1 should be arg 1 : first argument
           {a1 <+ (cadr s)} #| no need to recall-annot a1 separately here : a1 is (car (cdr s)), so it is already
                            annotated as part of the (recall-annot (cdr s) ...) call below (its result, a1_N, is
                            what actually gets used in the rewritten expression). find-type on the other hand must
                            receive the raw, un-annotated a1 : its isBRACKET-APPLY*? branch looks up nested
                            bracket-apply sub-expressions in annot-ht-lst by exact shape, and those entries were
                            stored (via store-types/update-type) under the generic $bracket-apply$ tag as written
                            in the source ; if a1 were rewritten to a specialized $bracket-apply-vector$/-string$/...
                            tag first, that lookup would silently miss for nested indexing like T[i][j]. |#
           ;(display "annot+.rkt : parse-expressions (case bracket-apply): s=" err-port) (display s err-port) (newline err-port)
           {tip-a1 <+ (type-base (find-type a1 ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))} ; #(T) -> vector? , (T) -> list?
           ;(display "annot+.rkt : parse-expressions (case bracket-apply): tip-a1=" err-port) (display tip-a1 err-port) (newline err-port)
           ; specialize other $bracket-apply$ types
           {a1_N <+ (recall-annot (cdr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)} ; we must recall annot() on the arguments also
           (cond ((eq? tip-a1 'vector?)
                  ;(display "annot+.rkt : parse-expressions (case bracket-apply):  tip-a1 = 'vector?" err-port) (newline err-port)
                  ;(display "annot+.rkt : parse-expressions (case bracket-apply):  (cdr s) = " err-port) (display (cdr s) err-port) (newline err-port)
                  #;(cons '$bracket-apply-vector$
                        a1_N)
                  ; the indexes typed ℕ or ℤ (one index , or two for a vector of vectors M[i j]) : the specialized accesses
                  (cons (case (and (memv (square-brackets-nb-indexes s) '(1 2))
                                   (indexes-kind s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                          ((natural) '$bracket-apply-vector-natural$)
                          ((integer) '$bracket-apply-vector-integer$)
                          (else '$bracket-apply-vector$))
                        a1_N)) ; we must recall annot() on the arguments also
                 ; flomat removed from Scheme+ (sci requires libgfortran) : a flomat uses overload-square-brackets
                 #;((eq? tip-a1 'flomat?)
                  (cons '$bracket-apply-flomat$
                        a1_N))
                 ((eq? tip-a1 'string?)
                  #;(cons '$bracket-apply-string$
                        a1_N)
                  (cons (case (and (eqv? (square-brackets-nb-indexes s) 1) ; one index typed ℕ or ℤ : the specialized accesses
                                   (indexes-kind s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                          ((natural) '$bracket-apply-string-natural$)
                          ((integer) '$bracket-apply-string-integer$)
                          (else '$bracket-apply-string$))
                        a1_N))
                 ((eq? tip-a1 'hash-table?)
                  (cons '$bracket-apply-hash-table$
                        a1_N))
                 ((eq? tip-a1 'array?)
                  (cons '$bracket-apply-array$
                        a1_N))
                 ; square brackets overloaded with overload-square-brackets for the type of a1 : the getter (or in an
                 ; assignment the setter) is chosen from the number of indexes by $bracket-apply-overloaded$ at expansion
                 ; {Mv[i j]} --> ($bracket-apply-overloaded$ ((matrix-vect-ref matrix-vect-set! 2) ...) Mv i j) --> (matrix-vect-ref Mv i j)
                 ((pair? (overloaded-square-brackets-for tip-a1))
                  (cons '$bracket-apply-overloaded$
                        (cons (map-in-order (λ (ovrld) (list (first ovrld) (second ovrld) (cadddr ovrld))) ; (getter setter nb-indexes)
                                            (overloaded-square-brackets-for tip-a1))
                              a1_N)))
                 (else
                   (cons proc/mac ; fall back on $bracket-apply$
                         a1_N)))) ; recalled annot


          ; search now for a possible overload stored in the hash tables
          
          ; by default * + etc will be overloaded without explicit declaration in code
          ; this allow fl* to be used, distingo on exact/inexact, flonum/fixnum ...

          (exec
           {argus <+ (rest s)}
           {nb-argus <+ (length argus)})

          ; {x ²} of a float --> (fl* x x) : see square-of-flonum (before the general path of the operators)
          (exec {flonum-square <+ (square-of-flonum s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)})
          (flonum-square flonum-square)

          ; an integer literal with a float : {x * 2} --> (fl* x 2.0) , see float-literal (the expression with the literal
          ; written as a float follows then the usual path)
          (exec {with-float-literal <+ (float-literal s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)})
          (with-float-literal (annot with-float-literal ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
          
          ; binary and n-ary

          ; when operator is both defined binary and n-ary, we must check the two cases:
          ((and (hash-table-exists? ovrld-ht-n-ary proc/mac)
                (hash-table-exists? ovrld-ht proc/mac)) #| note at this point proc/mac is the symbol of a procedure/operator proc/mac could not be here the result of a procedure
           call, if it was and the result would be an overloaded procedure/operator , result of something like
           (procedure-returning-an-overloaded-procedure) this would be parsed later in the 'else block and would be
           dispatched at runtime as evaluation is made at runtime |#
           (display "annot+.rkt : parse-expressions : hash-table-exists (n-ary and binary) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
           ; actually we use only annoted parsed args but we let the normal stuff computed too
           
           ;  we recall annot() on the arguments list first ? unless some annotation type in the arguments itself , that pushes the limits a bit far away for very poor use case
           {argus-after-annot <+ (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}

           ; get the arguments, we should find their type
           ; note: ;(display args (current-error-port)) ; args exist in Scheme+/operators --> error, solution use argus as variable name, not args !
           (display "annot+.rkt : parse-expressions (n-ary and binary) : argus=" err-port) (display argus err-port) (newline err-port)
           
           {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
					argus)} ; first find the type of arguments
           {args-types-after-annot <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
						    argus-after-annot)} ; first find the type of arguments
           (display "annot+.rkt : parse-expressions (n-ary and binary) : (result) argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions : args-types=" err-port) (display args-types err-port) (newline err-port)
           
           ; now rewrite expression with non overloaded operator/procedure
           ; we need to check the arg types match the overloaded ones from hash table
           {ovrld-n-ary <+ ovrld-ht-n-ary[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
           {ovrld-binary <+ ovrld-ht[proc/mac]}
           ; search if there exist some matching types -> return a procedure
           (display "annot+.rkt : parse-expressions (n-ary and binary) : ovrld-n-ary=" err-port) (display ovrld-n-ary err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary and binary) : ovrld-binary=" err-port) (display ovrld-binary err-port) (newline err-port)

           ; no more really used:
           {proc <+ (or (find-if-exists-matching-types-with-ovrld-n-ary ovrld-n-ary args-types)
                        (nb-argus = 2 and (find-if-exists-matching-types-with-ovrld ovrld-binary args-types)))} ; 2 args for binary
            
           (display "annot+.rkt : parse-expressions (n-ary and binary) : proc=" err-port) (display proc err-port) (newline err-port)
       
           #| this could be used, we check if the types of arguments have changed after annot() call (generally being global sexpr type) 
           it could have changed in the previous version because annot rewrite expressions taking in account the possible overload
           and then the symbols of overloaded functions where not in the hash table keys, now they are and so find-type
           returns the good type even for overloaded functions, so the types no more change after recalling annot
           the only difference in the background is that at first find-type call the type could be deduce with hash-table
           and for litterals at second find-type the type could be deduce directly with the resulting computed numerically
           litteral value as annot finally computes litterals for numbers at least |#
     
           (display "annot+.rkt : parse-expressions (n-ary and binary) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary and binary) : argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary and binary) : args-types=" err-port) (display args-types err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary and binary) : proc=" err-port) (display proc err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary and binary) : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary and binary) : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)
           
          
           (when (not (equal? argus argus-after-annot))
             (when err-flag-args-changed
               (error "annot+.rkt : parse-expressions (n-ary and binary) : warning args changed after annot."))
             (display "annot+.rkt : parse-expressions (n-ary and binary) : warning args changed after annot." err-port) (newline err-port))
           
           (when (not (equal? args-types args-types-after-annot))
             (when err-flag-type-changed
               (error "annot+.rkt : parse-expressions (n-ary and binary) : warning args types changed after annot."))
             (display "annot+.rkt : parse-expressions (n-ary and binary) : warning args types changed after annot." err-port) (newline err-port))

           ; call operator-associativity-recursion here
           (operator-associativity-recursion proc/mac argus-after-annot args-types-after-annot
                                             ovrld-n-ary ovrld-binary
                                             ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)        
           )


          ;;;;;;;;
          ; n-ary
          ;;;;;;;;
          
          ; the n-ary procedure/operator is overloaded (exist in hash table of procedures and operators being n-ary ,example: +)
          ((hash-table-exists? ovrld-ht-n-ary proc/mac) #| note at this point proc/mac is the symbol of a procedure/operator proc/mac could not be here the result of a procedure
           call, if it was and the result would be an overloaded procedure/operator , result of something like
           (procedure-returning-an-overloaded-procedure) this would be parsed later in the 'else block and would be
           dispatched at runtime as evaluation is made at runtime |#
           (display "annot+.rkt : parse-expressions : hash-table-exists (n-ary) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)

           ;  we must recall annot() on the arguments first: not a good idea
           ;{argus := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}

           ; get the arguments, we should find their type
           ; note: ;(display args (current-error-port)) ; args exist in Scheme+/operators
           (display "annot+.rkt : parse-expressions (n-ary) : argus=" err-port) (display argus err-port) (newline err-port)
           {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
					argus)} ; first find the type of arguments
           (display "annot+.rkt : parse-expressions (n-ary) : (result) argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions : args-types=" err-port) (display args-types err-port) (newline err-port)
           ; now rewrite expression with non overloaded operator/procedure
           ; we need to check the arg types match the overloaded ones from hash table
           {ovrld <+ ovrld-ht-n-ary[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
           ; search if there exist some matching types -> return a procedure
           {proc <+ (find-if-exists-matching-types-with-ovrld-n-ary ovrld args-types)}
           (display "annot+.rkt : parse-expressions (n-ary) : proc=" err-port) (display proc err-port) (newline err-port)

           ; if result is #f we should try to divide the expression by associativity ,find the operators/procedures and reconstruct the expression accordingly , schema: (+ a b c) = (+1 (+2 a b) c) , so it is possible if we know, of course the types of a,b,c and we can infer the type of +2 from a and b and then possibly the type of +1 from the return type of +2 and from the type of c, so we can find the right procedures/operators for +1 and +2 and reconstruct the expression accordingly
           
           ;  we must recall annot() on the arguments now:
           {argus-after-annot <+ (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
           {args-types-after-annot <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
						    argus-after-annot)} ; again find the types of arguments

           (display "annot+.rkt : parse-expressions (n-ary) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary) : argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary) : args-types=" err-port) (display args-types err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary) : proc=" err-port) (display proc err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary) : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (n-ary) : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)

           (when (not (equal? args-types args-types-after-annot))
             (when err-flag-type-changed
               (error "annot+.rkt : parse-expressions (n-ary) : warning args types changed after annot."))
             (display "annot+.rkt : parse-expressions (n-ary) : warning args types changed after annot." err-port) (newline err-port))


	    ; call operator-associativity-recursion here
           (operator-associativity-recursion proc/mac argus-after-annot args-types-after-annot
                                             ovrld ; n-ary
					     #f ; no binary
                                             ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
           
           ; search if there exist some matching types -> return a procedure
           ;{proc-after-annot := (find-if-exists-matching-types-with-ovrld-n-ary ovrld args-types-after-annot)}
           ;(display "annot+.rkt : parse-expressions (n-ary) : proc-after-annot=" err-port) (display proc-after-annot err-port) (newline err-port)

           
           ; now rewrite expression
           ;(rewrite/compute-operational-expression proc proc/mac argus-after-annot args-types) ; rewrite also because arguments could have changed
	   )  
          
          ;;;;;;;;;
          ; binary
          ;;;;;;;;;

          ; the binary operator is overloaded (exist in hash table of procedures and operators binary,example: +)
          ((hash-table-exists? ovrld-ht proc/mac) #| note at this point proc/mac is the symbol of a procedure/operator proc/mac could not be here the result of a procedure
         call, if it was and the result would be an overloaded procedure/operator , result of something like
         (procedure-returning-an-overloaded-procedure) this would be parsed later in the 'else block and would be
         dispatched at runtime as evaluation is made at runtime |#
           (display "annot+.rkt : parse-expressions : hash-table-exists (unary or binary) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)

           ;  we must recall annot() on the arguments first: Warning: this is not a good idea because it could change the type of arguments and we would not be able to find the right procedure/operator in the hash table, so we must find the type of arguments first and then recall annot() on them after we have found the right procedure/operator
           ; {argus := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}

           ; get the arguments, we should find their type
           ; note: ;(display args (current-error-port)) ; args exist in Scheme+/operators
           (display "annot+.rkt : parse-expressions  (unary or binary) : argus=" err-port) (display argus err-port) (newline err-port)
           {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                               argus)} ; first find the type of arguments
           (display "annot+.rkt : parse-expressions (unary or binary) : (result) argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions : args-types=" err-port) (display args-types err-port) (newline err-port)
           ; now rewrite expression with non overloaded operator/procedure
           ; we need to check the arg types match the overloaded ones from hash table
           {ovrld <+ ovrld-ht[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
           ; search if there exist some matching types -> return a procedure
           ;(display "annot+.rkt : parse-expressions :(unary or binary) : ovrld=" err-port) (display ovrld err-port) (newline err-port)
           {proc <+ (find-if-exists-matching-types-with-ovrld ovrld args-types)}
           (display "annot+.rkt : parse-expressions  (binary) : proc=" err-port) (display proc err-port) (newline err-port)

           ;  we must recall annot() on the arguments now:
           {argus-after-annot <+ (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
           {args-types-after-annot <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                           argus-after-annot)} ; again find the types of arguments

           (display "annot+.rkt : parse-expressions (binary) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (binary) : argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (binary) : args-types=" err-port) (display args-types err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (binary) : proc=" err-port) (display proc err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (binary) : argus-after-annot=" err-port) (display argus-after-annot err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions (binary) : args-types-after-annot=" err-port) (display args-types-after-annot err-port) (newline err-port)

           (when (not (equal? args-types args-types-after-annot))
             (when err-flag-type-changed
               (error "annot+.rkt : parse-expressions (binary) : warning args types changed after annot."))
             (display "annot+.rkt : parse-expressions (binary) : warning args types changed after annot." err-port) (newline err-port))

           ; search if there exist some matching types -> return a procedure
           {proc-after-annot <+ (find-if-exists-matching-types-with-ovrld ovrld args-types-after-annot)}
           (display "annot+.rkt : parse-expressions (binary) : proc-after-annot=" err-port) (display proc-after-annot err-port) (newline err-port)

           
           ; now rewrite expression
           (rewrite/compute-operational-expression proc proc/mac argus-after-annot args-types)) ; rewrite also because arguments could have changed


          ; procedures only (not operator):
          ; the procedure is overloaded (exist in hash table of procedures and operators) but here it is a procedure not an operator
          ((hash-table-exists? ovrld-ht-proc proc/mac) #| note at this point proc/mac is the symbol of a procedure/operator proc/mac could not be here the result of a procedure
         call, if it was and the result would be an overloaded procedure/operator , result of something like
         (procedure-returning-an-overloaded-procedure) this would be parsed later in the 'else block and would be
         dispatched at runtime as evaluation is made at runtime |#
           (display "annot+.rkt : parse-expressions : hash-table-exists (ovrld-ht-proc) : proc/mac=" err-port) (display proc/mac err-port) (newline err-port)

           ;  we must recall annot() on the arguments first:
           ;{argus := (recall-annot (rest s) ovrld-ht annot-ht-lst)} ; get the arguments, we should find their type
           ; note: ;(display args (current-error-port)) ; args exist in Scheme+/operators
           (display "annot+.rkt : parse-expressions  (procedure) : argus=" err-port) (display argus err-port) (newline err-port)
           {args-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                               argus)} ; first find the type of arguments
           (display "annot+.rkt : parse-expressions (procedure) : (result) argus=" err-port) (display argus err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions : args-types=" err-port) (display args-types err-port) (newline err-port)
           ; now rewrite expression with non overloaded operator/procedure
           ; we need to check the arg types match the overloaded ones from hash table
           {ovrld <+ ovrld-ht-proc[proc/mac]} ; get overloaded data, ex: (+ → (((vector?) vector-append (vector? vector?)) ((integer?) orig-+ (integer? integer?)) ((number?) orig-+ (number? number?)) ((real?) orig-+ (real? real?)) ((rational?) orig-+ (rational? rational?)) ((complex?) orig-+ (complex? complex?))))
           ; search if there exist some matching types -> return a procedure
           {proc <+ (find-if-exists-matching-types-with-ovrld ovrld args-types)}
           (display "annot+.rkt : parse-expressions  (procedure) : proc=" err-port) (display proc err-port) (newline err-port)

           ;  we must recall annot() on the arguments now:
           {argus := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)} 

           ; now rewrite expression
           (if proc
               (cons proc argus)
               (cons proc/mac argus))) ; rewrite also because arguments could have changed


          ; test if proc/mac is an overloaded procedure or operator and parse it and modify it accordingly
          ; but then we should probably first parse for annotation the arguments of proc/mac function
          ; note: not necessary, it could complex things $bracket-apply$ -> $bracket-apply-vector$
          ; as $bracket-apply-vector$ is not existing in SRFI (only $bracket-apply$ exists)
          ; we do not need to know it was a vector here, it could have been an hash table too, we do not care of it
          ;( because then we could have better information about the type:
          ; example of an argument '($bracket-apply$ z i) parsed in '($bracket-apply-vector$ z i)
          ; which clearly indicates it is a vector. And then we should construct the resulting expression.)
          ;
          ; so we could recreate the expression with proc/mac (+ arg ....) in (vector-append arg ....)
          ; and then only after parse for annotation the arguments arg ....
          ;
          ; update: in fact ,this does matter (before or after)
          ; example of overloaded function: we must study exactly which function is called in the argument (update: this should be what find-type focus on)
          ; because the function will help to know what is the type of its return value which we search for
          ; so sometimes it is better to parse the arguments before (overloaded function call) ,sometimes not (brackett apply)
          ; but as bracket-apply should be parsed/studied we should adapt code to this:
          ; not only check with bracket-apply but possibly with bracket-apply-vector,etc...

          #| type annotation for vector assignment (both container and expression) : (<- ($bracket-apply$ T1 2 :) T2)
        but only when expr is a vector declared with annotation , really useful only for slicing and vector-copy!
        optimization |#

          ; type inference for assignment and definitions ,example: {a := b := c := 3.5} --> (:= a b c 3.5)
          ; also multiple values take in account : ex: {(a b c) <- (x y z) <- (values 1 2 3)} --> (<- (a b c) (x y z) (values 1 2 3))

          ; when the type of RmostHS is a numerical type and the type declared for a LHS another one ((type flonum? x)
          ; then {x := i + 1} with a fixnum i) , the expression is not rewritten with the operators of the type of the LHS :
          ; an assignment of Scheme converts nothing , fl+ would fail on a fixnum and (fl+ (fx->fl i) 1.0) would give 2.0
          ; where Scheme gives 2 (the annotated program must give the same result as without annotation) , and the type
          ; annotation specializes the code , it never converts the values ; the declaration is false there , the
          ; inferred type replaces it (update-type) , as it is the type of the value
	  
          ; specialization of the procedure passed to vector-map, map, apply when the types of the elements are known :
          ; (vector-map * v1 v2) -> (vector-map fl* v1 v2) , (apply + (vector->list ...)) -> (apply fl+ ...)
          (exec {ho-expr <+ (and (or (memq proc/mac '(vector-map map apply for-each vector-for-each))
                                     (container-higher-order? proc/mac)) ; folds , reduces , build-list , build-vector
                                 (or (specialize-higher-order-lambda s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst) ; a lambda passed
                                     (specialize-higher-order s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))})

          (ho-expr ho-expr)

          ((ASSIGNMENT-op? proc/mac) ; <- , := , ...

           (display "annot+.rkt : parse-expressions : ASSIGNMENT " err-port) (newline err-port)
           ; argus : (a b c 3.5)
           {rev-args <+ (reverse argus)} ; (3.5 c b a)
           {RmostHS <+ (car rev-args)} ; 3.5     Right Most Hand Side
           {LHS <+ (cdr rev-args)} #| (c b a) , warning LHS could be a list and also could a list of lists (multiple values returned)
              ,  Left Hand Side |#
           ;(display "annot+.rkt : parse-expressions : annot-ht-lst =" err-port) (display-hashtable-list annot-ht-lst err-port) (newline err-port)
           (display "annot+.rkt : parse-expressions : RmostHS=" err-port) (display RmostHS err-port) (newline err-port)
           {RmostHS-type <+ (find-type RmostHS ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)} ; find the type
           ; an access to an object with overloaded square brackets in the LHS : {Mv[0] := (vector 7 8 9)} , no type stored
           ; for Mv[] and no <-vector-vector* (the setter is chosen by $bracket-apply-overloaded$)
           {LHS-overloaded? <+ (any (λ (x) (overloaded-square-brackets-access? x ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                    LHS)}
           ; the arguments are annotated with the types of before the assignment : the right hand side is computed with
           ; the old values ({k := (quotient k 2)} : the k of (quotient k 2) has the type of before)
           {argus := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
           ; update types
           (display "annot+.rkt : parse-expressions : RmostHS-type=" err-port) (display RmostHS-type err-port) (newline err-port)
           (when (and RmostHS-type ; type is found
                      (not (eq? RmostHS-type 'sexpr))) ; but type is not sexpr
             (if (multiple-values-type? RmostHS-type LHS) ; ex: (integer? complex?) check for multiple values returned (not a list type (T))
                 (map-in-order (λ (L) (map-in-order (λ (x tip) (update-type tip x annot-ht)) ; LHS = ((x y) (a b) ...) deal with multiple values returned
                                  L RmostHS-type))
                      LHS)
                 (map-in-order (λ (x) (unless (overloaded-square-brackets-access? x ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
                                        (update-type RmostHS-type x annot-ht)))
                      LHS)))
           ;  we must recall annot() on the arguments now:
           #;{argus := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)} ; before : after the update of the types
           ; reconstruct expression
           {proc <+ proc/mac} ; the procedure
           (when (and (eq? 'vector? (type-base RmostHS-type)) ; deal with vectors only (also #(T))
                      (not LHS-overloaded?)
                      (not (WALRUS-op? proc/mac))) ; the walrus operator must return the value : it keeps := (generic copy)
             {LHS-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                LHS)} ; find the type of LHS
             (when (every (λ (t) (eq? (type-base t) 'vector?)) ;  check we only have vectors (also #(T))
                           LHS-types)
               {proc := '<-vector-vector*}))
           (when (and (eq? 'string? RmostHS-type) ; deal with strings only
                      (not LHS-overloaded?)
                      (not (WALRUS-op? proc/mac))) ; the walrus operator must return the value
             {LHS-types <+ (map-in-order (lambda (a) (find-type a ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))
                                LHS)}
             (when (every (λ (t) (eq? t 'string?)) ;  check we only have strings
                           LHS-types)
               {proc := '<-string-string*}))
           {rexpr <+ (cons proc argus)}
           (display "annot+.rkt : parse-expressions : rexpr=" err-port) (display rexpr err-port) (newline err-port)
           rexpr)

          ((or (DEFINE-op? proc/mac) ; <+ , ...
               (and (DEFINITION-op? proc/mac) ; (define x ...) not (define (f ...) ...)
                    (symbol? (cadr s))))

           (display "annot+.rkt : parse-expressions : DEFINITION (<-,:=,..) or DEFINE" err-port) (newline err-port)
           ;argus :  (a b c 3.5)
           {rev-args <+ (reverse argus)} ; (3.5 c b a)
           {RmostHS <+ (car rev-args)} ; 3.5     Right Most Hand Side
           {LHS <+ (cdr rev-args)} #| (c b a) ,warning LHS could be a list and also could be a list of lists (multiple values returned)
             , LSH : Left Hand Side |#
           {RmostHS-type <+ (find-type RmostHS ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)} ; find the type
           ; update types
           (display "annot+.rkt : parse-expressions : RmostHS-type=" err-port) (display RmostHS-type err-port) (newline err-port)
           (when (and RmostHS-type ; type is found
                      (not (eq? RmostHS-type 'sexpr))) ; but type is not sexpr
             (if (multiple-values-type? RmostHS-type LHS) ; ex: (integer? complex?) check for multiple values returned (not a list type (T))
                 (map-in-order (λ (L) (map-in-order (λ (x tip) (update-type tip x annot-ht)) ; LHS = ((x y) (a b) ...) deal with multiple values returned
                                  L RmostHS-type))
                      LHS)
                 (map-in-order (λ (x) (update-type RmostHS-type x annot-ht))
                      LHS)))
           ; a definition whose value has an unknown type : the defined names are local bindings without type , they
           ; hide the outer variables of the same name (unless they have already a type at this level)
           (unless (and RmostHS-type
                        (not (eq? RmostHS-type 'sexpr)))
             (mark-local-definitions! (lhs-symbols LHS) annot-ht))
           ;  we must recall annot() on the arguments now:
           {argus := (recall-annot argus ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)}
           ; reconstruct expression
           {rexpr <+ (cons proc/mac argus)}
           (display "annot+.rkt : parse-expressions : rexpr=" err-port) (display rexpr err-port) (newline err-port)
           rexpr)

          ; the clause (module ...) has been moved before the clause (not (annotation-active?)) : ANNOTATION-ON is written
          ; in the body of the module , so the annotation is not active yet when the form (module ...) is reached and
          ; the clause (not (annotation-active?)) took it , the provided types were never copied to the outer table
          #;((eq? proc/mac 'module)
           {mod-level-ht := (new-level-table s)}
           {mod-body := (cdddr s)}
           {processed := (process-lst-for-annot s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                                (cons mod-level-ht annot-ht-lst))}
           {(provided-names provided-all?) := (collect-provided-names mod-body)}
           (export-provided-types! mod-level-ht annot-ht provided-names provided-all?)
           processed)

	  
          #| (let ((name init) ...) body ...) , named let (let loop ((name init) ...) body ...) ,
          (let* ...) , (letrec ...) , (letrec* ...) : bound-names/new-level-table already hide, for the
          body, any outer type of the same name (a plain let shadows, it must not let (fx+ a b) happen for
          an outer-flonum a/b that a let re-binds to fixnums) -- but that is ALL they ever did (deliberately
          : Scheme+ itself only ever writes <-/:=/<+, never let), so a plain R7RS let written by a user had
          its own bound names typed nowhere, and its body could never specialize on them. Here each binding's
          type is inferred from its init expression and stored for the body (and, for let*/letrec/letrec*,
          for the later bindings of the very same form too, since those may refer to the earlier ones) --
          exactly the same way (find-type + update-type) a {name := init} assignment already does. |#

	  #|  let/let*/letrec/letrec* (et let nommé) ont maintenant leur propre clause dans parse-expressions
	  (annot+.rkt) : chaque binding (nom init) est typé depuis son init (find-type + update-type, comme un {nom := init}
	  normal), visible dans le corps, et pour let*/letrec/letrec* aussi dans les inits des bindings suivants du même
	  let. Avant, seul le "cloisonnement" (ne pas laisser fuiter le type d'une variable externe du même nom) était géré ;
	  le binding lui-même n'était jamais typé, le corps ne pouvait donc jamais se spécialiser dessus. Testé : let simple,
	  let* séquentiel, letrec, let nommé (boucle) ; variable externe toujours correcte après le let ; voir
	  examples/test-let+.rkt. |#

          ((memq proc/mac '(let let* letrec letrec*))
           (declare named? bindings body mod-level-ht sequential? level-for-inits processed-bindings body-processed)
           (declare name init init-type init-recalled) ; transient, reused for each binding of the map below
           {named? := (and (eq? proc/mac 'let) (symbol? (cadr s)))} ; (let loop ((n i) ...) body ...)
           {bindings := (cond (named? (caddr s)) (else (cadr s)))}
           {body := (cond (named? (cdddr s)) (else (cddr s)))}
           #;{mod-level-ht := (new-level-table s)} ; already marks every bound name (incl. the loop name) local-without-type
           ; let* : the init of a binding sees only the bindings before it and the outer levels , the table starts empty
           ; and each name is marked after its init (the other forms : every bound name is marked from the start)
           {mod-level-ht := (if (eq? proc/mac 'let*) (make-hash-table) (new-level-table s))}
           {sequential? := (memq proc/mac '(let* letrec letrec*))} ; later bindings can see earlier ones in THIS form
           {level-for-inits := (cond (sequential? (cons mod-level-ht annot-ht-lst)) (else annot-ht-lst))}
           {processed-bindings :=
            (map-in-order (λ (b)
                            (cond ((and (pair? b) (symbol? (car b)) (pair? (cdr b)))
                                   {name := (car b)}
                                   {init := (cadr b)}
                                   {init-type := (find-type init ovrld-ht ovrld-ht-n-ary ovrld-ht-proc level-for-inits)}
                                   #;(when (and init-type (not (eq? init-type 'sexpr)))
                                     (update-type init-type name mod-level-ht)) ; else : stays local-without-type
                                   #;{init-recalled := (car (recall-annot (list init) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc level-for-inits))}
                                   #;{init-recalled := (annot init ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons (make-hash-table) level-for-inits))} ; one expression : annot , as for the key of case
                                   (cond ((eq? proc/mac 'let*) ; the init is annotated with the types of before its binding
                                          {init-recalled := (annot init ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons (make-hash-table) level-for-inits))}
                                          (mark-local-binding! name mod-level-ht)
                                          (when (and init-type (not (eq? init-type 'sexpr)))
                                            (update-type init-type name mod-level-ht))) ; else : stays local-without-type
                                         (else ; let , letrec , letrec* : the binding is already visible (letrec) or not seen (let)
                                          (when (and init-type (not (eq? init-type 'sexpr)))
                                            (update-type init-type name mod-level-ht)) ; else : stays local-without-type
                                          {init-recalled := (annot init ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons (make-hash-table) level-for-inits))})) ; one expression : annot , as for the key of case
                                   (list name init-recalled))
                                  (else b))) ; not a (name init) pair : leave untouched
                          bindings)}
           {body-processed := (process-lst-for-annot body ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons mod-level-ht annot-ht-lst))}
           (cond (named? (list* proc/mac (cadr s) processed-bindings body-processed))
                 (else (list* proc/mac processed-bindings body-processed))))

          ; otherwise we will enter the list :
          ; the elements of expression must be parsed recursively for annotation and we should return the expression containing the parsed elements
          ; (do ((var init step ...) ...) (test expr ...) body ...) of R7RS : the inits are computed before the loop , they
          ; are annotated with the outer levels ; the (type ...) of the body are known from the start by the steps , the
          ; test , the expressions of the result and the body , at the level of the do (as for the body of a lambda in
          ; lambda-return-type) : (do ((k 0 {k + 1})) ({k = n}) (type ℕ k) ...)
          ((and (eq? proc/mac 'do) (do-R7RS? s))
           (declare do-level specs-processed)
           {do-level := (new-level-table s)} ; the variables of the loop are local bindings
           (for-each (λ (e)
                       (when (and (pair? e) (is-type? (car e)))
                         (store-types (second e) (cddr e) do-level)))
                     (cdddr s))
           {specs-processed :=
            (map-in-order (λ (spec) ; (var init step ...)
                            (if (pair? (cdr spec))
                                (list* (car spec)
                                       (annot (cadr spec) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons (make-hash-table) annot-ht-lst))
                                       (map-in-order (λ (step)
                                                       (annot step ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                                              (cons (make-hash-table) (cons do-level annot-ht-lst))))
                                                     (cddr spec)))
                                spec))
                          (cadr s))}
           (list* proc/mac
                  specs-processed
                  (process-lst-for-annot (caddr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons do-level annot-ht-lst))
                  (process-lst-for-annot (cdddr s) ovrld-ht ovrld-ht-n-ary ovrld-ht-proc (cons do-level annot-ht-lst))))

          ; otherwise we will enter the list :
          ; the elements of expression must be parsed recursively for annotation and we should return the expression containing the parsed elements
          (else ; prepare a new has table for annotation of the next sub nested level of AST (abstract syntax tree)
            (mark-local-definitions! (declared-names s) annot-ht) ; (declare x) , (def x) ... at the current level
            #;(define level (new-level-table s)) ; the parameters , the variables of a do loop ... are local bindings
            ; a do of R7RS : the (type ...) of its body are known from the start , by its steps and its test too
            ; (as for the body of a lambda in lambda-return-type) : (do ((k 0 {k + 1})) ({k = n}) (type ℕ k) ...)
            #;(when (and (eq? proc/mac 'do) (do-R7RS? s))
              (for-each (λ (e)
                          (when (and (pair? e) (is-type? (car e)))
                            (store-types (second e) (cddr e) level)))
                        (cdddr s)))
            ; this will recall annot :
            (process-lst-for-annot s
                                   ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                                   #;(cons (make-hash-table)
                                         annot-ht-lst)
                                   (cons (new-level-table s) ; the parameters , the variables of a do loop ... are local bindings
                                         annot-ht-lst)
                                   #;(cons level annot-ht-lst)))))




  ; recall annot on a list of sexpr
  (define (recall-annot L ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (map-in-order (λ (s) (annot s
                       ovrld-ht ovrld-ht-n-ary ovrld-ht-proc ; keep them the same because in arguments of function we can not have overload of operators or procedures (it is only at the toplevel)
                       (cons (make-hash-table) annot-ht-lst))) ; chained hash table in list in case we have some type annotation in arguments !
         L))


  ; at some point for computation we should have type anotation parsed
  ; only then we can compute the expression

  ; annot: parse list, pair,... -> if necessary call parse-expressions: rewrite (recreate*) -> process-lst-for-annot which process whole list deeply with: -> annot (and we are back at beginning !!!) and process also with annot when necessary
  ;                             -> or recall annot on car and cdr of pair

  ; the types that the annotation knows for the expression s , for (:print-type s) at the REPL (as :print-type of Typed
  ; Racket) : the list of the lines to display , '() when it is unknown. For a symbol : the signature
  ; (param ... -> ret ...) of a procedure declared with (type [(ret ...) procedure? (param ...)] f) , else its overloads
  ; (an operator , or a procedure given to overload-operator / overload-procedure : one signature per overload) , else
  ; the type found by find-type. s is not evaluated , the tables are not modified.
  (define (type-for-repl s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    {decl <+ (and (symbol? s) (search-annot-hash-tables s annot-ht-lst))}
    {overloads <+ (if (symbol? s)
                      (delete-duplicates (append (overload-signatures s ovrld-ht-proc #f)
                                                 (overload-signatures s ovrld-ht #f)
                                                 (overload-signatures s ovrld-ht-n-ary #t)))
                      '())}
    (cond ((and (list? decl) {(length decl) = 3} (eq? (second decl) 'procedure?)) ; a signature ((ret ...) procedure? (param ...))
           (list (append (third decl) '(->) (first decl))))
          ((not (null? overloads)) overloads)
          (else
           {t <+ (with-handlers ((exn:fail? (λ (e) #f))) ; an expression that find-type can not process : unknown
                   (find-type s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst))}
           (if (and t (not (memq t '(sexpr local-binding-without-type))))
               (list t)
               '()))))

  ; the signatures of the overloads stored for the key name in one table of overloads : an entry
  ; ((ret ...) proc (pred ...)) or (proc (pred ...)) (no return type : ?) gives (pred ... -> ret ...) ; in the table of
  ; the n-ary overloads (n-ary? = #t) the predicate is the one of every argument : (pred ... -> ret ...)
  (define (overload-signatures name ovrld-table n-ary?)
    (if (hash-table-exists? ovrld-table name)
        (map-in-order (λ (entry)
                        (append (last entry)
                                (if n-ary? (list (string->symbol "...")) '())
                                '(->)
                                (if {(length entry) = 3} (first entry) '(?))))
                      {ovrld-table[name]})
        '()))

  ; annotate a level of expression
  ; we also need for agument some hash tables , possibly multiples
  ; s = (s1 s2 ...)
  ; the forms made by the annotation keep the location of the form they come from
  (define (annot s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    #;(relink-form! (annot-rewrite s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst) s)
    (let* ((saved (enter-form! s)) ; an error of the pass is reported at the location of the form being processed
           (result (annot-rewrite s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)))
      (leave-form! saved)
      (relink-form! result s)))

  (define (annot-rewrite s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)
    (cond ((null? s) '()) ; empty list
          ((list? s) ; we must process all the elements at the same level of recursion , so not by diving in a recursion over the rest of list
           ; do we have an interesting thing to parse and modify? when expression itself should be modified
           ; now in a special procedure as it should be bigger and bigger in the future
           (parse-expressions s ovrld-ht ovrld-ht-n-ary ovrld-ht-proc annot-ht-lst)) ; for now we are at the same level in the AST (abstract syntax tree)
          ((pair? s) ; pair or list , with list processed above we should not reach this part often (not for lists)
           (cons (annot (car s)
                        ovrld-ht ovrld-ht-n-ary ovrld-ht-proc ; added 2nd argument
                        (cons (make-hash-table) annot-ht-lst)) ; added third argument, constructed with a new hash table and check for #<void> return
                 ; in fact this code is only reached before an error , example: (1 . 2) is a pair and will result in an error at runtime if it is unquoted
                 ;  #%app: bad syntax in: (#%app 1 . 2)
                 (annot (cdr s)
                        ovrld-ht ovrld-ht-n-ary ovrld-ht-proc
                        (cons (make-hash-table) annot-ht-lst)))) ; added third argument, constructed with a new hash table and check for #<void> return
          (else s)) ; atom
    ) ; note : we do not need to parse litterals vectors ,scheme do not do it: #((1 . 2)) --> '#((1 . 2))


  

(export
	annot
	type-for-repl
	remove-expr
	init-annot-data
	annotation-active?
	annotation-marker?
	remove-annotation-markers
	remove-types-and-annotation-markers
	insert-top-level-markers-in-module)
 ; end module



;; example from common lisp:
; (let ((tmp (gensym)))
;    ``(lambda (,tmp ,,tmp ,',tmp) ()))
;
; '`(lambda (,tmp ,g444209 ,'g444209) ()) ; same result as in Racket





; annotation test:

;(type string? str str2)
;; {str := "hello"} ; type inference !
;; ;{str[3 :]}
;; (type string? str2)
;; {str2 := (string-append "azertyuiop")} ; type inference too
;; {str2[3 : 5] := str[1 : 3]}
;; ;{str2[4] := str[1]}
;; ;str2
;; {v1 := (vector 1 2 3 4 5 6 7)}
;; (define (foo) (vector -10 -20 -30 -40))
;; ;{v2 := #(-1 -2 -3 -4)} ; immutable !
;; {v2 := (foo)}
;; (type vector? v1 v2 v3)
;; ;{v1[2 : 5] := v2}
;; {v3 := v2[1 :]}
;; {v1[2 : 5] := v2[1 :]}
;; {v1[2 : 5] := v3}
;; {v2[2 : 4] := v1[3 : 5] := v3}
;; (list v1 v2 str2)
;; ;{v1[2]}
