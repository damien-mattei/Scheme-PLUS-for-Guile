;; Translated from Scheme+ for Racket (racket-types.rkt) for Guile.

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

;; Type tables of the procedures of Racket that are not in R7RS (racket/base , racket/math , racket/flonum ,
;; racket/fixnum , racket/list , racket/vector) , read by the type annotation (annot+.rkt) as scheme-types.rkt : scheme-types.rkt is the table of R7RS
;; only , shared by the ports to Guile and Kawa , this one is for Racket only.
;;
;; Each element of the list has the form of scheme-types.rkt :
;;
;;   (name (return-type-predicates ...) procedure? (parameter-type-predicates ...))
;;
;; only the first return type is read by annot+.rkt (scheme-return-type) , for a call without an entry for the types of
;; its arguments in the tables of annot+.rkt.
;;
;; This module has no dependencies; it is pure data.

(define-module (SRFI-105 racket-types)
  #:use-module (Scheme+ racket-compat))

  

  (define racket-types

    '(

      (sqr              (real?) procedure? (real?)) ; the square of Racket (square of R7RS) , was added in annot+.rkt

      (void             (void?) procedure? ((... any?)))

      ;; the flonums (racket/flonum) not in the tables of annot+.rkt : out of their domain they return +nan.0 , never a
      ;; complex ((flsqrt -1.0) is +nan.0)
      ;; the arithmetic : the tables of annot+.rkt keep only the rows of flonums , the return type is given here for all the
      ;; arguments (fl+ returns a flonum or raises an error)
      (fl+              (flonum?) procedure? ((... flonum?)))
      (fl-              (flonum?) procedure? (flonum? (... flonum?)))
      (fl*              (flonum?) procedure? ((... flonum?)))
      (fl/              (flonum?) procedure? (flonum? (... flonum?)))
      #;(flsqrt           (flonum?) procedure? (flonum?))
      (flsqrt           (nonnegative-flonum?) procedure? (flonum?)) ; ℝ⁺ : (flsqrt -1.0) is +nan.0 , in ℝ⁺
      (fllog            (flonum?) procedure? (flonum?))
      (flasin           (flonum?) procedure? (flonum?))
      (flacos           (flonum?) procedure? (flonum?))
      (flexpt           (flonum?) procedure? (flonum? flonum?))
      (->fl             (flonum?) procedure? (exact-integer?))
      (flvector-ref     (flonum?) procedure? (flvector? exact-nonnegative-integer?))
      (flrandom         (flonum?) procedure? (pseudo-random-generator?))
      (flvector-length  (natural-fixnum?) procedure? (flvector?)) ; ℕ
      (fl->exact-integer (exact-integer?) procedure? (flonum?)) ; an exact-integer? , not a fixnum? : (fl->exact-integer 1e30)
      (fl=              (boolean?) procedure? (flonum? (... flonum?)))
      (fl<              (boolean?) procedure? (flonum? (... flonum?)))
      (fl>              (boolean?) procedure? (flonum? (... flonum?)))
      (fl<=             (boolean?) procedure? (flonum? (... flonum?)))
      (fl>=             (boolean?) procedure? (flonum? (... flonum?)))

      ;; the fixnums (racket/fixnum) not in the tables of annot+.rkt
      ;; the arithmetic : the tables of annot+.rkt keep only the rows of fixnums , the return type is given here for all the
      ;; arguments (fx+ returns a fixnum or raises an error , as on a bignum : the annotation does not change it in orig-+)
      (fx+              (fixnum?) procedure? ((... fixnum?)))
      (fx-              (fixnum?) procedure? (fixnum? (... fixnum?)))
      (fx*              (fixnum?) procedure? ((... fixnum?)))
      (fl->fx           (fixnum?) procedure? (flonum?)) ; truncates
      (fxand            (fixnum?) procedure? ((... fixnum?)))
      (fxior            (fixnum?) procedure? ((... fixnum?)))
      (fxxor            (fixnum?) procedure? ((... fixnum?)))
      (fxnot            (fixnum?) procedure? (fixnum?))
      (fxlshift         (fixnum?) procedure? (fixnum? fixnum?))
      (fxrshift         (fixnum?) procedure? (fixnum? fixnum?))
      (fxvector-ref     (fixnum?) procedure? (fxvector? exact-nonnegative-integer?))
      (fxvector-length  (natural-fixnum?) procedure? (fxvector?)) ; ℕ
      (fx=              (boolean?) procedure? (fixnum? (... fixnum?)))
      (fx<              (boolean?) procedure? (fixnum? (... fixnum?)))
      (fx>              (boolean?) procedure? (fixnum? (... fixnum?)))
      (fx<=             (boolean?) procedure? (fixnum? (... fixnum?)))
      (fx>=             (boolean?) procedure? (fixnum? (... fixnum?)))

      ;; the operations of a flonum and a fixnum of Scheme+ (Scheme+/flonum-fixnum) , written by the type annotation
      ;; (the rows of the mixed types of annot+.rkt) : macros , the same results as the generic procedures
      (fl+/fx           (flonum?) macro? (flonum? fixnum?))
      (fx/fl+           (flonum?) macro? (fixnum? flonum?))
      (fl-/fx           (flonum?) macro? (flonum? fixnum?))
      (fx/fl-           (flonum?) macro? (fixnum? flonum?))
      (fl*/fx           (real?) macro? (flonum? fixnum?)) ; (* x 0) is the exact 0
      (fx/fl*           (real?) macro? (fixnum? flonum?))
      (fl//fx           (flonum?) macro? (flonum? fixnum?)) ; (/ x 0) is an error
      (fx/fl/           (real?) macro? (fixnum? flonum?)) ; (/ 0 x) is the exact 0
      (flmin/fx         (flonum?) macro? (flonum? fixnum?))
      (fx/flmin         (flonum?) macro? (fixnum? flonum?))
      (fl</fx           (boolean?) macro? (flonum? fixnum?))
      (fx/fl<           (boolean?) macro? (fixnum? flonum?))
      (fl>/fx           (boolean?) macro? (flonum? fixnum?))
      (fx/fl>           (boolean?) macro? (fixnum? flonum?))
      (fl<=/fx          (boolean?) macro? (flonum? fixnum?))
      (fx/fl<=          (boolean?) macro? (fixnum? flonum?))
      (fl>=/fx          (boolean?) macro? (flonum? fixnum?))
      (fx/fl>=          (boolean?) macro? (fixnum? flonum?))
      (fl=/fx           (boolean?) macro? (flonum? fixnum?))
      (fx/fl=           (boolean?) macro? (fixnum? flonum?))
      (fl≠/fx           (boolean?) macro? (flonum? fixnum?))
      (fx/fl≠           (boolean?) macro? (fixnum? flonum?))

      ;; the byte strings of Racket (the bytevectors of R7RS above are not in racket) : a length in memory is a fixnum , a
      ;; byte is 0 to 255
      (bytes-length     (natural-fixnum?) procedure? (bytes?)) ; ℕ
      (bytes-ref        (natural-fixnum?) procedure? (bytes? exact-nonnegative-integer?)) ; 0 to 255 : ℕ

      ;; the exact roundings (racket/math) : an error for +inf.0 and +nan.0 , an exact integer otherwise
      (exact-round      (exact-integer?) procedure? (rational?))
      (exact-floor      (exact-integer?) procedure? (rational?))
      (exact-ceiling    (exact-integer?) procedure? (rational?))
      (exact-truncate   (exact-integer?) procedure? (rational?))

      ;; the bits of the exact integers
      (bitwise-and      (exact-integer?) procedure? ((... exact-integer?)))
      (bitwise-ior      (exact-integer?) procedure? ((... exact-integer?)))
      (bitwise-xor      (exact-integer?) procedure? ((... exact-integer?)))
      (arithmetic-shift (exact-integer?) procedure? (exact-integer? exact-integer?))

      ))

  ;; The list and vector procedures of Racket only (racket/base , racket/list , racket/vector) whose result has a type
  ;; given by the type of the elements of a container argument : the same form and rules as container-procedures-types
  ;; of scheme-types.rkt (R7RS and SRFI 1) , read by the type annotation (annot+.rkt , higher-order-type).
  (define racket-container-procedures-types
    '(;; an element
      (argmin           element 1 list)   ; (argmin proc lst)
      (argmax           element 1 list)
      (vector-argmin    element 1 vector) ; (vector-argmin proc vec)
      (vector-argmax    element 1 vector)
      ;; a container of the same elements
      (rest             same 0 list)
      (sort             same 0 list)      ; (sort lst less-than?)
      (remove-duplicates same 0 list)
      (shuffle          same 0 list)
      (remq             same 1 list)      ; (remq v lst)
      (remv             same 1 list)
      (vector-take      same 0 vector)
      (vector-drop      same 0 vector)
      (vector-sort      same 0 vector)    ; (vector-sort vec less-than?)
      (vector-filter    same 1 vector)    ; (vector-filter pred vec)
      ;; a container of the values of a procedure on the indexes
      (build-list       build 1 list)     ; (build-list n proc)
      (build-vector     build 1 vector)
      ;; an accumulation
      (foldl            fold 1 list)      ; (foldl f init l ...) , f called (f e ... acc)
      (foldr            fold 1 list)))

  

(export
	racket-types
	racket-container-procedures-types)
 ;; module racket-types
