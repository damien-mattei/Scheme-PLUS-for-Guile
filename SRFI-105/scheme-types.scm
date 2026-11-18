;; Translated from Scheme+ for Racket (scheme-types.rkt) for Guile.

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

;; Type table for the R7RS-small standard library (scheme base), the
;; (scheme inexact) library (acos asin atan cos exp finite? infinite? log
;; nan? sin sqrt tan), plus the core derived-expression/syntax keywords
;; that (scheme base) exports.
;; Source: "Revised(7) Report on the Algorithmic Language Scheme" (R7RS),
;; https://standards.scheme.org/official/r7rs.pdf , section 4 (Expressions,
;; derived syntax) and section 6 (Standard procedures).
;;
;; Each element of the list has the form:
;;
;;   for a procedure: (name (return-type-predicates ...) procedure? (parameter-type-predicates ...))
;;   for a macro:      (name (return-type-predicates ...) macro?     (parameter-type-predicates ...))
;;
;; Types are given as the Scheme predicate that recognizes them (e.g. a
;; vector argument is denoted by the symbol 'vector?), exactly as such a
;; predicate would be named and called in Scheme (e.g. (vector? x)).
;;
;; A few pseudo-predicates are used that are not part of R7RS itself, needed
;; because R7RS has no standard names for these notions:
;;
;;   any?     - matches any value/expression (no constraint on the type)
;;   void?    - the (unspecified) result of a side-effecting procedure
;;   syntax?  - stands for an unevaluated syntactic sub-form (an identifier,
;;              expression, binding clause, or body) rather than a runtime
;;              value, since macros operate on syntax, not values
;;   exact-nonnegative-integer? - an exact integer >= 0 (used for indices,
;;              lengths, and counts); a real, callable Racket predicate,
;;              but R7RS itself has no standard name for this notion
;;   (opt T)  - an optional parameter of type T
;;   (... T)  - zero or more additional parameters of type T
;;
;; This module has no dependencies; it is pure data.
;; The procedures of Racket that are not in R7RS (void , sqr , add1 , racket/flonum , racket/fixnum ...) are in
;; racket-types.rkt , with the same form.

;; AI generated (Anthropic Sonnet 5)

;; note: little part of the definitions in this file should indeed used
;; but as it is done automatically i can not figure out exactly how much
(define-module (SRFI-105 scheme-types)
  #:use-module (Scheme+ racket-compat))

  

  (define scheme-types

    '(

      ;; ---------------------------------------------------------------
      ;; 6.1  Equivalence predicates
      ;; ---------------------------------------------------------------

      (eqv?   (boolean?) procedure? (any? any?))
      (eq?    (boolean?) procedure? (any? any?))
      (equal? (boolean?) procedure? (any? any?))

      ;; ---------------------------------------------------------------
      ;; 6.2  Numbers
      ;; ---------------------------------------------------------------

      (number?                  (boolean?) procedure? (any?))
      (complex?                 (boolean?) procedure? (any?))
      (real?                    (boolean?) procedure? (any?))
      (rational?                (boolean?) procedure? (any?))
      (integer?                 (boolean?) procedure? (any?))
      (exact?                   (boolean?) procedure? (number?))
      (inexact?                 (boolean?) procedure? (number?))
      (exact-integer?           (boolean?) procedure? (any?))
      (zero?                    (boolean?) procedure? (number?))
      (positive?                (boolean?) procedure? (real?))
      (negative?                (boolean?) procedure? (real?))
      (odd?                     (boolean?) procedure? (integer?))
      (even?                    (boolean?) procedure? (integer?))
      (max                      (real?) procedure? (real? (... real?)))
      (min                      (real?) procedure? (real? (... real?)))
      ;; already in annot+.rkt
      ;; (+                        (number?) procedure? ((... number?)))
      ;; (*                        (number?) procedure? ((... number?)))
      ;; (-                        (number?) procedure? (number? (... number?)))
      ;; (/                        (number?) procedure? (number? (... number?)))
      (abs                      (real?) procedure? (real?))
      (floor/                   (integer? integer?) procedure? (integer? integer?))
      (floor-quotient           (integer?) procedure? (integer? integer?))
      (floor-remainder          (integer?) procedure? (integer? integer?))
      (truncate/                (integer? integer?) procedure? (integer? integer?))
      (truncate-quotient        (integer?) procedure? (integer? integer?))
      (truncate-remainder       (integer?) procedure? (integer? integer?))
      (quotient                 (integer?) procedure? (integer? integer?))
      (remainder                (integer?) procedure? (integer? integer?))
      (modulo                   (integer?) procedure? (integer? integer?))
      (gcd                      (integer?) procedure? ((... integer?)))
      (lcm                      (integer?) procedure? ((... integer?)))
      (numerator                (number?) procedure? (rational?))
      (denominator              (number?) procedure? (rational?))
      (floor                    (integer?) procedure? (real?))
      (ceiling                  (integer?) procedure? (real?))
      (truncate                 (integer?) procedure? (real?)) ; do not return exact-integer? !
      (round                    (integer?) procedure? (real?))
      (rationalize              (rational?) procedure? (real? real?))
      (exact                    (number?) procedure? (number?))
      (inexact                  (number?) procedure? (number?))
      (exact->inexact           (number?) procedure? (number?))
      (inexact->exact           (number?) procedure? (number?))
      (number->string           (string?) procedure? (number? (opt exact-integer?)))
      #;(string->number           (number?) procedure? (string? (opt exact-integer?)))
      (string->number           (any?) procedure? (string? (opt exact-integer?))) ; a number or #f ((string->number "a") is #f)
      (=                        (boolean?) procedure? (number? number? (... number?)))
      (<                        (boolean?) procedure? (real? real? (... real?)))
      (>                        (boolean?) procedure? (real? real? (... real?)))
      (<=                       (boolean?) procedure? (real? real? (... real?)))
      (>=                       (boolean?) procedure? (real? real? (... real?)))
      (square                   (number?) procedure? (number?))
      (exact-integer-sqrt       (exact-integer? exact-integer?) procedure? (exact-integer?))
      ; expt with integer also in operator in annot+ (commented in scheme-type)
      ; (expt                     (number?) procedure? (number? number?))

      ;; ---------------------------------------------------------------
      ;; 6.3  Booleans
      ;; ---------------------------------------------------------------

      (not      (boolean?) procedure? (any?))
      (boolean? (boolean?) procedure? (any?))
      (boolean=? (boolean?) procedure? (boolean? boolean? (... boolean?)))

      ;; ---------------------------------------------------------------
      ;; 6.4  Pairs and lists
      ;; ---------------------------------------------------------------

      (pair?     (boolean?) procedure? (any?))
      (cons      (pair?) procedure? (any? any?))
      (car       (any?) procedure? (pair?))
      (cdr       (any?) procedure? (pair?))
      (set-car!  (void?) procedure? (pair? any?))
      (set-cdr!  (void?) procedure? (pair? any?))
      (caar      (any?) procedure? (pair?))
      (cadr      (any?) procedure? (pair?))
      (cdar      (any?) procedure? (pair?))
      (cddr      (any?) procedure? (pair?))
      (null?     (boolean?) procedure? (any?))
      (list?     (boolean?) procedure? (any?))
      (make-list (list?) procedure? (exact-nonnegative-integer? (opt any?)))
      (list      (list?) procedure? ((... any?)))
      ;; DONE : changed in exact-integer?
      ;(length    (exact-integer?) procedure? (list?))
      #;(length    (fixnum?) procedure? (list?)) ; in Racket the length of a list in memory is a fixnum
      (length    (natural-fixnum?) procedure? (list?)) ; ℕ (natural-fixnum? of Scheme+/type) : an index from a length is natural
      (append    (list?) procedure? ((... list?)))
      (reverse   (list?) procedure? (list?))
      (list-tail (list?) procedure? (list? exact-nonnegative-integer?))
      (list-ref  (any?) procedure? (list? exact-nonnegative-integer?))
      (list-set! (void?) procedure? (list? exact-nonnegative-integer? any?))
      #;(memq      (list?) procedure? (any? list?))
      (memq      (any?) procedure? (any? list?)) ; a list or #f , any? : #f is not a list?
      #;(memv      (list?) procedure? (any? list?))
      (memv      (any?) procedure? (any? list?)) ; a list or #f , any? : #f is not a list?
      #;(member    (list?) procedure? (any? list? (opt procedure?)))
      (member    (any?) procedure? (any? list? (opt procedure?))) ; a list or #f , any? : #f is not a list?
      #;(assq      (pair?) procedure? (any? list?))
      (assq      (any?) procedure? (any? list?)) ; a pair or #f , any? : #f is not a pair?
      #;(assv      (pair?) procedure? (any? list?))
      (assv      (any?) procedure? (any? list?)) ; a pair or #f , any? : #f is not a pair?
      #;(assoc     (pair?) procedure? (any? list? (opt procedure?)))
      (assoc     (any?) procedure? (any? list? (opt procedure?))) ; a pair or #f , any? : #f is not a pair?
      (list-copy (list?) procedure? (list?))

      ;; ---------------------------------------------------------------
      ;; 6.5  Symbols
      ;; ---------------------------------------------------------------

      (symbol?        (boolean?) procedure? (any?))
      (symbol=?       (boolean?) procedure? (symbol? symbol? (... symbol?)))
      (symbol->string (string?) procedure? (symbol?))
      (string->symbol (symbol?) procedure? (string?))

      ;; ---------------------------------------------------------------
      ;; 6.6  Characters
      ;; ---------------------------------------------------------------

      (char?         (boolean?) procedure? (any?))
      (char=?        (boolean?) procedure? (char? char? (... char?)))
      (char<?        (boolean?) procedure? (char? char? (... char?)))
      (char>?        (boolean?) procedure? (char? char? (... char?)))
      (char<=?       (boolean?) procedure? (char? char? (... char?)))
      (char>=?       (boolean?) procedure? (char? char? (... char?)))
      ;(char->integer (exact-nonnegative-integer?) procedure? (char?))
      #;(char->integer (fixnum?) procedure? (char?)) ; at most #x10FFFF : a fixnum
      (char->integer (natural-fixnum?) procedure? (char?)) ; at most #x10FFFF : a fixnum ≥ 0 , ℕ
      (integer->char (char?) procedure? (exact-nonnegative-integer?))

      ;; ---------------------------------------------------------------
      ;; 6.7  Strings
      ;; ---------------------------------------------------------------

      (string?      (boolean?) procedure? (any?))
      (make-string  (string?) procedure? (exact-nonnegative-integer? (opt char?)))
      (string       (string?) procedure? ((... char?)))
      ;(string-length (exact-nonnegative-integer?) procedure? (string?))
      #;(string-length (fixnum?) procedure? (string?)) ; in Racket the length of a string in memory is a fixnum
      (string-length (natural-fixnum?) procedure? (string?)) ; ℕ (natural-fixnum? of Scheme+/type) : an index from a length is natural
      (string-ref   (char?) procedure? (string? exact-nonnegative-integer?))
      (string-set!  (void?) procedure? (string? exact-nonnegative-integer? char?))
      (string=?     (boolean?) procedure? (string? string? (... string?)))
      (string<?     (boolean?) procedure? (string? string? (... string?)))
      (string>?     (boolean?) procedure? (string? string? (... string?)))
      (string<=?    (boolean?) procedure? (string? string? (... string?)))
      (string>=?    (boolean?) procedure? (string? string? (... string?)))
      (substring    (string?) procedure? (string? exact-nonnegative-integer? exact-nonnegative-integer?))
      (string-append (string?) procedure? ((... string?)))
      (string->list (list?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (list->string (string?) procedure? (list?))
      (string-copy  (string?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string-copy! (void?) procedure? (string? exact-nonnegative-integer? string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string-fill! (void?) procedure? (string? char? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string-for-each (void?) procedure? (procedure? string? (... string?)))
      (string-map   (string?) procedure? (procedure? string? (... string?)))

      ;; ---------------------------------------------------------------
      ;; 6.8  Vectors
      ;; ---------------------------------------------------------------

      (vector?      (boolean?) procedure? (any?))
      (make-vector  (vector?) procedure? (exact-nonnegative-integer? (opt any?)))
      (vector       (vector?) procedure? ((... any?)))
      ;(vector-length (exact-integer?) procedure? (vector?)) ; modif D.MATTEI
      #;(vector-length (fixnum?) procedure? (vector?)) ; modif D.MATTEI
      (vector-length (natural-fixnum?) procedure? (vector?)) ; ℕ (natural-fixnum? of Scheme+/type) : an index from a length is natural
      (vector-ref   (any?) procedure? (vector? exact-nonnegative-integer?))
      (vector-set!  (void?) procedure? (vector? exact-nonnegative-integer? any?))
      (vector->list (list?) procedure? (vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (list->vector (vector?) procedure? (list?))
      (vector->string (string?) procedure? (vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string->vector (vector?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-copy  (vector?) procedure? (vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-copy! (void?) procedure? (vector? exact-nonnegative-integer? vector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-append (vector?) procedure? ((... vector?)))
      (vector-fill! (void?) procedure? (vector? any? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (vector-map   (vector?) procedure? (procedure? vector? (... vector?)))
      (vector-for-each (void?) procedure? (procedure? vector? (... vector?)))

      ;; ---------------------------------------------------------------
      ;; 6.9  Bytevectors
      ;; ---------------------------------------------------------------

      (bytevector?      (boolean?) procedure? (any?))
      (make-bytevector  (bytevector?) procedure? (exact-nonnegative-integer? (opt exact-integer?)))
      (bytevector       (bytevector?) procedure? ((... exact-integer?)))
      (bytevector-length (exact-nonnegative-integer?) procedure? (bytevector?))
      (bytevector-u8-ref (exact-integer?) procedure? (bytevector? exact-nonnegative-integer?))
      (bytevector-u8-set! (void?) procedure? (bytevector? exact-nonnegative-integer? exact-integer?))
      (bytevector-copy  (bytevector?) procedure? (bytevector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (bytevector-copy! (void?) procedure? (bytevector? exact-nonnegative-integer? bytevector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (bytevector-append (bytevector?) procedure? ((... bytevector?)))
      (utf8->string     (string?) procedure? (bytevector? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (string->utf8     (bytevector?) procedure? (string? (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))

      ;; ---------------------------------------------------------------
      ;; 6.10  Control features
      ;; ---------------------------------------------------------------

      (procedure?  (boolean?) procedure? (any?))
      (apply       (any?) procedure? (procedure? (... any?) list?))
      (map         (list?) procedure? (procedure? list? (... list?)))
      (for-each    (void?) procedure? (procedure? list? (... list?)))
      (call-with-current-continuation (any?) procedure? (procedure?))
      (call/cc     (any?) procedure? (procedure?))
      (values      (any?) procedure? ((... any?)))
      (call-with-values (any?) procedure? (procedure? procedure?))
      (dynamic-wind (any?) procedure? (procedure? procedure? procedure?))

      ;; ---------------------------------------------------------------
      ;; 6.11  Exceptions
      ;; ---------------------------------------------------------------

      (with-exception-handler (any?) procedure? (procedure? procedure?))
      (raise               (void?) procedure? (any?))
      (raise-continuable   (any?) procedure? (any?))
      (error               (void?) procedure? (string? (... any?)))
      (error-object?       (boolean?) procedure? (any?))
      (error-object-message (string?) procedure? (error-object?))
      (error-object-irritants (list?) procedure? (error-object?))
      (read-error?         (boolean?) procedure? (any?))
      (file-error?         (boolean?) procedure? (any?))

      ;; ---------------------------------------------------------------
      ;; 6.13  Ports
      ;; ---------------------------------------------------------------

      (port?              (boolean?) procedure? (any?))
      (input-port?        (boolean?) procedure? (any?))
      (output-port?       (boolean?) procedure? (any?))
      (textual-port?      (boolean?) procedure? (any?))
      (binary-port?       (boolean?) procedure? (any?))
      (input-port-open?   (boolean?) procedure? (input-port?))
      (output-port-open?  (boolean?) procedure? (output-port?))
      (current-input-port  (input-port?) procedure? ())
      (current-output-port (output-port?) procedure? ())
      (current-error-port  (output-port?) procedure? ())
      (close-port         (void?) procedure? (port?))
      (close-input-port   (void?) procedure? (input-port?))
      (close-output-port  (void?) procedure? (output-port?))
      (eof-object         (eof-object?) procedure? ())
      (eof-object?        (boolean?) procedure? (any?))
      (char-ready?        (boolean?) procedure? ((opt input-port?)))
      (read-char          (any?) procedure? ((opt input-port?)))
      (peek-char          (any?) procedure? ((opt input-port?)))
      (read-line          (any?) procedure? ((opt input-port?)))
      (read-string        (any?) procedure? (exact-nonnegative-integer? (opt input-port?)))
      (open-input-string   (input-port?) procedure? (string?))
      (open-output-string  (output-port?) procedure? ())
      (get-output-string   (string?) procedure? (output-port?))
      (write-char   (void?) procedure? (char? (opt output-port?)))
      (write-string (void?) procedure? (string? (opt output-port?) (opt exact-nonnegative-integer?) (opt exact-nonnegative-integer?)))
      (write        (void?) procedure? (any? (opt output-port?)))
      (write-shared (void?) procedure? (any? (opt output-port?)))
      (write-simple (void?) procedure? (any? (opt output-port?)))
      (display      (void?) procedure? (any? (opt output-port?)))
      (newline      (void?) procedure? ((opt output-port?)))
      (flush-output-port (void?) procedure? ((opt output-port?)))

      ;; ---------------------------------------------------------------
      ;; Promises
      ;; ---------------------------------------------------------------

      (make-promise (promise?) procedure? (any?))
      (promise?     (boolean?) procedure? (any?))
      (force        (any?) procedure? (promise?))

      ;; ---------------------------------------------------------------
      ;; Parameter objects
      ;; ---------------------------------------------------------------

      (make-parameter (procedure?) procedure? (any? (opt procedure?)))

      ;; ---------------------------------------------------------------
      ;; Features
      ;; ---------------------------------------------------------------

      (features (list?) procedure? ())

      ;; ---------------------------------------------------------------
      ;; (scheme inexact) library
      ;; Procedures typically only useful with inexact values; NOT part
      ;; of (scheme base) even though they are ordinary numeric ops.
      ;; ---------------------------------------------------------------

      ;; the return types are number? , neither real? nor flonum? : sqrt , log , asin , acos of a real can be a
      ;; complex ((sqrt -1.0) is +1.0i , (acos 2.0) is 0.0+1.31...i) ; sin , cos , tan , exp , atan are real only for
      ;; a real argument , and can be exact ((sin 0) is 0 , (exp 0) is 1) ; one signature can not say "the type of
      ;; the argument". The case of a flonum argument (flsin , flcos , fltan , flexp , flatan : flonum?) is in the
      ;; tables of annot+.rkt , not sqrt , log , asin , acos of a flonum (a flonum or a complex , known only at run
      ;; time). For the arithmetic number? gives orig-+ , orig-< ... as real? would.
      (acos     (number?) procedure? (number?))
      (asin     (number?) procedure? (number?))
      (atan     (number?) procedure? (number? (opt real?)))
      (cos      (number?) procedure? (number?))
      (exp      (number?) procedure? (number?)) ; not flonum? : (exp 0) is the exact 1 ; (exp x) of a flonum is typed by the tables of annot+.rkt (flexp)
      (finite?  (boolean?) procedure? (real?))
      (infinite? (boolean?) procedure? (real?))
      (log      (number?) procedure? (number? (opt number?)))
      (nan?     (boolean?) procedure? (real?))
      (sin      (number?) procedure? (number?))
      (sqrt     (number?) procedure? (number?))
      (tan      (number?) procedure? (number?))

      ;; =================================================================
      ;; Core syntax (macros / special forms) exported by (scheme base)
      ;; =================================================================

      (quote            (any?) macro? (syntax?))
      (lambda           (procedure?) macro? (syntax? syntax? (... syntax?)))
      (if               (any?) macro? (any? syntax? (opt syntax?)))
      (set!             (void?) macro? (symbol? any?))
      (define           (void?) macro? (syntax? (... syntax?)))
      (define-values    (void?) macro? (syntax? syntax?))
      (define-record-type (void?) macro? (syntax? syntax? syntax? (... syntax?)))
      (define-syntax    (void?) macro? (symbol? syntax?))
      (let-syntax       (any?) macro? (syntax? (... syntax?)))
      (letrec-syntax    (any?) macro? (syntax? (... syntax?)))
      (syntax-rules     (syntax?) macro? (syntax? (... syntax?)))
      (let              (any?) macro? (syntax? (... syntax?)))
      (let*             (any?) macro? (syntax? (... syntax?)))
      (letrec           (any?) macro? (syntax? (... syntax?)))
      (letrec*          (any?) macro? (syntax? (... syntax?)))
      (let-values       (any?) macro? (syntax? (... syntax?)))
      (let*-values      (any?) macro? (syntax? (... syntax?)))
      (begin            (any?) macro? ((... syntax?)))
      (do               (any?) macro? (syntax? syntax? (... syntax?)))
      (when             (any?) macro? (any? syntax? (... syntax?)))
      (unless           (any?) macro? (any? syntax? (... syntax?)))
      (cond             (any?) macro? ((... syntax?)))
      (case             (any?) macro? (any? (... syntax?)))
      (and              (any?) macro? ((... syntax?)))
      (or               (any?) macro? ((... syntax?)))
      (quasiquote       (any?) macro? (syntax?))
      (unquote          (any?) macro? (syntax?))
      (unquote-splicing (any?) macro? (syntax?))
      (delay            (promise?) macro? (syntax?))
      (delay-force      (promise?) macro? (syntax?))
      (parameterize     (any?) macro? (syntax? (... syntax?)))
      (guard            (any?) macro? (syntax? (... syntax?)))

      ;; =================================================================
      ;; Racket (not R7RS) : moved to racket-types.rkt (this table is R7RS only , shared by the ports to Guile and Kawa)
      ;; =================================================================

      #;(void             (void?) procedure? ((... any?)))

      ))

  ;; The list and vector procedures whose result has a type given by the type of the elements of a container argument
  ;; (a composite type (T) or #(T) , or the bracket notation {L[]}) , read by the type annotation (annot+.rkt ,
  ;; higher-order-type). Each element of the list has the form: (name rule position container)
  ;;   rule : element - the result is an element of the container : E
  ;;          same    - the result is a container of the same elements : (E) or #(E)
  ;;          build   - the result is a container of the values of the procedure at the position , called on an index ℕ
  ;;          fill    - the result is a container of the value at the position : (T) or #(T) , T type of the value
  ;;          fold    - (foldl f init l1 ... ln) : f is called on the elements and the accumulator (E1 ... En A) , A type
  ;;                    of init at the position ; the result is A when f returns A (the accumulator keeps its type)
  ;;          reduce  - (reduce f ridentity l) : f is called on (E E) , ridentity at the position ; the result is E when f
  ;;                    returns E and ridentity is of type E (it is the result for an empty list)
  ;;   position  : the position of the container argument (of the procedure for build , of the value for fill , of the
  ;;               initial value for fold and reduce , the procedure is the first argument and the lists follow) , all :
  ;;               every argument
  ;;   container : list or vector
  ;; Procedures of R7RS and SRFI 1 (portable : Guile and Kawa have SRFI 1) ; the procedures of Racket only (racket/base ,
;; racket/list , racket/vector : argmin , sort , build-list , foldl ...) are in racket-container-procedures-types of
;; racket-types.rkt , with the same form.
  ;; make-vector : with the annotation a vector keeps the type of its initial elements , the fill value must have the
  ;; type of the elements put after ((make-vector n 0.0) for a vector of floats) ; (make-vector n) : no element type.
  (define container-procedures-types
    '(;; an element
      (car              element 0 list)
      (first            element 0 list)   ; SRFI 1
      (second           element 0 list)   ; SRFI 1
      (third            element 0 list)   ; SRFI 1
      (fourth           element 0 list)   ; SRFI 1
      (fifth            element 0 list)   ; SRFI 1
      (cadr             element 0 list)
      (caddr            element 0 list)
      (last             element 0 list)   ; SRFI 1
      (list-ref         element 0 list)
      (vector-ref       element 0 vector)
      ;; a container of the same elements
      (cdr              same 0 list)
      (cddr             same 0 list)
      (reverse          same 0 list)
      (list-copy        same 0 list)
      (list-tail        same 0 list)
      (take             same 0 list)      ; SRFI 1
      (drop             same 0 list)      ; SRFI 1
      (take-right       same 0 list)      ; SRFI 1
      (drop-right       same 0 list)      ; SRFI 1
      (delete-duplicates same 0 list)     ; SRFI 1
      (filter           same 1 list)      ; SRFI 1 , (filter pred lst)
      (remove           same 1 list)      ; SRFI 1 , (remove pred lst) of SRFI 1 , (remove v lst) of Racket
      (delete           same 1 list)      ; SRFI 1 , (delete x lst)
      (append           same all list)
      (vector-copy      same 0 vector)
      (vector-append    same all vector)
      ;; a container of the fill value
      (make-list        fill 1 list)      ; (make-list n v)
      (make-vector      fill 1 vector)    ; (make-vector n v)
      ;; an accumulation
      (fold             fold 1 list)      ; SRFI 1 , (fold kons knil l ...) , kons called (kons e ... acc)
      (fold-right       fold 1 list)      ; SRFI 1
      (reduce           reduce 1 list)    ; SRFI 1 , (reduce f ridentity l)
      (reduce-right     reduce 1 list)))  ; SRFI 1

  

(export
	scheme-types
	container-procedures-types)
 ;; module scheme-types
