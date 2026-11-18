;; rackunit.scm

;; Guile Scheme version

;; The checks of rackunit (Racket) used by the tests of Scheme+ , written with SRFI 64 , so that the tests of Scheme+ for
;; Racket run in Guile with few changes : (check-equal? actual expected) is (test-equal expected actual).
;; A check that fails is reported by the runner of SRFI 64 (see run-tests.sh) .
;; It also gives the few forms of racket/base that the tests use : struct (a record , equal? compares the fields as for
;; a transparent structure) , displayln , vector-map (of R6RS : the same as the one of Racket) , foldl (fold of SRFI 1) ,
;; let-values (SRFI 11) , void and void? , sqr , build-vector , flonum? and fixnum? (R6RS).

;; Copyright 2026 Damien MATTEI

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


(define-module (tests rackunit)

  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-9)
  #:use-module ((rnrs base) #:select (vector-map vector-for-each))
  #:use-module ((srfi srfi-1) #:select ((fold . foldl)))
  #:use-module ((srfi srfi-11) #:select (let-values let*-values))
  #:use-module ((rnrs arithmetic flonums) #:select (flonum?))
  #:use-module ((rnrs arithmetic fixnums) #:select (fixnum?))

  #:export (check-equal? check-not-equal? check-eq? check-eqv? check-true check-false check-not-false check-pred
	    check-= check-exn check-not-exn
	    exn:fail? exn:fail:contract? exn:fail:syntax? exn:fail:contract:divide-by-zero?
	    struct displayln void void? sqr build-vector)

  #:re-export (test-begin test-end vector-map vector-for-each foldl let-values let*-values flonum? fixnum?))


(define-syntax check-equal?
  (syntax-rules ()
    ((_ actual expected) (test-equal expected actual))
    ((_ actual expected message) (test-equal message expected actual))))

(define-syntax check-not-equal?
  (syntax-rules ()
    ((_ actual expected . message) (test-assert (not (equal? actual expected))))))

(define-syntax check-eq?
  (syntax-rules ()
    ((_ actual expected . message) (test-eq expected actual))))

(define-syntax check-eqv?
  (syntax-rules ()
    ((_ actual expected . message) (test-eqv expected actual))))

;; rackunit : check-true passes for #t only , check-false for #f only
(define-syntax check-true
  (syntax-rules ()
    ((_ expr . message) (test-eq #t expr))))

(define-syntax check-false
  (syntax-rules ()
    ((_ expr . message) (test-eq #f expr))))

(define-syntax check-not-false
  (syntax-rules ()
    ((_ expr . message) (test-assert expr))))

(define-syntax check-pred
  (syntax-rules ()
    ((_ pred expr . message) (test-assert (pred expr)))))

(define-syntax check-=
  (syntax-rules ()
    ((_ actual expected epsilon . message) (test-approximate expected actual epsilon))))

;; (check-exn predicate thunk) : the thunk must raise an error ; in Guile the errors have no Racket types : any error
;; is accepted , whatever the predicate
(define-syntax check-exn
  (syntax-rules ()
    ((_ predicate thunk . message)
     (test-assert (catch #t (lambda () (thunk) #f) (lambda args #t))))))

(define-syntax check-not-exn
  (syntax-rules ()
    ((_ thunk . message)
     (test-assert (catch #t (lambda () (thunk) #t) (lambda args #f))))))

;; the types of the errors of Racket : any error in Guile
(define (exn:fail? e) #t)
(define (exn:fail:contract? e) #t)
(define (exn:fail:syntax? e) #t)
(define (exn:fail:contract:divide-by-zero? e) #t)
;; (struct name (field ...) option ...) of Racket : constructor name , predicate name? , accessors name-field
(define-syntax struct
  (lambda (stx)
    (syntax-case stx ()
      ((_ name (field ...) option ...)
       (let* ((n (symbol->string (syntax->datum #'name)))
	      (make-id (lambda (str) (datum->syntax #'name (string->symbol str)))))
	 (with-syntax ((type (make-id (string-append "<" n ">")))
		       (pred (make-id (string-append n "?")))
		       ((accessor ...) (map (lambda (f) (make-id (string-append n "-" (symbol->string (syntax->datum f)))))
					    #'(field ...))))
	   #'(define-record-type type (name field ...) pred (field accessor) ...)))))))

(define (displayln x) (display x) (newline))

(define (void . args) (if #f #f))
(define (void? x) (unspecified? x))
(define (sqr x) (* x x))
(define (build-vector n f)
  (let ((v (make-vector n)))
    (do ((i 0 (+ i 1))) ((= i n) v)
      (vector-set! v i (f i)))))
