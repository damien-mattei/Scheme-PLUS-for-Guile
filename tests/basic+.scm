;; Translated from Scheme+ for Racket (tests/run-tests.rkt) for Guile : the checks of rackunit are written with SRFI 64
;; (tests/rackunit.scm) ; run them with tests/run-tests.sh

;; The first tests of Scheme+ : SRFI 105 curly infix and the run time of Scheme+.

(define-module (tests basic))

(use-modules (Scheme+)
	     (tests rackunit))
;; Scheme+ is used above ; the tests run directly.
(displayln "Running tests converted by curly-infix2prefix4guile.scm with Scheme+ for Guile")

;; SRFI-105 curly-infix tests (these use the reader at load time)
(displayln "SRFI-105 curly-infix tests:")
(check-equal? {3 * 5 + 2} 17)
(check-equal? {3 + 5 * 2} 13)
(check-equal? {(3 + 1) * (2 * (+ 2 1) - (sin 0))} 24)
(displayln "  SRFI-105 tests passed")

;; Scheme+ runtime tests
(displayln "Scheme+ runtime tests:")
(define v (list->vector '(10 20 30)))
(check-equal? {v[1]} 20)

;; Additional examples from documentation (direct curly-infix forms)
;; 1) Bracket indexing on strings and negative indices
(check-equal? {"hello"[-1]} #\o)

;; 2) Slice notation on strings (substring) and vectors
;; Note: slices are Scheme tokens, not Python-style; include spaces around ':'
(check-equal? {"elephant"[2 : 5]} "eph")
(define vec (list->vector '(0 1 2 3 4 5)))
;; Use spaces around ':' for slice notation
(check-equal? {vec[1 : 4]} (list->vector '(1 2 3)))

;; 3) if is a Scheme special form (prefix); use parentheses. The condition
;; can be an infix expression between { } and Scheme+ provides `then`/`else`
;; keywords to make the form more readable.
(check-equal? (if {3 > 2} then 10 else 0) 10)

;; 4) Postfix factorial notation (!) - factorial of 5 is 120
(check-equal? {5 !} 120)

;; 5) Exponentiation ** and superscript examples
(check-equal? {2 ** 3} 8)

;; Additional simple examples pulled from the documentation:
;; a) Postfix summation: all numbers then + should return their sum
(check-equal? {1 2 3 4 +} 10)

;; b) Mix of postfix, infix and prefix inside { } -> {1 (4 5 *) 3 +} => 1 + (4*5) + 3 = 24
(check-equal? {1 (4 5 *) 3 +} 24)

;; c) Small recursive function using an infix condition in the if test
(define (fib n)
  (if {n < 2}
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(check-equal? (fib 6) 8)

;; d) String slicing with a step (every 2nd char)
(check-equal? {"abcdefghijkl"[ : : 2]} "acegik")

(displayln "  Scheme+ runtime tests passed")

(displayln "All done.")
