; Translated from sweet-expressions+.rkt of SRFI 110 for Racket for Guile : the checks of rackunit are written with
; SRFI 64 (tests/rackunit.scm) ; run them with tests/run-tests.sh (the files *-sweet+.scm are converted by
; sweet-infix2prefix4guile.scm)
;
; Scheme+ written with the sweet-expressions of SRFI 110 : the indentation gives the parenthesis
; (inside ( ) [ ] { } the indentation is not taken into account, as in SRFI 110).
; An empty line ends the expression. In Guile <- and := only assign : a variable is defined with <+.

define-module (tests sweet-expressions)

use-modules (Scheme+) (tests rackunit)

ANNOTATION-ON

define (square x)
  {x * x}

define (apply-op op a)
  op a

; a procedure defined above is known by the infix parser : (apply-op - 3) is a call , not (- apply-op 3)
{r1 <+ (apply-op - 3)}

check-equal? r1 -3

; the type annotation works in an indented body : {x + y} becomes an addition of flonums
define (fsum x y)
  type flonum? x y
  {x + y}

check-equal? fsum(1.5 2.5) 4.0

; neoteric expressions and postfix
check-equal? square(5) 25

{n <+ 5}

check-equal? {n !} 120

; let with the group marker \\ , and the sublist marker $
check-equal?
  let
    \\
      a 1
      b 2
    {a + b}
  3

define nine
  list $ square 3

check-equal? nine '(9)

; comments : #; followed by a space comments the line , #| |# is a block comment
#; display "this line is ignored"

check-equal? square(2) 4 #| a block comment at the end of a line |#

; a region for the symbolic computation : the quoted lists are infix expressions
CARE-OF-QUOTES-OFF

{formula <+ '(Bₖ · Bₖ₋₁ ⊕ Cₖ · (Bₖ ⊕ Bₖ₋₁))}

CARE-OF-QUOTES-ON

check-equal? formula '(⊕ (· Bₖ Bₖ₋₁) (· Cₖ (⊕ Bₖ Bₖ₋₁)))

; the strict SRFI 105 mode : {2 + 3 * 4} is ($nfx$ 2 + 3 * 4) , evaluated by the macro $nfx$ of Scheme+
BEGIN-STRICT-SRFI-105-REGION

check-equal? {2 + 3 * 4} 14

END-STRICT-SRFI-105-REGION

; the directives of SRFI 110 : #!curly-infix reads with the rules of SRFI 105 (two forms glued without a space are
; two forms) , #!sweet goes back to the sweet-expressions
; (a directive written just after a pragma of the strict mode : it was ignored before 2026-11-18)
#!curly-infix
(define glued 1)(define glued2 2)
(check-equal? {glued + glued2} 3)
#!sweet

check-equal? {glued * 10} 10
