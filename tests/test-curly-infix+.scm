;; Translated from Scheme+ for Racket (examples/test-curly-infix+.rkt) for Guile : the checks of rackunit are written with SRFI 64
;; (tests/rackunit.scm) ; run them with tests/run-tests.sh


;; Test suite for the extended SRFI 105 Curly Infix syntax of Scheme+
;;
;; It exercises mainly infix expressions (operator precedence and
;; associativity), infix expressions with inner round parentheses, mixed
;; prefix / infix expressions that are autodetected, prefix expressions
;; with an operator as argument, chains of in/equalities, and a few postfix
;; expressions.  Every example from the "Scheme+ Syntax and Conventions"
;; part of the manual that is a pure syntax example is reused here.
;;
;; Run it with:   racket examples/test-curly-infix+.rkt
;; A silent run (only the parser trace) means every check passed ; any
;; failure is printed by rackunit as "FAILURE ...".

(define-module (tests test-curly-infix))

  (use-modules
	(Scheme+))
  ANNOTATION-ON
  (use-modules
	(tests rackunit)) ; section 30
  
  ;; ================================================================
  ;; 1. Plain infix : operator precedence and associativity
  ;; ================================================================

  (check-equal? {2 + 3 * 4} 14)                 ; * before +
  (check-equal? {3 * 5 + 2} 17)
  (check-equal? {5 + 2} 7)
  (check-equal? {3 * 7 + 2 - 12 / 3 + -2} 17)   ; manual
  (check-equal? {10 - 2 - 3} 5)                 ; - is left associative
  (check-equal? {100 / 10 / 5} 2)              ; / is left associative
  (check-equal? {1 + 2 + 3 + 4 + 5} 15)         ; n-ary +
  (check-equal? {2 * 3 * 4} 24)                 ; n-ary *
  (check-equal? {-5 + 3} -2)                    ; unary minus
  (check-equal? {3 * -2} -6)

  ;; exponentiation ** and superscripts
  (check-equal? {2 ** 3} 8)
  (check-equal? {2 * 2 ** 3} 16)               ; ** binds tighter than *
  (check-equal? {3 ²} 9)
  (check-equal? {4 ² + 3 ²} 25)
  (check-equal? {3 · 5 + 2 ³} 23)              ; manual : · is * , ³ is cube
  (check-equal? {3 ² + 2 · 3 · 5 + 5 ²} 64)    ; manual
  (check-equal? {3 ² + 2 · 3 · 5 + 5 ²} (+ (expt 3 2) (* 2 3 5) (expt 5 2)))

  ;; exact rationals
  (check-equal? {1 / 2} 1/2)
  (check-equal? {1 / 2 + 1 / 4} 3/4)

  ;; modulo, bit shifts
  (check-equal? {17 % 5} 2)
  (check-equal? {1 << 5} 32)
  (check-equal? {40 >> 2} 10)

  ;; ================================================================
  ;; 2. Comparison and boolean operators in infix
  ;; ================================================================

  (check-true  {3 < 5})
  (check-true  {5 > 3})
  (check-true  {4 <= 4})
  (check-true  {4 >= 4})
  (check-true  {2 ≤ 9})
  (check-true  {5 ≥ 5})
  (check-true  {2 + 2 = 4})
  (check-true  {3 <> 4})
  (check-false {3 ≠ 3})

  (check-true  {{2 < 3} and {4 < 5}})
  (check-false {{2 < 3} and {5 < 4}})
  (check-true  (or {1 > 2} {2 > 1}))           ; infix args of a prefix or

  ;; ================================================================
  ;; 3. Infix expressions with inner round parentheses
  ;; ================================================================

  (check-equal? {3 * (2 + 4) - 1} 17)                     ; manual
  (check-equal? (let ((x {3 * (2 + 4) - 1})) x) 17)       ; manual : (define x {3 * (2 + 4) - 1})
  (check-equal? {(3 + 1) * (2 + 4)} 24)
  (check-equal? {((1 + 2) * 3) - (4 / (1 + 1))} 7)
  (check-equal? {(10 - 2) / (1 + 1) + (2 * (3 + 1))} 12)
  (check-equal? {2 ** (1 + 2)} 8)
  (check-equal? {(2 + 3) ² - (1 + 1) ³} 17)               ; 25 - 8

  ;; the two big ones from the manual (inner ( ) plus prefix calls)
  (check-equal? {3 * (- 9 4) + 2} 17)
  (check-equal? {(- 7 (3 * (+ 2 4) - 1)) + 3} -7)
  (check-= {(3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5)}
           (+ (* (+ 3 1) (- (* 2 (+ 2 1)) (sin 0.3))) (- (* 2 5) 5))
           1e-12)

  ;; ================================================================
  ;; 4. Mixed prefix / infix, autodetected
  ;; ================================================================

  (check-equal? {3 * (+ 2 4) - 1} 17)          ; manual : (+ 2 4) prefix inside infix
  (check-equal? (- (+ 3 3) {2 + 2}) 2)         ; manual : infix arg of a prefix -
  (check-equal? {(max 3 7) + (min 10 2)} 9)
  (check-equal? {(expt 2 3) + 1} 9)
  (check-equal? {(apply + (list 1 2 3 4)) * 2} 20)
  (check-equal? {(vector-ref (vector 10 20 30) 1) + 5} 25)
  (check-equal? {(- 10 4) * 2} 12)             ; prefix (- 10 4) as an operand of infix *
  (check-equal? {(gcd 12 18) + (lcm 3 4)} 18)  ; two prefix calls, infix +
  (check-equal? (+ {2 * 3} {4 * 5}) 26)        ; infix sub-expressions of a prefix +
  (check-equal? (list {1 + 1} {2 + 2} {3 + 3}) (list 2 4 6))
  (check-equal? {√ 16} 4)                      ; prefix-style call written in { }
  (check-equal? (√ {9 + 16}) 5)                ; infix arg of a prefix call

  ;; ================================================================
  ;; 5. Nested curly braces
  ;; ================================================================

  (check-equal? {{1 + 2} * {3 + 4}} 21)
  (check-equal? {{10 / 2} + {2 ** 3}} 13)
  (check-equal? {{{1 + 1} + {2 + 2}} * {3 + 4}} 42)

  ;; ================================================================
  ;; 6. Infix inside definitions, conditionals, recursion (manual)
  ;; ================================================================

  (define (fib n)                             ; manual
    (if {n < 2}
        n
        {(fib (n - 1)) + (fib (n - 2))}))
  (check-equal? (fib 7) 13)
  (check-equal? (fib 10) 55)

  (define (fact n)
    (if {n = 0}
        1
        {n * (fact {n - 1})}))
  (check-equal? (fact 5) 120)

  (define (norm x y) {x ² + y ²})             ; manual
  (check-equal? (norm 3 4) 25)

  (define (line-length x0 y0 x1 y1)           ; manual
    (√ {(x1 - x0) ² + (y1 - y0) ²}))
  (check-equal? (line-length 0 0 3 4) 5)

  (define (discriminant a b c) {b ² - 4 * a * c})
  (check-equal? (discriminant 1 -5 6) 1)
  (check-equal? (discriminant 2 4 2) 0)

  ;; ================================================================
  ;; 7. Curly infix inside macro definitions (manual)
  ;; ================================================================

  (define-syntax +=
    (syntax-rules ()
      ({var1 _ var2} {var1 := var1 + var2})))

  (define y 3)
  {y += 7}
  (check-equal? y 10)

  (define-syntax plus
    (syntax-rules ()
      ({var1 _ ...} {var1 + ...})))

  (check-equal? {2 plus 3} 5)
  (check-equal? {2 plus 3 plus 4 plus 5 plus 6} 20)

  ;; ================================================================
  ;; 8. Assignment operators on infix right-hand sides
  ;; ================================================================

  (define x 0)
  {x <- {3 * (2 + 4)}}
  (check-equal? x 18)
  {x := x + 2}
  (check-equal? x 20)
  {x <- (x - 5) / 3}
  (check-equal? x 5)

  ;; ================================================================
  ;; 9. Postfix expressions
  ;; ================================================================

  (check-equal? {1 2 3 4 +} 10)               ; manual : postfix n-ary +
  (check-equal? {5 !} 120)                     ; builtin postfix factorial
  (check-equal? {4 ²} 16)                      ; postfix superscript
  (check-equal? {2 ³} 8)
  (check-equal? {(2 + 3) !} 120)              ; postfix with an inner ( ) operand
  (check-equal? {(1 + 1) 3 4 +} 9)           ; postfix +, parenthesized operand
  (check-equal? { {2 + 3} ² } 25)             ; postfix superscript on a nested { }

  ;; ================================================================
  ;; 10. Plain prefix still works unchanged
  ;; ================================================================

  (check-equal? (+ 1 2 3 4) 10)
  (check-equal? (- 7 (* 3 (+ 2 4))) -11)
  (check-equal? (map (lambda (n) {n * n}) (list 1 2 3 4)) (list 1 4 9 16))

  ;; ================================================================
  ;; 11. Prefix expressions with an operator as argument, inside { }
  ;;     (not a canonical infix expression : they stay in prefix)
  ;; ================================================================

  (use-modules
	((srfi srfi-1) #:select (fold reduce)))
  (define L (list 1 2 3))
  (define (my-fold f init l) (foldl f init l)) ; a procedure unknown to the reader

  (check-equal? {(fold + 0 L)} 6)
  (check-equal? {(reduce + 0 L)} 6)
  (check-equal? {(vector-map + (vector 1 2 3) (vector 10 20 30))} (vector 11 22 33))
  (check-equal? {(my-fold + 0 L)} 6)
  (check-equal? {(fold + 0 L) * 2} 12)
  (check-equal? {(fold * 1 L) + (reduce max 0 L)} 9)
  {sum-L <+ (fold + 0 L)}
  (check-equal? sum-L 6)

  ;; ================================================================
  ;; 12. Chains of in/equalities (the examples of in-equalities.rkt)
  ;; ================================================================

  ;; the REPL examples at the end of in-equalities.rkt
  (check-false {0 ≤ 1 ≤ 2 and #f})
  (check-true  {#t and (#f or 1 < 2 <= 2)})
  (check-false {#t and (#f or 1 < 2 < 2)})
  (check-false {(or 1 < 2 < 2)})              ; the in/equalities get priority over or/and
  (check-true  {(or 1 < 2 <= 2)})
  (check-equal? {(or 1 < 2)} 1)               ; a single binary in/equality is not grouped : prefix or of 1 , < , 2
  (check-equal? {(or 1 > 2)} 1)
  (check-true  {0 ≤ 4 ≤ 4})
  (check-true  {1 < 2 < 3 <= 3})
  (check-false {3 > 2 > 2})

  ;; the in/equalities-state-1 examples of in-equalities.rkt, each one compared with its prefix form
  ;; (and binds tighter than or) for several values of the variables, giving #t and #f
  (define ineq-args '((0 1 1 5 3 0 #t)   ; x a b c d y f
                      (5 1 1 5 3 2 #t)
                      (0 5 1 5 3 1 #f)
                      (0 1 2 5 3 2 #f)
                      (0 1 1 5 1 0 #t)))

  (define (check-same-values infix-proc prefix-proc)
    (for-each (lambda (args)
                (check-equal? (apply infix-proc args) (apply prefix-proc args)))
              ineq-args))

  (define (ineq-1 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2})
  (define (ineq-1-prefix x a b c d y f)
    (and (< (+ x 2) 3)
         (< (+ a 1) (* 3 b))
         (<= (* 3 b) (- c 1))
         (< (- c 1) (+ d 2))))
  (check-same-values ineq-1 ineq-1-prefix)
  (check-true (ineq-1 0 1 1 5 3 0 #t))
  (check-false (ineq-1 0 1 2 5 3 2 #f))

  (define (ineq-2 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y})
  (define (ineq-2-prefix x a b c d y f) (or (ineq-1-prefix x a b c d y f) y))
  (check-same-values ineq-2 ineq-2-prefix)

  (define (ineq-3 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y * 3 > 4})
  (define (ineq-3-prefix x a b c d y f) (or (ineq-1-prefix x a b c d y f) (> (* y 3) 4)))
  (check-same-values ineq-3 ineq-3-prefix)

  (define (ineq-4 x a b c d y f) {5 < 6 < 7 or y > 4 or 1 < 2 < 3})
  (define (ineq-4-prefix x a b c d y f) (or (< 5 6 7) (> y 4) (< 1 2 3)))
  (check-same-values ineq-4 ineq-4-prefix)

  ;; an operand between two different comparisons is evaluated once , from left to right , and not after a false comparison
  (define chain-calls '())
  (define (chain-operand name value)
    (set! chain-calls (cons name chain-calls))
    value)
  (check-true {0 < chain-operand('f 2) <= 3})
  (check-equal? chain-calls '(f))
  (set! chain-calls '())
  (check-true {chain-operand('a 1) < chain-operand('b 2) <= chain-operand('c 3)})
  (check-equal? chain-calls '(c b a))
  (set! chain-calls '())
  (check-false {0 < chain-operand('b 5) <= 3 < chain-operand('c 4)})
  (check-equal? chain-calls '(b))

  (define (ineq-5 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y * 3 > 4 and f and 1 < 2 < 3})
  (define (ineq-5-prefix x a b c d y f)
    (or (ineq-1-prefix x a b c d y f)
        (and (> (* y 3) 4) f (< 1 2 3))))
  (check-same-values ineq-5 ineq-5-prefix)

  (define (seven) 7)
  (check-equal? {(abs -3)} 3)                  ; (f x) is not an in/equality
  (check-equal? {(seven)} 7)                   ; (f)

  ;; the examples of the comments inside in-equalities.rkt
  (define (ineq-6 x d xw y yw) {1 + d < x - xw < 7 and 0 < y - yw < 5})
  (define (ineq-6-prefix x d xw y yw) (and (< (+ 1 d) (- x xw) 7) (< 0 (- y yw) 5)))
  (for-each (lambda (args)
              (check-equal? (apply ineq-6 args) (apply ineq-6-prefix args)))
            '((5 1 2 4 1) (5 3 2 4 1) (9 1 1 4 1) (5 1 2 7 1)))
  (check-true (ineq-6 5 1 2 4 1))

  (define (ineq-7 x a b d xw y yw) {a + 1 < b * 3 and 1 + d < x - xw < 7 and 0 < y - yw < 5})
  (define (ineq-7-prefix x a b d xw y yw)
    (and (< (+ a 1) (* b 3)) (< (+ 1 d) (- x xw) 7) (< 0 (- y yw) 5)))
  (for-each (lambda (args)
              (check-equal? (apply ineq-7 args) (apply ineq-7-prefix args)))
            '((5 1 2 1 2 4 1) (5 6 2 1 2 4 1) (5 1 2 3 2 4 1) (5 1 2 1 2 9 1)))
  (check-true (ineq-7 5 1 2 1 2 4 1))

  (define (ineq-8 a b c d) {a < b and c < d})
  (check-true (ineq-8 1 2 3 4))
  (check-false (ineq-8 1 2 4 3))

  (define (ineq-9 x y z t) {x < y < z = t})
  (check-true (ineq-9 1 2 3 3))
  (check-false (ineq-9 1 2 3 4))
  (check-false (ineq-9 2 1 3 3))

  ;; ================================================================
  ;; 13. Chains of in/equalities in an assignment or a definition
  ;;     (the operators with a lower precedence than the in/equalities
  ;;     end the chain : and , or , ~> , assignment , definition)
  ;; ================================================================

  (define p 5)
  {b1 <+ 1 < 2 < 3}                            ; (:= b1 (< 1 2 3))
  (check-true b1)
  {a2 <+ b2 <+ 3 < 2 < 1}                      ; (<+ a2 b2 (< 3 2 1)) ; Guile : := does not define b2 , <+ defines it
  (check-false a2)
  (check-false b2)
  {b3 <+ p + 1 < 7 < 8}                        ; (<+ b3 (< (+ p 1) 7 8))
  (check-true b3)
  {b4 <+ 0 < p < 10 and p > 3}                 ; (:= b4 (and (< 0 p 10) (> p 3)))
  (check-true b4)
  (define b5 #f)
  {1 < 2 < 3 =: b5}                            ; (=: (< 1 2 3) b5)
  (check-true b5)
  {b6 <+ 1 < 2 < 3 or #f}                      ; (<- b6 (or (< 1 2 3) #f))
  (check-true b6)

  ;; ================================================================
  ;; 14. Regions of the pragmas ANNOTATION-ON / ANNOTATION-OFF
  ;;     (a pragma acts on the elements that follow it in the same list,
  ;;     until the next pragma or the end of the list)
  ;; ================================================================

  (define-syntax-rule (quote-it e) 'e)         ; a macro that uses its argument as data

  (define (quote-off)
    ANNOTATION-OFF                             ; OFF only in the body of quote-off
    (quote-it (+ 1 2)))
  (check-equal? (quote-off) '(+ 1 2))          ; not annotated : (+ 1 2) is not folded

  (define (twice-off a)
    ANNOTATION-OFF
    (type flonum? a)                           ; removed from the code , not used
    (* a 2.0))                                 ; stays * : accepts an exact integer
  (check-equal? (twice-off 2) 4.0)

  (define (twice-on a)                         ; ON in the body of twice-on : annotated whatever the state of the module
    ANNOTATION-ON
    (type flonum? a)
    (* a 2.0))                                 ; annotated : fl* , only for flonums
  (check-equal? (twice-on 2.5) 5.0)
  ;; Guile : the annotation writes the generic * (fl* of R6RS is slower in Guile) , an exact integer is accepted
  (check-equal? (twice-on 2) 4.0)

  ANNOTATION-OFF                               ; OFF at the level of the module
  (check-equal? (quote-it (* 2 3)) '(* 2 3))
  ANNOTATION-ON                                ; ON again for the rest of the module

  ;; ================================================================
  ;; 15. Syntax quotes are data, like quoted expressions :
  ;;     #'(...) , #`(...) and (quote-syntax ...) are not parsed
  ;;     as infix or postfix , even inside { } , and not annotated
  ;; ================================================================

  (check-equal? {(syntax->datum #'(a + b))} '(a + b))
  (check-equal? {(syntax->datum #`(a + b))} '(a + b))
  (check-equal? {(syntax->datum (quote-syntax (a + b)))} '(a + b))
  (check-equal? {(syntax->datum #'(1 2 3 +))} '(1 2 3 +))
  (check-equal? {(syntax->datum #'(+ - * /))} '(+ - * /))
  (check-equal? (syntax->datum #'{a + b}) '(+ a b))                ; a syntax quoted { } is still infix
  (check-equal? {(syntax->datum #`(a + #,{1 + 2}))} '(a + 3))       ; the unsyntax part is evaluated
  (check-equal? (syntax->datum #'(+ 1 2)) '(+ 1 2))                 ; not folded to 3 by the annotation pass
  (check-equal? {(syntax->datum #'(+ 1 2))} '(+ 1 2))

  ;; ================================================================
  ;; 16. The macro definitions are not annotated : their patterns and
  ;;     templates are not code of the current scope
  ;; ================================================================

  {p8-a <+ 1}                                  ; fixnums , outside the macros
  {p8-b <+ 2}
  (define-syntax-rule (add-p8 p8-a p8-b) (+ p8-a p8-b)) ; the pattern variables are not the fixnums above
  (check-equal? (add-p8 1.5 2.5) 4.0)          ; was : fx+: contract violation

  (define-syntax count-plus (syntax-rules (+) ((_ (+ e ...)) 'plus) ((_ e) 'other))) ; was : (syntax-rules 0 ...)
  (check-equal? (count-plus (+ q r)) 'plus)
  (check-equal? (count-plus q) 'other)

  (define-syntax add2-p8 (lambda (stx) (syntax-case stx () ((_ p8-a p8-b) #'(+ p8-a p8-b)))))
  (check-equal? (add2-p8 1.5 2.5) 4.0)

  (define-syntax-rule (twice-p8 x) (let () (type flonum? x) (* x 2.0))) ; the type annotation is removed from the template
  (check-equal? (twice-p8 1.5) 3.0)

  {p8-w <+ 2.5}                                ; a flonum
  (check-equal? (let-syntax ((m2 (syntax-rules (*) ((_ (* u v)) 'product) ((_ e) 'other))))
                  (list (m2 (* q r)) (+ p8-w 1.0))) ; the local macro is not annotated , the body is (fl+)
                '(product 3.5))

  ;; ================================================================
  ;; 17. The lists of datums of case are data : they are not parsed
  ;;     inside { } and not annotated , the key and the expressions are
  ;; ================================================================

  (define p10-op '*)
  (check-equal? {(case p10-op ((+ -) 'add) ((* /) 'mul) (else 'other))} 'mul) ; was : car: contract violation
  (check-equal? {(case {1 + 1} ((2) {10 * 2}) (else 0))} 20)                   ; the key and the expressions are parsed
  (check-equal? {(case 3 ((1 2) 'low) (else (3 * 4)))} 12)

  (define (kind-p10 op) (case op ((+) 'plus) ((*) 'times) (else 'other)))      ; was : (case op (0 'plus) (1 'times) ...)
  (check-equal? (kind-p10 '+) 'plus)
  (check-equal? (kind-p10 '*) 'times)
  (check-equal? (kind-p10 '-) 'other)

  {p10-x <+ 2.5}                                                               ; a flonum
  (define (f-p10 op) (case op ((+) (+ p10-x 1.0)) (else (* p10-x 2.0))))       ; the expressions are annotated : fl+ , fl*
  (check-equal? (f-p10 '+) 3.5)
  (check-equal? (f-p10 '-) 5.0)

  (def+ (case-def+ n)                                                          ; def+ converts its body at the expansion :
    (case n ((1) (10 - n)) (else (n * 2))))                                    ; the clauses are syntax objects there
  (check-equal? (case-def+ 1) 9)
  (check-equal? (case-def+ 3) 6)

  ;; ================================================================
  ;; 18. A parameter or a local variable hides the type of the outer
  ;;     variable of the same name (p12-x , p12-y : fixnums outside)
  ;; ================================================================

  (use-modules
	((srfi srfi-8) #:select (receive)))
  {p12-x <+ 1}
  {p12-y <+ 2}
  {p12-v <+ (vector 1 2 3)}                                                    ; a vector of fixnums
  (define (p12-half n) (/ n 2.0))                                             ; its return type is unknown

  (define (p12-g p12-x p12-y) (+ p12-x p12-y))                                ; was : (fx+ p12-x p12-y)
  (check-equal? (p12-g 1.5 2.5) 4.0)
  (def (p12-g2 p12-x p12-y) (* p12-x p12-y))                                  ; def of Scheme+
  (check-equal? (p12-g2 1.5 2.0) 3.0)
  (define p12-h (lambda (p12-x p12-y) (- p12-x p12-y)))
  (check-equal? (p12-h 2.5 1.0) 1.5)
  (define p12-k (case-lambda ((p12-x) (* p12-x 2)) ((p12-x p12-y) (+ p12-x p12-y))))
  (check-equal? (p12-k 1.5) 3.0)
  (check-equal? (p12-k 1.5 1.0) 2.5)
  (check-equal? (do ((p12-x 0.5 (* p12-x 2.0)) (p12-y 0.25 (+ p12-y p12-y)))   ; do of R7RS
                    ((> p12-x 3.0) (+ p12-x p12-y)))
                6.0)
  (check-equal? (receive (p12-x p12-y) (values 1.5 2.0) (+ p12-x p12-y)) 3.5)  ; receive of SRFI 8
  (check-equal? (let ((p12-x 1.5) (p12-y 2.5)) (+ p12-x p12-y)) 4.0)           ; let , was (fx+ p12-x p12-y)
  (check-equal? (let* ((p12-x 1.5) (p12-y p12-x)) (+ p12-x p12-y)) 3.0)         ; let*
  (check-equal? (let loop ((p12-x 0.5) (i 0))                                  ; named let
                  (if (= i 2) p12-x (loop (+ p12-x p12-x) (+ i 1))))
                2.0)
  (check-equal? (letrec ((p12-x 1.5)) (+ p12-x p12-x)) 3.0)                     ; letrec
  (check-equal? (let-values (((p12-x p12-y) (values 1.5 2.5))) (+ p12-x p12-y)) 4.0) ; let-values
  (define (p12-d1) (declare p12-x) {p12-x := (p12-half 3)} (+ p12-x p12-x))    ; a declaration
  (check-equal? (p12-d1) 3.0)
  (define (p12-d2) (define p12-x (p12-half 3)) (+ p12-x p12-x))               ; a definition of unknown type
  (check-equal? (p12-d2) 3.0)
  (define (p12-vsum p12-v) {p12-v[0] + p12-v[1]})                             ; the elements of a vector parameter
  (check-equal? (p12-vsum (vector 1.5 2.5)) 4.0)
  (check-true (ineq-9 1.5 2.5 3.5 3.5))                                       ; section 12 : was (fx< x y)
  (check-true (ineq-1 0.5 1 1 5 3 0 #t))                                      ; section 12 : was (fx+ x 2)

  ;; still optimized : a type written in the body of the procedure
  (define (p12-f1 p12-x) ANNOTATION-ON (type flonum? p12-x) (* p12-x 2.0))    ; fl* , annotated whatever the state of the module
  (check-equal? (p12-f1 1.5) 3.0)
  (check-equal? (p12-f1 2) 4.0)                                               ; Guile : the generic * , not fl*

  ;; ================================================================
  ;; 19. The procedures and macros of the program are known by the parser :
  ;;     defined in the file, typed with a type of procedure, overloaded.
  ;;     An expression is postfix when its last element is a procedure and
  ;;     its first element is surely a data : a number, a string, a
  ;;     character, a boolean, a quoted data, a variable typed with a type
  ;;     which is not a procedure. The clauses of cond, the parameters of
  ;;     lambda, the variables of do are not parsed as postfix
  ;; ================================================================

  (define (p19-apply-op op a) (op a))
  (define (p19-list . args) args)
  (define (p19-call a f) (f a))
  (type procedure? vector-map)                                    ; vector-map is not in racket/base
  (type flonum? p19-z p19-w)
  {p19-z <+ 1.0}
  {p19-w <+ 2.0}
  {p19-v <+ (vector 1 2 3)}
  {p19-n <+ 5}

  ;; between { } : converted at the reading, the definition or the type must be before
  {p19-e1 <+ (p19-apply-op - 1)}                                  ; was (- p19-apply-op 1)
  (check-equal? p19-e1 -1)
  {p19-e2 <+ (vector-map - p19-v)}                                ; was (- vector-map p19-v)
  (check-equal? p19-e2 #(-1 -2 -3))
  {p19-e3 <+ (1 2 p19-list)}                                      ; postfix
  (check-equal? p19-e3 '(1 2))
  {p19-e4 <+ (p19-call 3 !)}                                      ; a call of p19-call , was (! p19-call 3)
  (check-equal? p19-e4 6)
  {p19-e5 <+ (3 p19-n +)}                                         ; postfix : 3 is a data
  (check-equal? p19-e5 8)
  {p19-e6 <+ (p19-z 3 +)}                                         ; postfix : p19-z is typed flonum? , was a call of p19-z
  (check-equal? p19-e6 4.0)
  {p19-e7 <+ (p19-z p19-w max)}
  (check-equal? p19-e7 2.0)
  {p19-e8 <+ ("a" "b" string-append)}
  (check-equal? p19-e8 "ab")
  {p19-e9 <+ (p19-z sqrt)}                                        ; postfix with 2 terms : p19-z is typed flonum?
  (check-equal? p19-e9 1.0)
  {p19-e10 <+ (#t not)}                                           ; postfix : a boolean is a data
  (check-equal? p19-e10 #f)
  {p19-e11 <+ (cond ((pair? p19-v) car) (#t cdr))}                ; (#t cdr) is a clause of cond , not a postfix
  (check-equal? p19-e11 cdr)
  {p19-e12 <+ (cond ((pair? 1) car) (else !))}                    ; (else !) is a clause of cond , was (! else)
  (check-equal? p19-e12 !)
  {p19-f <+ (lambda (p19-z sqrt) p19-z)}                          ; the parameters of lambda are not parsed , not (sqrt p19-z)
  (check-equal? (p19-f 5 6) 5)
  {p19-e13 <+ (do ((p19-w sqrt) (p19-i 0 (p19-i + 1)))             ; the variables of do are not parsed , init , step , test are
                  ((p19-i = 2) (p19-w 16)))}
  (check-equal? p19-e13 4)

  ;; ================================================================
  ;; 20. Quoted lists. CARE-OF-QUOTES-ON (the default) : the quotes are
  ;;     respected , a quoted list is data , in a quoted { } only the top
  ;;     level is infix. CARE-OF-QUOTES-OFF : a region for the symbolic
  ;;     computation , the quoted and quasiquoted lists are infix at all
  ;;     levels ; the parts ,... and ,@... are code. A pragma acts until
  ;;     the other pragma or the end of the list where it is written
  ;; ================================================================

  {p20-a <+ 7}
  (check-equal? '(x + y * z) (list 'x '+ 'y '* 'z))                       ; data
  (check-equal? '{x + y * z} (list '+ 'x (list '* 'y 'z)))                 ; a quoted { } is infix
  (check-equal? '{(x + y) * z} (list '* (list 'x '+ 'y) 'z))               ; but its ( ) are data
  (check-equal? '{(x + y) * z + 1} (list '+ (list '* (list 'x '+ 'y) 'z) 1)) ; whatever the number of operators
  (check-equal? '{f (x + y)} (list 'f (list 'x '+ 'y)))                    ; {a b} is (a b)
  (check-equal? `(,p20-a · x ⊕ y) (list 7 '· 'x '⊕ 'y))

  CARE-OF-QUOTES-OFF
  (check-equal? '(x + y * z) (list '+ 'x (list '* 'y 'z)))
  (check-equal? '{(x + y) * z} (list '* (list '+ 'x 'y) 'z))
  (check-equal? '{(x + y) * z + 1} (list '+ (list '* (list '+ 'x 'y) 'z) 1))
  (check-equal? '{f (x + y)} (list 'f (list '+ 'x 'y)))
  (check-equal? `(,p20-a · x ⊕ y) (list '⊕ (list '· 7 'x) 'y))              ; ,p20-a is code
  (check-equal? `(,@(list 1 2) + 1) (list '+ 1 2 1))
  (check-equal? '(Bₖ · Bₖ₋₁ ⊕ Cₖ · (Bₖ ⊕ Bₖ₋₁))
                (list '⊕ (list '· 'Bₖ 'Bₖ₋₁) (list '· 'Cₖ (list '⊕ 'Bₖ 'Bₖ₋₁))))
  (check-equal? '(a b c) (list 'a 'b 'c))                                  ; a list of data in prefix stays as it is
  (define (p20-f) CARE-OF-QUOTES-ON '(u + v))                              ; the region ON ends with the body
  (check-equal? (p20-f) (list 'u '+ 'v))
  (check-equal? '(u + v) (list '+ 'u 'v))                                  ; still in the region OFF
  CARE-OF-QUOTES-ON
  (check-equal? '(x + y) (list 'x '+ 'y))

  ;; ================================================================
  ;; 21. Superscripts (exponents) : the examples of the comments of
  ;;     superscript-parser.rkt , the cases of the simple infix shortcut
  ;;     of the readers ({2 ⁴ ²} , {- 2 ³}) and of the parenthesis
  ;;     (⁽¹⁺¹⁾·⁽¹⁺²⁾) , and other cases
  ;; ================================================================

  (use-modules
	(Scheme+ superscript-parser))
  (define (p21-d x) (syntax->datum (datum->syntax #f x))) ; the results contain syntax objects

  ;; the expressions of the comments of superscript-parser.rkt
  (check-equal? {3 · 5 + 2} 17)
  (check-equal? {3 · 5 + 2 ³} 23)
  (check-equal? {3 ² + 2 · 3 · 5 + 5 ²} 64)
  (check-equal? {3 ⁻²} 1/9)                    ; was "-: identifier's binding is ambiguous" in a module (the #'- of superscript-parser.rkt)
  (check-equal? {2 ¹⁰} 1024)
  (check-equal? {2 ⁴} 16)
  (check-equal? {2 ⁴ ²} 65536)                 ; 2^(4^2) , was read (⁴ 2 ²) by the simple infix shortcut
  (check-equal? {- 2 ³} -8)                    ; -(2^3) , was read (2 - ³) by SRFI-105
  (check-equal? {3 * - 2 ⁴ ² + 1} -196607)
  (define (p21-nn) (define nn 3) {3 ²·⁽ⁿⁿ⁻⁴⁾})
  (check-equal? (p21-nn) 1/9)
  (define (p21-foo) (define n 3) {3 ⁻²·⁽ⁿ⁻⁴⁾}) ; the procedure foo of the comments
  (check-equal? (p21-foo) 9)

  ;; the calls of the comments of superscript-parser.rkt
  ;; note : the multiplication in a superscript is · (U+00B7 , middle dot) of full-superscript-string ,
  ;; not ⋅ (U+22C5 , dot operator) which is not a superscript character
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²))) '(3 ** 2))
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²·³⁻⁴))) '(3 ** (2 · 3 - 4)))
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²·⁽³⁻⁴⁾))) '(3 ** (2 · (3 - 4))))
  (check-equal? (p21-d (superscript-operator-loop (list 3 'ⁿ⁻⁴))) '(3 ** (n - 4)))
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²·⁽ⁿ⁻⁴⁾))) '(3 ** (2 · (n - 4))))
  (check-equal? (p21-d (parse-superscript-token "17")) 17)
  (check-equal? (p21-d (parse-superscript-token "¹⁷")) 17)
  (check-equal? (p21-d (parse-superscript-token "3+4")) '(3 + 4))
  (check-equal? (p21-d (parse-superscript-token "3+4-5")) '(3 + 4 - 5))
  (check-equal? (p21-d (parse-superscript-token "3*(4-5)")) '(3 * (4 - 5)))
  (check-equal? (p21-d (parse-superscript-token "3+2i")) '(3 + 0+2i))
  (check-equal? (p21-d (parse-superscript-token "3-2i")) '(3 - 0+2i))
  (check-equal? (p21-d (parse-superscript-token "3*(4-5)-2")) '(3 * (4 - 5) - 2))
  (check-equal? (p21-d (parse-superscript-token "-5")) '(- 5))       ; the sign stays an operator
  (check-equal? (p21-d (parse-superscript-token "alpha2")) 'alpha2)
  (check-equal? (p21-d (parse-superscript-token "alpha+4")) '(alpha + 4))
  (check-equal? (p21-d (parse-superscript-token "²·³⁻⁴")) '(2 · 3 - 4))
  (check-equal? (p21-d (parse-superscript-token "3*(4-5)-2+3i")) '(3 * (4 - 5) - 2 + 0+3i))
  (check-equal? (p21-d (parse-superscript-token "3*(4-(5-2))-2+3i")) '(3 * (4 - (5 - 2)) - 2 + 0+3i))
  (check-equal? (p21-d (parse-superscript-token "+-3*(4-(5-2))-2+3i")) '(+ - 3 * (4 - (5 - 2)) - 2 + 0+3i))
  (check-equal? (p21-d (parse-superscript-token "+-3*(4-(5-2))/-2")) '(+ - 3 * (4 - (5 - 2)) / - 2))

  ;; the cases of the simple infix shortcut of the readers : 3 elements with a superscript
  (define (p21-x-x x) {x ² ²})
  (check-equal? (p21-x-x 2) 16)                ; 2^(2^2) , was read (² x ²)
  (define (p21-minus-x x) {- x ²})
  (check-equal? (p21-minus-x 3) -9)            ; -(3^2) , was read (x - ²) by SRFI-105
  (check-equal? {+ 2 ³} 8)                     ; was read (2 + ³) by SRFI-105

  ;; * and · are both superscript and normal characters : alone they are operators , not exponents
  ;; (superscript-only? and not superscript? in the simple infix shortcut of the readers)
  {p21-op <+ *}                                ; gave "Error parsing +- : ... '(*)"
  (check-equal? (p21-op 2 3) 6)
  {p21-op· <+ ·}                               ; gave "Error parsing +- : ... '(·)"
  (check-equal? (p21-op· 2 3) 6)

  ;; several groups between parenthesis in an exponent : the matching closing parenthesis
  (check-equal? (p21-d (parse-superscript-token "⁽¹⁺¹⁾·⁽¹⁺²⁾")) '((1 + 1) · (1 + 2)))
  (check-equal? {2 ⁽¹⁺¹⁾·⁽¹⁺²⁾} 64)
  (check-equal? {2 ⁽¹⁺¹⁾} 4)
  (check-equal? {2 ⁽¹⁺¹⁾·³} 64)

  ;; other cases
  (define (p21-x2+1 x) {x ² + 1})
  (check-equal? (p21-x2+1 2) 5)
  (check-equal? {1 + 2 ³} 9)
  (check-equal? {(- 2) ³} -8)
  (check-equal? {- (2 ³)} -8)
  (check-equal? {2 ¹⸍²} (expt 2 1/2))          ; ⸍ is the division : square root of 2
  (check-equal? {3 ⁻²⁺³ᶦ} (expt 3 -2+3i))      ; complex exponent
  (define (p21-x-n+1 x) (define n 3) {x ⁿ⁺¹})
  (check-equal? (p21-x-n+1 2) 16)
  (define x² 7)                                ; without space , x² is a name
  (check-equal? {x² + 1} 8)
  (check-equal? (p21-d (parse-superscript-token "²⸱⁵")) 2.5)  ; ⸱ is the decimal separator
  (check-equal? (p21-d (parse-superscript-token "²ʲ")) 0+2i)  ; j , the electrical convention
  (check-equal? (p21-d (parse-superscript-token "ⁿ⸍²")) '(n / 2))
  (check-equal? (p21-d (parse-superscript-token "⁻²·⁽ⁿ⁻⁴⁾")) '(- 2 · (n - 4)))

  ;; ================================================================
  ;; 22. Chains of exponentiation : right associative , 2^(3^2) , as in
  ;;     mathematics. expt has only 2 arguments : a chain of expt is
  ;;     nested , {2 expt 3 expt 2} is (expt 2 (expt 3 2)) and no more
  ;;     (expt 2 3 2) (arity error) ; ** is nested the same way
  ;; ================================================================

  ;; expt in a chain (n-ary written in infix)
  (check-equal? {2 expt 3 expt 2} 512)
  (check-equal? {2 expt 3 expt 2 expt 1} 512)
  (check-equal? {2 expt 1 expt 3} 2)
  (check-equal? {3 expt 2 expt 2} 81)
  (check-equal? {(2 expt 3) expt 2} 64)
  (check-equal? {2 expt (3 expt 2)} 512)
  (check-equal? {2 expt 3 expt 2 + 1} 513)
  (check-equal? {1 + 2 expt 3 expt 2} 513)
  (check-equal? {2 · 2 expt 3 expt 2} 1024)
  (check-equal? {2 expt 3 ** 2} 512)           ; the two exponentiation operators mixed
  (check-equal? {2 ** 3 expt 2} 512)
  (check-equal? {2.0 expt 3 expt 2} 512.0)
  (define (p22-tower x) {x expt x expt x})
  (check-equal? (p22-tower 2) 16)

  ;; ** in a chain
  (check-equal? {2 ** 3 ** 2} 512)
  (check-equal? {(2 ** 3) ** 2} 64)
  (check-equal? {2 ** (3 ** 2)} 512)
  (check-equal? {2 ** 3 ** 2 + 1} 513)

  ;; operators that are variables or procedures : the precedence is decided at run time
  ;; (the tests planned for the Pratt parser)
  (define p22-mult *)
  (define p22-add +)
  (define (p22-mult-proc) *)
  (define (p22-cinque) 5)
  (check-equal? {3 p22-mult 5 p22-add 2} 17)
  (check-equal? {3 (p22-mult-proc) 5 p22-add 2} 17)
  (check-equal? {3 (p22-mult-proc) (p22-cinque) p22-add 2} 17)

  ;; ================================================================
  ;; 23. A flonum to an integer power : {x ²} is read (** x 2) with an
  ;;     exact exponent ; with (type flonum? x) its type is flonum? and
  ;;     the operation around it is specialized :
  ;;     {1.0 - x ²} --> (fl- 1.0 (orig-** x 2))
  ;;     (in typed code the exponent 0 is written 0.0 : (expt 2.0 0) is
  ;;     the exact 1)
  ;; ================================================================
  (define (p23-d x) (type flonum? x) {1.0 - x ²})
  (check-equal? (p23-d 2.0) -3.0)
  (define (p23-cube x) (type flonum? x) {1.0 + x ³})
  (check-equal? (p23-cube 2.0) 9.0)
  (define (p23-e x) (type flonum? x) {1.0 + (expt x 2)})
  (check-equal? (p23-e 3.0) 10.0)
  (define (p23-star x) (type flonum? x) {1.0 + x ** 2})
  (check-equal? (p23-star 3.0) 10.0)

  ;; ================================================================
  ;; 24. The types char? and void? : a literal character #\a is a
  ;;     char? , an element {s[i]} of a string s a char? , a slice
  ;;     {s[i : j]} a string? , (void) a void? ; the overloads are
  ;;     then resolved at read time :
  ;;     (p24-show c) --> (p24-char c) , (p24-show-void u) --> (p24-void u)
  ;; ================================================================
  (define (p24-char c) (char->integer c))
  (define (p24-string s) (string-length s))
  (overload-procedure p24-show p24-char (char?))
  (overload-procedure p24-show p24-string (string?))
  (define (p24-void u) 'void)
  (overload-procedure p24-show-void p24-void (void?))

  (define (p24-literal)
    {c <+ #\a}
    (p24-show c))                                ; (p24-char c)
  (check-equal? (p24-literal) 97)

  (define (p24-element s)
    (type string? s)
    {e <+ s[0]}
    (p24-show e))                                ; (p24-char e)
  (check-equal? (p24-element "hello") 104)

  (define (p24-slice s)
    (type string? s)
    {t <+ s[1 : 3]}
    (list t (p24-show t)))                       ; (p24-string t)
  (check-equal? (p24-slice "hello") '("el" 2))

  (define (p24-unit)
    {u <+ (void)}
    (p24-show-void u))                           ; (p24-void u)
  (check-equal? (p24-unit) 'void)

  ;; ================================================================
  ;; 25. not as a prefix unary operator in infix , with the precedence
  ;;     of Python : below the comparisons and the arithmetic , above
  ;;     and / or. A not in the position of an operand (at the
  ;;     beginning or after an operator) takes the terms until the
  ;;     next and , or , assignment , definition , if , else :
  ;;     {a and not b} --> (and a (not b)) , {not x < y} --> (not (< x y))
  ;;     A not in the position of a value is unchanged : {f := not}
  ;; ================================================================
  (define (p25-and-not a b) {a and not b})          ; (and a (not b))
  (check-equal? (p25-and-not #t #f) #t)
  (check-equal? (p25-and-not #t #t) #f)
  (define (p25-not-and a b) {not a and b})          ; (and (not a) b)
  (check-equal? (p25-not-and #f #t) #t)
  (check-equal? (p25-not-and #t #t) #f)
  (define (p25-not-less x y) {not x < y})           ; (not (< x y))
  (check-equal? (p25-not-less 1 2) #f)
  (check-equal? (p25-not-less 3 2) #t)
  (define (p25-or-not a b c) {a or not b and c})    ; (or a (and (not b) c))
  (check-equal? (p25-or-not #f #f #t) #t)
  (check-equal? (p25-or-not #f #t #t) #f)
  (define (p25-not-not a) {not not a})              ; (not (not a))
  (check-equal? (p25-not-not 5) #t)
  (define (p25-not a) {not a})                      ; (not a)
  (check-equal? (p25-not #f) #t)
  (define (p25-sum a b c) {not a + b < c})          ; (not (< (+ a b) c))
  (check-equal? (p25-sum 1 2 4) #f)
  (define (p25-assign a b)
    {x := not a and b}                              ; (:= x (and (not a) b))
    {y := (not a or b)}                             ; in a sub-expression : (:= y (or (not a) b))
    (list x y))
  (check-equal? (p25-assign #f #t) '(#t #t))
  (check-equal? (p25-assign #t #f) '(#f #f))
  (define (p25-if a b c) {a if not b else c})       ; (if (not b) a c)
  (check-equal? (p25-if 1 #f 2) 1)
  (check-equal? (p25-if 1 #t 2) 2)
  (define (p25-typed x y)
    (type flonum? x y)
    {not x < y})                                    ; (not (fl< x y))
  (check-equal? (p25-typed 1.0 2.0) #f)
  ;; not as a value : unchanged
  {p25-f <+ not}
  (check-equal? (p25-f #f) #t)
  (check-equal? {(map not '(#t #f))} '(#f #t))
  (check-equal? {(not #f) and #t} #t)
  ;; in a strict SRFI 105 region , {a and not b} is ($nfx$ a and not b) , parsed by the macro $nfx$
  BEGIN-STRICT-SRFI-105-REGION
  (define (p25-strict a b x y) (list {a and not b} {not x < y}))
  END-STRICT-SRFI-105-REGION
  (check-equal? (p25-strict #t #f 3 2) '(#t #t))

  ;; ================================================================
  ;; 26. ~ (bitwise not) as a prefix unary operator in infix , with the
  ;;     precedence of Python , as the unary minus : above * + & and the
  ;;     comparisons , below the exponentiation (~ takes the following
  ;;     term and its chain of exponentiation).
  ;;     {a & ~ b} --> (& a (~ b)) , {~ a ** 2} --> (~ (** a 2))
  ;;     A ~ in the position of a value is unchanged : {f := ~}
  ;; ================================================================
  (check-equal? (~ 5) -6)
  (define (p26-not a) {~ a})                        ; (~ a)
  (check-equal? (p26-not 5) -6)
  (define (p26-and-not a b) {a & ~ b})              ; (& a (~ b))
  (check-equal? (p26-and-not 7 2) 5)
  (define (p26-not-and a b) {~ a & b})              ; (& (~ a) b)
  (check-equal? (p26-not-and 2 7) 5)
  (define (p26-plus a) {~ a + 1})                   ; (+ (~ a) 1) : - a
  (check-equal? (p26-plus 5) -5)
  (define (p26-times a) {2 * ~ a})                  ; (* 2 (~ a))
  (check-equal? (p26-times 5) -12)
  (define (p26-not-not a) {~ ~ a})                  ; (~ (~ a))
  (check-equal? (p26-not-not 5) 5)
  (define (p26-power a) {~ a ** 2})                 ; (~ (** a 2))
  (check-equal? (p26-power 3) -10)
  (define (p26-superscript a) {~ a ²})              ; (~ (** a 2))
  (check-equal? (p26-superscript 3) -10)
  (define (p26-exponent a) {2 ** ~ a})              ; (** 2 (~ a))
  (check-equal? (p26-exponent -3) 4)
  (define (p26-chain a) {~ a ** 2 ** 3})            ; (~ (** a (** 2 3)))
  (check-equal? (p26-chain 2) -257)
  (define (p26-minus a) {- ~ a})                    ; (- (~ a))
  (check-equal? (p26-minus 5) 6)
  (define (p26-with-not a) {not ~ a = 0})           ; (not (= (~ a) 0))
  (check-equal? (p26-with-not -1) #f)
  (define (p26-assign a)
    {x := ~ a}                                      ; (:= x (~ a))
    x)
  (check-equal? (p26-assign 0) -1)
  ;; ~ as a value : unchanged
  {p26-f <+ ~}
  (check-equal? (p26-f 0) -1)
  (check-equal? {(map ~ '(0 1))} '(-1 -2))
  BEGIN-STRICT-SRFI-105-REGION
  (define (p26-strict a b) {a & ~ b})
  END-STRICT-SRFI-105-REGION
  (check-equal? (p26-strict 7 2) 5)

  ;; ================================================================
  ;; 27. The two precedence engines : the Pratt parser (by default) and
  ;;     the original engine of Scheme+ (use-pratt-parser? set to #f)
  ;;     must give the same result. The reader uses only the default
  ;;     engine : here the parser is called directly with each engine on
  ;;     the same terms (quoted : data , not converted by the reader).
  ;; ================================================================
  (use-modules
	((Scheme+ infix-with-precedence-to-prefix) #:select (!*prec-generic-infix-parser-prepare-runtime use-pratt-parser?)))

  (define (p27-parse terms pratt?)
    (parameterize ((use-pratt-parser? pratt?))
      (!*prec-generic-infix-parser-prepare-runtime terms (lambda (op a b) (list op a b)))))

  (define (p27-check terms expected) ; the same expected result with the two engines
    (check-equal? (p27-parse terms #t) expected)
    (check-equal? (p27-parse terms #f) expected))

  ;; arithmetic , comparisons , logic , assignment
  (p27-check '(3 * 4 + 2) '(+ (* 3 4) 2))
  (p27-check '(a < b <= c) '(and (< a b) (<= b c)))
  (p27-check '(a and b or c) '(or (and a b) c))
  (p27-check '(a and b and c or d) '(or (and a b c) d)) ; the chains of and / or are n-ary
  (p27-check '(a or b or c and d) '(or a b (and c d)))
  (p27-check '(x := y + 1) '(:= x (+ y 1)))
  ;; exponentiation , signs , superscripts
  (p27-check '(2 expt 3 expt 2) '(expt 2 (expt 3 2)))
  (p27-check '(- 2 ** 4) '(- (** 2 4)))
  (p27-check '(3 * - 2 ** 4 + 1) '(+ (* 3 (- (** 2 4))) 1))
  (p27-check '(a * - b) '(* a (- b)))
  (p27-check '(x ²·³⁻⁴) '(** x (- (· 2 3) 4)))
  ;; not (section 25)
  (p27-check '(a and not b) '(and a (not b)))
  (p27-check '(not x < y) '(not (< x y)))
  (p27-check '(a or not b and c) '(or a (and (not b) c)))
  (p27-check '(not a and not b and not c or c and not d) '(or (and (not a) (not b) (not c)) (and c (not d))))
  (p27-check '(not not a) '(not (not a)))
  (p27-check '(x := (not a and b)) '(:= x (and (not a) b)))
  (p27-check '(a if not b else c) '(if (not b) a c))
  ;; ~ (section 26)
  (p27-check '(a & ~ b) '(& a (~ b)))
  (p27-check '(~ a + 1) '(+ (~ a) 1))
  (p27-check '(~ a ** 2 ** 3) '(~ (** a (** 2 3))))
  (p27-check '(2 ** ~ a ** 3) '(** 2 (~ (** a 3))))
  (p27-check '(~ a ²) '(~ (** a 2)))
  (p27-check '(not ~ a = 0) '(not (= (~ a) 0)))

  ;; ================================================================
  ;; 28. The numeric procedures typed by the type of their arguments :
  ;;     with (type flonum? x) , (sin x) is (flsin x) of type flonum? ,
  ;;     so {(sin x) * 2.0} is (fl* (flsin x) 2.0) ; with fixnums
  ;;     (quotient k 2) is (fxquotient k 2) ... Without such an entry
  ;;     the call stays generic : (sin 0) is the exact 0 , (sqrt x) of a
  ;;     negative float is a complex number , (exp 0) is the exact 1.
  ;; ================================================================
  (define (p28-float x)
    (type flonum? x)
    (list {(sin x) * 2.0} {(cos x) + (tan x)} {(atan x) - 1.0} {(exp x) * x}
          {(abs x) + 1.0} {(min x 1.0) + (max x 1.0)}))
  (check-equal? (p28-float 0.5) (list (* (sin 0.5) 2.0) (+ (cos 0.5) (tan 0.5)) (- (atan 0.5) 1.0) (* (exp 0.5) 0.5)
                                      1.5 1.5))
  (check-equal? (p28-float -2.0) (list (* (sin -2.0) 2.0) (+ (cos -2.0) (tan -2.0)) (- (atan -2.0) 1.0) (* (exp -2.0) -2.0)
                                       3.0 -1.0))
  (define (p28-fixnum k n)
    (type fixnum? k n)
    (list {(quotient k 2) + 1} {(remainder k 2) * 3} {(modulo k 3) + 1} {(abs k) + (max k n) + (min k n)}
          {(exact->inexact k) * 2.0} {(exact->inexact n) / 2.0})) ; not inexact (R7RS) : unbound in Racket without annotation
  (check-equal? (p28-fixnum 7 -3) '(4 3 2 11 14.0 -1.5))
  (check-equal? (p28-fixnum -7 2) '(-2 -3 3 2 -14.0 1.0))
  (define (p28-generic x n)
    (type flonum? x)
    (type fixnum? n)
    (list (sin 0) (sqrt x) {(exp n) + 1})) ; no entry : the calls stay generic
  (check-equal? (p28-generic -4.0 0) (list 0 (sqrt -4.0) 2.0)) ; (sqrt -4.0) is a complex number , (exp 0) is 1.0 in Guile (the exact 1 in Racket)
  ;; the lengths and the codes of the characters are fixnums (scheme-types.rkt) : the operations around them are fx
  (define (p28-lengths l s v)
    (type list? l)
    (type string? s)
    (type vector? v)
    (list {(length l) * 2} {(string-length s) + 1} {(vector-length v) - 1} {(char->integer (string-ref s 0)) - 65}))
  (check-equal? (p28-lengths '(1 2 3) "ABC" #(1 2)) '(6 4 1 0))
  ;; rounding and square : of a float a float (flfloor ...) , of a fixnum a fixnum
  (define (p28-rounding x k)
    (type flonum? x)
    (type fixnum? k)
    (list {(floor x) + 1.0} {(ceiling x) * 2.0} {(round x) - 0.5} {(truncate x) / 2.0}
          {(floor k) + 1} {(round k) * 2} {(sqr x) + 1.0} {(sqr k) + 1}))
  (check-equal? (p28-rounding -2.5 7) '(-2.0 -4.0 -2.5 -1.0 8 14 7.25 50)) ; (round -2.5) is -2.0 : to even
  (check-equal? (p28-rounding 2.5 -3) '(3.0 6.0 1.5 1.0 -2 -6 7.25 10))

  ;; ================================================================
  ;; 29. not in the position of an operator : the negation of a
  ;;     comparison , as not in of Python , for any procedure or
  ;;     operator : {5 not member l} --> (not (member 5 l)) ; the
  ;;     operands go until and , or , an assignment ...
  ;; ================================================================
  (define p29-l '(1 2 3))
  (check-equal? {5 member p29-l} #f)                      ; (member 5 p29-l)
  (check-equal? {5 not member p29-l} #t)                  ; (not (member 5 p29-l))
  (check-equal? {2 not member p29-l} #f)
  (define (p29-and x) {x not member p29-l and x > 0})     ; (and (not (member x p29-l)) (> x 0))
  (check-equal? (p29-and 5) #t)
  (check-equal? (p29-and 2) #f)
  (define (p29-sum x) {x + 1 not member p29-l})           ; (not (member (+ x 1) p29-l))
  (check-equal? (p29-sum 4) #t)
  (check-equal? (p29-sum 1) #f)
  (define (p29-assign x)
    {y := x not member p29-l}                             ; (:= y (not (member x p29-l)))
    y)
  (check-equal? (p29-assign 7) #t)
  (check-equal? {3 not < 2} #t)                           ; (not (< 3 2))
  (define (in x l) (and (member x l) #t))                 ; an in of Python , defined by the user
  (check-equal? {5 not in p29-l} #t)                      ; (not (in 5 p29-l))
  ;; unchanged : a not that is a value , a call with not as argument
  {p29-f <+ not}
  (check-equal? (p29-f #f) #t)
  (check-equal? {(list 1 not 2 3)} (list 1 not 2 3))
  ;; the two precedence engines
  (p27-check '(5 not member l and x) '(and (not (member 5 l)) x))
  (p27-check '(x + 1 not member l) '(not (member (+ x 1) l)))
  (p27-check '(a and b not member l or c) '(or (and a (not (member b l))) c))

  ;; ================================================================
  ;; 30. The walrus operator :=> of Python (:= in Python) : assigns
  ;;     and returns the value , {y := {x :=> 5} + 1} ; the variable
  ;;     must exist before (a new name is refused at expansion time).
  ;; ================================================================
  (declare p30-x p30-y p30-n p30-a p30-b)
  {p30-y := {p30-x :=> 5} + 1}
  (check-equal? (list p30-x p30-y) '(5 6))
  (define (p30-length l)
    (if {(p30-n :=> (length l)) > 2} p30-n 0))
  (check-equal? (p30-length '(1 2 3 4)) 4)
  (check-equal? (p30-length '(1)) 0)
  (check-equal? {p30-a :=> p30-b :=> 3} 3)                 ; chained : (:=> p30-a p30-b 3)
  (check-equal? (list p30-a p30-b) '(3 3))
  (define p30-v (vector 0 0 0))
  (define p30-count 0)
  (define (p30-next) {p30-count := p30-count + 1} p30-count)
  (check-equal? {p30-v[(p30-next)] :=> 7} 7)              ; the index is computed once
  (check-equal? (list p30-v p30-count) '(#(0 7 0) 1))
  (define (p30-local x)
    {x :=> x * 2})                                          ; a parameter exists
  (check-equal? (p30-local 4) 8)
  ;; a new name is refused : it would be defined in the let of :=> and lost
  ;; Guile : no check at expansion time (:= only assigns) , the assignment of a name never defined fails at run time
  (check-exn exn:fail?
             (lambda () (:=> p30-new 1)))
  ;; annotated : (:=> x expr) has the type of expr
  (define (p30-float a)
    ANNOTATION-ON
    (type flonum? a)
    (declare x)
    {y := {x :=> a * 2.0} * 3.0}                            ; (:= y (fl* (:=> x (fl* a 2.0)) 3.0))
    (list x y))
  (check-equal? (p30-float 1.5) '(3.0 9.0))
  (define (p30-vector v)
    ANNOTATION-ON
    (type vector? v)
    (declare w)
    (eq? {w :=> v} v))                                       ; the value is returned , not a copy
  (check-true (p30-vector (vector 1 2)))
  ;; the two precedence engines
  (p27-check '(y := (x :=> 5) + 1) '(:= y (+ (:=> x 5) 1)))
  (p27-check '(n :=> a + b * c) '(:=> n (+ a (* b c))))
  (p27-check '(a :=> not b and c) '(:=> a (and (not b) c)))

  ;; ================================================================
  ;; 31. The access by one index written inline (faster) : a vector
  ;;     and an index (not a slice) , a negative index counts from
  ;;     the end as in Python ; the other cases (slices , strings ,
  ;;     a slice in a variable) go through the general procedures.
  ;; ================================================================
  (define p31-v (vector 10 20 30 40))
  (check-equal? {p31-v[0]} 10)
  (check-equal? {p31-v[-1]} 40)
  (define (p31-get v i) {v[i]})
  (check-equal? (p31-get p31-v 1) 20)
  (check-equal? (p31-get p31-v -2) 30)                    ; a negative index in a variable
  (check-equal? {p31-v[:]} #(10 20 30 40))                ; a copy
  (check-false (eq? {p31-v[:]} p31-v))
  (check-equal? {p31-v[1 :]} #(20 30 40))
  (define p31-colon :)
  (check-equal? (p31-get p31-v p31-colon) #(10 20 30 40)) ; a slice in a variable
  (check-equal? {"toto"[-1]} #\o)                         ; a string : the general procedure
  (define p31-M (vector (vector 1 2 3) (vector 4 5 6)))
  (check-equal? {p31-M[1][-1]} 6)
  (define p31-w (vector 1 2 3 4))
  {p31-w[-1] := 7}
  (define (p31-set! v i x) {v[i] := x})
  (p31-set! p31-w -2 5)
  (p31-set! p31-w 0 9)
  (check-equal? p31-w #(9 2 5 7))
  {p31-w[:] := #(0 0 0 0)}                                ; a slice : the general procedure
  (check-equal? p31-w #(0 0 0 0))
  ;; annotated : $bracket-apply-vector$ , a literal index >= 0 is a plain vector-ref
  (define (p31-typed v i)
    ANNOTATION-ON
    (type #(fixnum?) v)
    (type fixnum? i)
    {v[i] + v[-1] + v[0]})
  (check-equal? (p31-typed (vector 1 2 3 4) -2) 8)        ; 3 + 4 + 1
  (define (p31-typed-set! v i)
    ANNOTATION-ON
    (type #(fixnum?) v)
    (type fixnum? i)
    {v[i] := 99}
    {v[0] := v[-1]}
    v)
  (check-equal? (p31-typed-set! (vector 1 2 3) -2) #(3 99 3))

  ;; ================================================================
  ;; 32. Written inline too : the strings (one index) , the vectors of
  ;;     vectors with two indexes M[i j] , the arrays of SRFI 25 , and
  ;;     their assignments. A negative index counts from the end of
  ;;     the vector where it applies (a row of a rectangular matrix).
  ;; ================================================================
  (define p32-s (string-copy "hello"))
  (check-equal? {p32-s[-1]} #\o)
  (define (p32-char s i) {s[i]})
  (check-equal? (p32-char p32-s -2) #\l)
  (check-equal? {p32-s[1 :]} "ello")                       ; a slice : the general procedure
  {p32-s[-1] := #\O}
  (define (p32-set-char! s i c) {s[i] := c})
  (p32-set-char! p32-s 0 #\H)
  (check-equal? p32-s "HellO")
  ;; a rectangular matrix : 2 rows of 3 , and 3 rows of 2
  (define p32-M (vector (vector 1 2 3) (vector 4 5 6)))
  (define p32-N (vector (vector 1 2) (vector 3 4) (vector 5 6)))
  (check-equal? {p32-M[0 2]} 3)
  (check-equal? {p32-M[1 0]} 4)
  (check-equal? {p32-M[0 -1]} 3)                           ; was 2 : -1 counted from the end of the vector of rows
  (check-equal? {p32-M[-1 -1]} 6)
  (check-equal? {p32-N[0 -1]} 2)                           ; was an error : index 2 in a row of 2
  (define (p32-ref M i j) {M[i j]})
  (check-equal? (p32-ref p32-N -1 -2) 5)
  (check-equal? {p32-M[1 :]} (vector (vector 4 5 6)))       ; a slice of the vector of rows
  {p32-M[0 -1] := 30}
  (define (p32-set! M i j x) {M[i j] := x})
  (p32-set! p32-N 0 -1 20)
  (p32-set! p32-N -1 0 50)
  (check-equal? p32-M (vector (vector 1 2 30) (vector 4 5 6)))
  (check-equal? p32-N (vector (vector 1 20) (vector 3 4) (vector 50 6)))
  ;; three dimensions : the general procedures (function-array-n-dim-ref / -set! of array.rkt)
  (define p32-V (vector (vector (vector 1 2 3) (vector 4 5 6))))
  (check-equal? {p32-V[0 1 -1]} 6)
  {p32-V[0 -1 -1] := 70}
  (check-equal? p32-V (vector (vector (vector 1 2 3) (vector 4 5 70))))
  ;; annotated
  (define (p32-typed M i j)
    ANNOTATION-ON
    (type #(#(fixnum?)) M)
    (type fixnum? i j)
    {M[i j] := M[i j] + 100}
    {M[i j] + M[0 0]})
  (check-equal? (p32-typed (vector (vector 1 2 3) (vector 4 5 6)) 0 -1) 104)   ; (3 + 100) + 1
  (define (p32-typed-string s)
    ANNOTATION-ON
    (type string? s)
    {s[-1] := s[0]}
    s)
  (check-equal? (p32-typed-string (string-copy "abc")) "aba")
  ;; the arrays of SRFI 25 : array-ref and array-set! directly
  
  (define p32-A (make-array 0 2 3))                       ; Guile : an array of Guile (Racket : SRFI 25 , (make-array (shape 0 2 0 3) 0))
  {p32-A[1 2] := 7}
  (check-equal? {p32-A[1 2]} 7)
  (check-equal? (array-ref p32-A 1 2) 7)
  (define (p32-typed-array A)
    ANNOTATION-ON
    (type array? A)
    {A[0 1] := A[1 2] + 1}
    {A[0 1]})
  (check-equal? (p32-typed-array p32-A) 8)

  ;; ================================================================
  ;; 33. The element F[i j] of a flomat (Racket only) written inline ,
  ;;     in reading and in assignment , with and without annotation.
  ;;     Removed : flomat is no more in Scheme+ (sci requires libgfortran) ,
  ;;     a flomat uses overload-square-brackets in the program.
  ;; ================================================================
  #;(require (only-in flomat make-flomat))
  #;(define p33-F (make-flomat 2 3 0.0))
  #;{p33-F[1 2] := 7.0}
  #;(check-equal? {p33-F[1 2]} 7.0)
  #;(define (p33-set! F i j x) {F[i j] := x})
  #;(p33-set! p33-F 0 1 2.5)
  #;(define (p33-get F i j) {F[i j]})
  #;(check-equal? (p33-get p33-F 0 1) 2.5)
  #;(define (p33-typed F)
    ANNOTATION-ON
    (type flomat? F)
    {F[0 0] := F[1 2] + 1.0}
    {F[0 0]})
  #;(check-equal? (p33-typed p33-F) 8.0)

  ;; ================================================================
  ;; 34. The square : the exact integer exponent 2 is a multiplication
  ;;     (faster than expt , and more precise : the exact square
  ;;     rounded once) ; the other exponents and 2.0 keep expt (by
  ;;     multiplications x ³ would be wrong by one ulp for 26 % of the
  ;;     floats). With the annotation {x ²} of a float is (fl* x x).
  ;; ================================================================
  (check-equal? {1.1 ²} (* 1.1 1.1))
  (check-equal? {7 ²} 49)
  (check-equal? {1/3 ²} 1/9)
  (check-equal? {1.1 ** 2.0} (expt 1.1 2.0))               ; 2.0 : expt
  (check-equal? {1.1 ³} (expt 1.1 3))                      ; 3 : expt , not 1.1 * (1.1 * 1.1)
  (check-equal? {1.1 ⁴} (expt 1.1 4))
  (check-equal? {2 ** 3 ** 2} 512)                         ; the chains : unchanged
  (check-equal? (** 2 3 2 1) 512)
  (define (p34-square x) {x ²})
  (check-equal? (p34-square -3.331703861009889e-8) 1.1100250617468202e-15) ; the exact square (expt gives ...04e-15)
  (define (p34-typed x n)
    ANNOTATION-ON
    (type flonum? x)
    (type fixnum? n)
    (list {x ²} {x ** 2} {(x + 1.0) ²} {x ³} {x ** n} {x ² + 1.0}))
  (check-equal? (p34-typed 1.5 4) (list 2.25 2.25 6.25 (expt 1.5 3) (expt 1.5 4) 3.25))
  (check-equal? (car (p34-typed -3.331703861009889e-8 2)) 1.1100250617468202e-15)

  ;; ================================================================
  ;; 35. The reserved names of the types : ℕ and ℤ are fixnum? , ℝ is
  ;;     flonum? , in the forms type , the signatures , the composite
  ;;     types and the overloads (chosen at read time and at run time).
  ;; ================================================================
  (check-true (ℝ 2.0))
  (check-false (ℝ 2))
  #;(check-true (ℕ -3))
  (check-false (ℕ -3))                                     ; ℕ : a fixnum >= 0
  (check-true (ℕ 3))
  (check-true (ℤ -3))                                      ; ℤ : fixnum? , the negative ones too
  (check-true (ℤ 5))
  (check-true (ℚ 1/2))                                     ; ℚ : rational? , ℂ : complex? (only defined)
  (check-true (ℂ 1+2i))
  (define (p35-f x i k)
    ANNOTATION-ON
    (type ℝ x)
    (type ℕ i)
    (type ℤ k)
    (list {x * 2.0} {i + 1} {k - 1}))                       ; (fl* x 2.0) (fx+ i 1) (fx- k 1)
  (check-equal? (p35-f 1.5 3 -4) '(3.0 4 -5))
  (check-equal? (p35-f 1 3 -4) '(2.0 4 -5))                   ; Guile : the generic operations , not fl* fx+ fx-
  (define (p35-g x) {x * x + 1.0})
  (define (p35-h y)
    ANNOTATION-ON
    (type [(ℝ) procedure? (ℝ)] p35-g)
    {(p35-g y) * 3.0})                                      ; the signature : (fl* (p35-g y) 3.0)
  (check-equal? (p35-h 2.0) 15.0)
  (define (p35-v v i)
    ANNOTATION-ON
    (type #(ℝ) v)
    (type ℕ i)
    {v[i] * 2.0})                                           ; a vector of flonums : (fl* ($bracket-apply-vector$ v i) 2.0)
  (check-equal? (p35-v (vector 1.0 2.0) 1) 4.0)
  (struct p35-vec (x) #:transparent)
  (define (p35-add v r) (p35-vec (+ (p35-vec-x v) r)))
  (overload-procedure p35-plus (p35-vec?) p35-add (p35-vec? ℝ)) ; ℝ : a predicate at run time
  (check-equal? (p35-plus (p35-vec 1.0) 2.5) (p35-vec 3.5))
  (define (p35-use v r)
    ANNOTATION-ON
    (type p35-vec? v)
    (type ℝ r)
    (p35-plus v r))                                         ; chosen at read time : (p35-add v r)
  (check-equal? (p35-use (p35-vec 1.0) 2.5) (p35-vec 3.5))

  ;; ================================================================
  ;; 36. The indexes typed ℕ or ℤ : accesses and assignments
  ;;     specialized for the vectors and the strings , ℕ : vector-ref
  ;;     directly , ℤ : only the test of the sign (a negative index
  ;;     counts from the end) , no type : the general access.
  ;; ================================================================
  (define (p36-f v s M i k n)
    ANNOTATION-ON
    (type vector? v M)
    (type string? s)
    (type ℕ i)
    (type ℤ k)
    {v[i] := v[k] + 1}                                      ; ($bracket-apply-vector-natural$ v i) , (… -integer$ v k)
    {s[i] := s[k]}
    {M[i k] := M[i i]}                                      ; M[0][-1] := M[0][0]
    (list {v[i]} {v[k]} {v[i + 1]} {v[n]} {v[2]} {s[i]} {s[k]} {M[i k]} {M[i i]} {i + 1} {v[1 :]} {M[i][k]} v s M))
  (check-equal? (p36-f (vector 1 2 3) (string-copy "abc") (vector (vector 1 2) (vector 3 4)) 0 -1 1)
                (list 4 3 2 2 3 #\c #\c 1 1 1 #(2 3) 1 #(4 2 3) "cbc" #(#(1 1) #(3 4))))
  (define (p36-walrus v i)
    ANNOTATION-ON
    (type vector? v)
    (type ℕ i)
    (declare x)
    {x := {v[i] :=> 7} + 1}
    (list x v))
  (check-equal? (p36-walrus (vector 0 0) 1) (list 8 #(0 7)))
  (define (p36-bad v i)                                     ; ℕ is not checked : a negative index is an error of bounds
    ANNOTATION-ON
    (type vector? v)
    (type ℕ i)
    {v[i]})
  (check-exn exn:fail:contract? (lambda () (p36-bad (vector 1 2) -1)))
  ;; the sum and the product of naturals are naturals for the indexes : {v[2 * (i + 2) + 1]} is a vector-ref directly
  (define (p36-sum v M i j k)
    ANNOTATION-ON
    (type vector? v M)
    (type ℕ i j)
    (type ℤ k)
    (list {v[i + 1]} {v[2 * i + 1]} {v[2 * (i + 2) + 1]} {M[i][j + 1]} {M[i (j + 1)]}  ; natural
          {v[2 * (i - 2) + 1]} {v[i - 1]} {v[k + 1]}))                                 ; integer : the test of the sign
  (check-equal? (p36-sum (vector 0 1 2 3 4 5 6 7 8) (vector (vector 1 2 3) (vector 4 5 6)) 1 0 -3)
                (list 2 3 7 5 5 8 0 7))                                              ; v[-1] = 8 , v[-2] = 7
  (define (p36-loop v n)                                    ; (type ℕ k) in the body of a loop
    ANNOTATION-ON
    (type vector? v)
    (let loop ((k 0) (sum 0))
      (type ℕ k)
      (if {k < n}
          (loop {k + 1} {sum + v[k]})
          sum)))
  (check-equal? (p36-loop (vector 1 2 3 4) 4) 10)
  ;; limit cases : an index whose type can not be inferred keeps the general access , nothing breaks
  (define (p36-foo x) (+ x 1))                              ; no type
  (define (p36-g x) 2)
  (type ((natural-fixnum?) procedure? (natural-fixnum?)) p36-g) ; returns a ℕ
  (define (p36-limits v i k b s)
    ANNOTATION-ON
    (type vector? v)
    (type string? s)
    (type ℕ i)
    (list {v[i * (p36-foo k) + 1]}                          ; general (p36-foo has no type)
          {v[(p36-foo k)]}                                  ; general
          {v[i * p36-foo(k) + 1]}                           ; general , neoteric call
          {v[(vector-length v) - 1]}                        ; integer
          {v[i + (if b 1 2)]}                               ; general (the type of an if is not inferred)
          {v[i * (p36-g k) + 1]}                            ; natural (p36-g returns a ℕ)
          {s[i * (p36-foo k)]}                              ; general
          {v[(i + 1)]}))                                    ; natural
  (check-equal? (p36-limits (vector 0 1 2 3 4 5 6 7 8 9) 1 2 #t "abcdefg") (list 4 3 4 9 2 3 #\d 2))


  ;; ================================================================
  ;; 37. The do of Scheme : the types of its variables written in its
  ;;     body are known by its steps and its test too ({k + 1} is
  ;;     (fx+ k 1) , {sum + v[k]} is (fl+ sum ...)).
  ;; ================================================================
  (define (p37-sum v n)
    (type #(flonum?) v)
    (type ℕ n)
    (do ((k 0 {k + 1})
         (sum 0.0 {sum + v[k]}))
        ({k = n} sum)
      (type ℕ k)
      (type flonum? sum)))
  (check-equal? (p37-sum (vector 1.0 2.0 3.5) 3) 6.5)
  (define (p37-product A B n)                               ; the product of matrices of the benchmarks
    (type #(#(flonum?)) A B)
    (type ℕ n)
    {C <+ (build-vector n (lambda (i) (make-vector n 0.0)))}
    (type #(#(flonum?)) C)
    (do ((i 0 {i + 1})) ({i = n})
      (type ℕ i)
      #;($+> {Ai := A[i]}                                   ; $+> : the definitions in the body of a do
           {Ci := C[i]}
           (do ((j 0 {j + 1})) ({j = n})
             (type ℕ j)
             (do ((k 0 {k + 1})
                  (sum 0.0 {sum + Ai[k] * B[k][j]}))
                 ({k = n} {Ci[j] := sum})
               (type ℕ k)
               (type flonum? sum))))
      {Ai <+ A[i]}                                          ; the body of a do can contain definitions
      {Ci <+ C[i]}
      (do ((j 0 {j + 1})) ({j = n})
        (type ℕ j)
        (do ((k 0 {k + 1})
             (sum 0.0 {sum + Ai[k] * B[k][j]}))
            ({k = n} {Ci[j] := sum})
          (type ℕ k)
          (type flonum? sum))))
    C)
  (check-equal? (p37-product (vector (vector 1.0 2.0) (vector 3.0 4.0)) (vector (vector 1.0 2.0) (vector 3.0 4.0)) 2)
                (vector (vector 7.0 10.0) (vector 15.0 22.0)))


  ;; ================================================================
  ;; 38. The type of an if : the type of its branches when it is the
  ;;     same ; an assignment annotates its right hand side with the
  ;;     types of before ({k := (if ... (quotient k 2) ...)} : k stays
  ;;     a fixnum , (remainder k 2) is (fxremainder k 2)).
  ;; ================================================================
  (define (p38-flight k)                                    ; the do ... while of Scheme+
    (type fixnum? k)
    {len <+ 0}
    (unless {k = 1}
      (do {k := (if {(remainder k 2) = 0} (quotient k 2) {3 * k + 1})}
          {len := len + 1}
        while {k <> 1}))
    len)
  (check-equal? (map p38-flight '(1 2 3 6 7 27)) '(0 1 7 8 16 111))
  (define (p38-if x b)                                      ; the if of Scheme+ with then and else
    (type flonum? x)
    {y := (if b then (+ 0 0) {x * 2.0} else {x + 1.0})}
    {y * y})
  (check-equal? (list (p38-if 3.0 #t) (p38-if 3.0 #f)) '(36.0 16.0))
  (define (p38-cdr example i)                               ; any? (the return type of cdr in scheme-types.rkt) is unknown :
    (type ℕ i)                                              ; it does not replace the declared vector? ({y[i]} : vector-ref)
    (declare y)
    (type vector? y)
    {y := (cdr example)}
    {y[i]})
  (check-equal? (p38-cdr (cons #(1 2) #(3 4 5)) 2) 5)


  ;; ================================================================
  ;; 39. lambda+ : a lambda with return , the infix between { } as
  ;;     everywhere (converted at the reading).
  ;; ================================================================
  (define p39-foo (lambda+ () (return {3 * 5 + 2}) 'not_good))
  (check-equal? (p39-foo) 17)
  (define p39-first-negative                                ; return from a loop
    (lambda+ (v)
      (for-each (λ (x) (when {x < 0} (return x))) v)
      #f))
  (check-equal? (list (p39-first-negative '(3 -2 5 -7)) (p39-first-negative '(1 2))) '(-2 #f))
  (check-equal? ((lambda+ (a b . L) {a + b + (length L)}) 1 2 'x 'y) 5) ; the parameters
  (check-equal? ((lambda+ L (length L)) 'x 'y 'z) 3)
  (define p39-typed                                         ; typed in its body : (fl+ x 1.0)
    (lambda+ (x)
      (type flonum? x)
      {x + 1.0}))
  (check-equal? (p39-typed 2.5) 3.5)
  (check-equal? ((lambda+ (x y) {x * y}) 6 7) 42)                   ; without return : a lambda


  ;; ================================================================
  ;; 40. def : a define when its body uses neither return nor
  ;;     return-rec , one call/cc by call with return only , the
  ;;     call/cc of return-rec only when it is used.
  ;; ================================================================
  (def (p40-plain x) {x * 2})                               ; a define
  (def (p40-return x) (when {x < 0} (return 'negative)) {x * 2})
  (def (p40-first-negative L) (for-each (λ (x) (when {x < 0} (return x))) L) #f)
  (def (p40-rec-search L)                                   ; return-rec : out of all the recursive calls
    (cond ((null? L) 'none)
          ((negative? (car L)) (return-rec (car L)))
          (else (p40-rec-search (cdr L)))))
  (def (p40-fact n) (if {n = 0} 1 {n * (p40-fact {n - 1})}))
  (def (p40-values) (return 1 2 3))
  (def (p40-rest a . L) (list a L))
  (check-equal? (list (p40-plain 4) (p40-return -1) (p40-return 3) (p40-first-negative '(1 -5 2))
                      (p40-rec-search '(3 4 -8 9)) (p40-fact 10) (call-with-values p40-values list) (p40-rest 1 2 3))
                '(8 negative 6 -5 -8 3628800 (1 2 3) (1 (2 3))))


  ;; ================================================================
  ;; 41. The inits of let* and do are annotated with the types they
  ;;     see : the bindings before them and the outer levels , not the
  ;;     variable they define (x outer fixnum : (fx->fl x)).
  ;; ================================================================
  (define (p41-let* x)
    (type fixnum? x)
    (let* ((x (exact->inexact x))                           ; the outer x : (fx->fl x)
           (y {x * 2.0}))                                   ; the x of the let* , a flonum : fl*
      y))
  (check-equal? (p41-let* 3) 6.0)
  (define (p41-do k)
    (type fixnum? k)
    (do ((k (exact->inexact k) {k + 0.5}))                  ; the init : the outer k (fx->fl k) , the step : fl+
        ({k > 5.0} k)
      (type flonum? k)))
  (check-equal? (p41-do 3) 5.5)


  ;; ================================================================
  ;; 42. M[i j] and M[i][j] are the same access to a vector of vectors :
  ;;     M[i j] is typed as M[i][j] (before : one level , #(flonum?) ,
  ;;     and an overload of the vectors was chosen for a float).
  ;; ================================================================
  (define (p42-vadd u v) (vector-map + u v))
  (define (p42-fladd a b) (+ a b))
  (overload-procedure p42-add (vector?) p42-vadd (vector? vector?))
  (overload-procedure p42-add (flonum?) p42-fladd (flonum? flonum?))
  (define (p42-sum M)
    (type #(#(flonum?)) M)
    {y := M[0 1]}                                           ; flonum? (was #(flonum?))
    (list (p42-add y y)
          (p42-add {(vector (vector -1.0 -3.0) (vector 1.5 -1.0))[1 0]} 1.0) ; a literal container
          (p42-add {M[1][0]} 1.0)))
  (check-equal? (p42-sum (vector (vector 1.0 2.0) (vector 3.0 4.0))) '(4.0 2.5 4.0))

  ;; ================================================================
  ;; 43. ℝ⁺ (nonnegative-flonum?) : a float not negative , -0.0 and
  ;;     +nan.0 included as in Typed Racket. sqrt , √ , log , expt of a
  ;;     ℝ⁺ are flsqrt , fllog , flexpt ; abs , exp of a float are ℝ⁺ ;
  ;;     sqrt of a flonum? stays generic ((sqrt -4.0) is a complex). The
  ;;     lengths are ℕ. The expected values are computed by the generic
  ;;     procedures , without the annotation (ANNOTATION-OFF).
  ;; ================================================================
  (define (p43-pos x y)
    (type ℝ⁺ x)
    (type flonum? y)
    (list (sqrt x) {(log x) + 1.0} {x ** 0.5} (sqrt (abs y)) (√ (exp y)) (sqrt y)))
  (define (p43-generic x y)
    ANNOTATION-OFF
    (list (sqrt x) {(log x) + 1.0} {x ** 0.5} (sqrt (abs y)) (√ (exp y)) (sqrt y)))
  (check-equal? (p43-pos 4.0 -4.0) (p43-generic 4.0 -4.0)) ; (sqrt -4.0) is the complex 0.0+2.0i
  (check-equal? (p43-pos -0.0 +nan.0) (p43-generic -0.0 +nan.0)) ; -0.0 : (log -0.0) is -inf.0
  (define (p43-last v)
    (type #(flonum?) v)
    {v[(vector-length v) - 1]})                             ; (vector-length v) is ℕ
  (check-equal? (p43-last (vector 1.0 2.0 3.0)) 3.0)

  ;; ================================================================
  ;; 44. The sum and the product of ℝ⁺ are ℝ⁺ , the square of a float
  ;;     too ({x ²} , {x ** 2} , {x * x}) : sqrt of a sum of squares is
  ;;     flsqrt. Compared to the generic procedures (ANNOTATION-OFF).
  ;; ================================================================
  (define (p44-pos x y z a b)
    (type ℝ x y z)
    (type ℝ⁺ a b)
    (list (sqrt {x ² + y ²}) (sqrt {x * x + y * y + z ** 2}) (sqrt {a * b}) (log {a + b}) (sqrt {(x - y) ²})))
  (define (p44-generic x y z a b)
    ANNOTATION-OFF
    (list (sqrt {x ² + y ²}) (sqrt {x * x + y * y + z ** 2}) (sqrt {a * b}) (log {a + b}) (sqrt {(x - y) ²})))
  (check-equal? (p44-pos 3.0 4.0 -12.0 2.0 8.0) (p44-generic 3.0 4.0 -12.0 2.0 8.0))
  (check-equal? (p44-pos -0.0 +nan.0 +inf.0 -0.0 +inf.0) (p44-generic -0.0 +nan.0 +inf.0 -0.0 +inf.0))

  ;; ================================================================
  ;; 45. An integer literal with a float is written as a float at the
  ;;     reading : {x * 2} is (fl* x 2.0) , {1 - x} (fl- 1.0 x) ,
  ;;     {x < 1} (fl< x 1.0) , {3 * x * 2} from the left. Not 0 (the
  ;;     exact 0 of (* 0 x)) , nor beyond 2^53. Compared to the generic
  ;;     procedures (ANNOTATION-OFF) on -0.0 , +nan.0 , +inf.0 too.
  ;; ================================================================
  (define (p45-on x)
    (type ℝ x)
    (list {x * 2} {(x * 2) + 1.0} {1 - x} {2 / x} {x < 1} {x = 3} (max x 3) {3 * x * 2}
          {x * 0} {x + 0} {x * 9007199254740993} {x + 9007199254740992}))
  (define (p45-off x)
    ANNOTATION-OFF
    (list {x * 2} {(x * 2) + 1.0} {1 - x} {2 / x} {x < 1} {x = 3} (max x 3) {3 * x * 2}
          {x * 0} {x + 0} {x * 9007199254740993} {x + 9007199254740992}))
  (for-each (λ (x) (check-equal? (p45-on x) (p45-off x)))
            (list -1.5 3.0 -0.0 +nan.0 +inf.0 1e300))

  ;; ================================================================
  ;; 46. A float and a fixnum : {x + i} is (fl+/fx x i) , {i + x}
  ;;     (fx/fl+ i x) ... of Scheme+/flonum-fixnum : the fixnum
  ;;     converted by fx->fl , with a guard at run time , the generic
  ;;     procedure for the exact 0 ((+ -0.0 0) is -0.0 , (* x 0) is 0 ,
  ;;     (/ x 0) an error) and for a comparison beyond 2^53. Compared to
  ;;     the generic procedures (ANNOTATION-OFF).
  ;; ================================================================
  (define (p46-on x i)
    (type ℝ x)
    (type ℤ i)
    (list {x + i} {i + x} {x - i} {i - x} {x * i} {i * x} {i / x} (min x i) (min i x)
          {x < i} {i <= x} {x = i} {x ≠ i} {i >= x} {x + i + 1.0} {(x * i) + 1.0}))
  (define (p46-off x i)
    ANNOTATION-OFF
    (list {x + i} {i + x} {x - i} {i - x} {x * i} {i * x} {i / x} (min x i) (min i x)
          {x < i} {i <= x} {x = i} {x ≠ i} {i >= x} {x + i + 1.0} {(x * i) + 1.0}))
  (define (p46-divide-on x i)
    (type ℝ x)
    (type ℤ i)
    {x / i})
  (define (p46-divide-off x i)
    ANNOTATION-OFF
    {x / i})
  (for-each (λ (x)
              (for-each (λ (i)
                          (check-equal? (p46-on x i) (p46-off x i))
                          (if (zero? i)
                              (check-exn exn:fail:contract:divide-by-zero? (λ () (p46-divide-on x i)))
                              (check-equal? (p46-divide-on x i) (p46-divide-off x i))))
                        (list 0 1 -3 9007199254740993 -9007199254740993)))
            (list -1.5 3.0 0.0 -0.0 +nan.0 +inf.0 9007199254740992.0))

  ;; 47. ℤ* and ℕ* : the type of an exponentiation stays true. A float to the power of a ℤ* or of a literal not zero is a
  ;;     float , a fixnum to the power of a ℕ or of a literal not negative is an integer ; an exponent only fixnum? gives
  ;;     the generic operations that follow : (expt 2.0 0) is the exact 1 , (expt 2 -1) is 1/2
  (define (p47-f x n) (type flonum? x) (type fixnum? n) {{x ** n} + 1.0})
  (define (p47-f-nonzero x n) (type flonum? x) (type ℤ* n) {{x ** n} + 1.0})
  (define (p47-f-positive x n) (type flonum? x) (type ℕ* n) {{x ** n} + 1.0})
  (define (p47-f-literal x) (type flonum? x) {{x ** 3} + 1.0})
  (define (p47-f-zero x) (type flonum? x) {{x ** 0} + 1.0})
  (define (p47-g a n) (type fixnum? a n) {{a ** n} + 1})
  (define (p47-g-natural a n) (type fixnum? a) (type ℕ n) {{a ** n} + 1})
  (define (p47-g-positive a n) (type fixnum? a) (type ℕ* n) {{a ** n} + 1})
  (define (p47-g-literal a) (type fixnum? a) {{a ** 3} + 1})
  (check-equal? (p47-f 2.0 3) 9.0)
  (check-equal? (p47-f 2.0 0) 2.0)
  (check-equal? (p47-f-nonzero 2.0 3) 9.0)
  (check-equal? (p47-f-nonzero 2.0 -1) 1.5)
  (check-equal? (p47-f-positive 2.0 3) 9.0)
  (check-equal? (p47-f-literal 2.0) 9.0)
  (check-equal? (p47-f-zero 2.0) 2.0)
  (check-equal? (p47-g 2 3) 9)
  (check-equal? (p47-g 2 -1) 3/2)
  (check-equal? (p47-g-natural -2 3) -7)
  (check-equal? (p47-g-natural 2 0) 2)
  (check-equal? (p47-g-positive 2 3) 9)
  (check-equal? (p47-g-literal 2) 9)

  ;; If rackunit printed no FAILURE / ERROR block above, every check passed.
  (displayln "test-curly-infix+ : done")

   ; end module
