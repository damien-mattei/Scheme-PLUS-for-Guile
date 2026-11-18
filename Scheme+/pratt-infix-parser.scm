;; Translated from Scheme+ for Racket (pratt-infix-parser.rkt) for Guile.

;; pratt infix parser

;; This file is part of Scheme+

;; Copyright 2025 Damien MATTEI

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


;; FR : nouveau cœur de précédence infixe pour Scheme+, un parseur de Pratt (binding power + nud/led par
;;      token, une seule boucle recursive), qui remplace !*-generic-infix-parser/!**-generic-infix-parser
;;      (voir infix-with-precedence-to-prefix.rkt) derriere le parametre use-pratt-parser?.
;;
;; EN : new infix precedence core for Scheme+, a Pratt parser (binding power + nud/led per token, a
;;      single recursive loop), replacing !*-generic-infix-parser/!**-generic-infix-parser (see
;;      infix-with-precedence-to-prefix.rkt) behind the use-pratt-parser? parameter.
;;
;; scope (see the migration plan) : unary +/- and chained in/equalities are reimplemented here as a
;; nud and a led, cleanly subsuming plus-minus-parser.rkt / in-equalities.rkt's algorithms -- but as of
;; this change the 4 live entry points of infix-with-precedence-to-prefix.rkt still call those two FSMs
;; in their pre-processing BEFORE reaching this core (unchanged, per the plan), so a raw unary sign or a
;; raw chained in/equality never actually reaches pratt-infix-parser through the live pipeline today ;
;; nud-unary-sign and led-relational are correct and unit-tested directly, but not yet wired end-to-end.
;; Postfix operators and the runtime-deferred dispatch of unknown/overloaded operators stay entirely
;; outside this core too, handled exactly as before by infix-prefix-postfix.rkt and the report mechanism
;; in infix-with-precedence-to-prefix.rkt.

(define-module (Scheme+ pratt-infix-parser)
  #:use-module (Scheme+ racket-compat))

  

  (use-modules
	((Scheme+ in-equalities) #:select (chained-comparisons))
	(Scheme+ operators)
	(Scheme+ operators-list)
	(Scheme+ syntax))

  ;; -------------------------------------------------------------------
  ;; token-key : normalizes a token for TABLE LOOKUP only (deciding which nud/led applies, or whether two
  ;; tokens denote the same operator). The result must never replace the original token in the produced
  ;; tree : a runtime-deferred "operator" can be a raw procedure value (example: (define mult *) used as
  ;; {3 mult 5}) that a creator like (lambda (op a b) (op a b)) must be able to call directly -- replacing
  ;; it by its name would break that call. secure-syntax->datum / procedure->symbol come from syntax.rkt,
  ;; the same normalization already used by datum=?/member-generic.
  ;; -------------------------------------------------------------------
  (define (token-key tok)
    (define d (secure-syntax->datum tok))
    (if (procedure? d)
        (or (procedure->symbol d) d)
        d))

  ;; relational binding power : used both as the lbp that makes the main loop enter led-relational, and
  ;; as the min-bp passed to each individual comparison's own right-operand parse (same value : a chained
  ;; in/equality must not be swallowed by that inner parse, led-relational loops over the chain itself).
  (define relational-bp (operator-bp '<))

  ;; right binding power of unary +/- 's nud : strictly between * and friends (bp 120) and expt/**
  ;; (bp 130), so a trailing exponentiation chain is absorbed ({- 2 ** 4} -> (- (expt 2 4)) = -16) but a
  ;; trailing */%/& is not ({- 3 % 2} -> (% (- 3) 2) = 1) -- verified against the worked examples
  ;; documented in plus-minus-parser.rkt (every one of its "{- ...}" comments was hand-traced against
  ;; this exact rbp while writing this file).
  (define unary-sign-rbp 125)

  (define nud-table (make-hash))
  (define led-table (make-hash)) ; value : (cons bp led-procedure)

  ;; -------------------------------------------------------------------
  ;; nud : parses a single primary expression starting at (car toks). Returns (values left-value
  ;; remaining-toks). A token with no nud entry is an ordinary operand (number, symbol, string, already-
  ;; resolved sub-expression list, syntax object, runtime procedure value, ...) and is returned unchanged.
  ;; -------------------------------------------------------------------
  (define (nud-dispatch toks creator)
    (when (null? toks)
      (error 'pratt-infix-parser "nud : unexpected end of input"))
    (define tok (car toks))
    (define key (token-key tok))
    (define h (and (symbol? key) (hash-ref nud-table key #f)))
    (if h
        (h tok (cdr toks) creator)
        (values tok (cdr toks))))

  ;; -------------------------------------------------------------------
  ;; the main Pratt loop : nud on the first token, then while the next token is a known operator whose
  ;; lbp is > min-bp, apply its led and keep looping on the result. right-associativity and left-
  ;; associativity are both expressed purely through the min-bp each led passes to its own recursive
  ;; parse-expression call (see make-led below) : left-assoc recurses at its own bp (so a following
  ;; same-bp operator fails the > test inside that recursive call and is instead picked up by THIS outer
  ;; loop, nesting left) ; right-assoc recurses at bp-1 (so a following same-bp operator passes the > test
  ;; inside the recursive call and is grabbed there instead, nesting right). Verified by hand against the
  ;; documented examples {3 * - 2 ** 4 + 1} -> -47 and {3 * - 2 ** 4 ** 2} -> -196608 while writing this.
  ;; -------------------------------------------------------------------
  (define (parse-expression toks min-bp creator)
    (define-values (left toks1) (nud-dispatch toks creator))
    (let loop ([left left] [toks toks1])
      (if (null? toks)
          (values left toks)
          (let* ([key (token-key (car toks))]
                 [h (and (symbol? key) (hash-ref led-table key #f))])
            (if (and h (> (car h) min-bp))
                (let-values ([(new-left toks2) ((cdr h) (car toks) left (cdr toks) creator)])
                  (loop new-left toks2))
                (values left toks))))))

  ;; -------------------------------------------------------------------
  ;; nud for unary +/- : matches plus-minus-parser.rkt's begin-operators+- semantics exactly -- a unary
  ;; + is simply dropped (not wrapped), a unary - wraps its operand in (- operand) ; nested signs
  ;; ({- - + - 3 ...}) fall out for free from the ordinary nud recursion, no special-casing needed.
  ;; builds the result directly as (list '- operand) -- the same shape plus-minus-parser.rkt itself
  ;; produces -- rather than going through the generic 2-argument creator, since (see the file banner)
  ;; this nud is not reachable from the live -runtime evaluating caller today.
  ;; -------------------------------------------------------------------
  (define (nud-unary-sign op-tok toks creator)
    (define-values (operand toks1) (parse-expression toks unary-sign-rbp creator))
    (if (eq? (token-key op-tok) '-)
        (values (list '- operand) toks1)
        (values operand toks1)))

  (hash-set! nud-table '+ nud-unary-sign)
  (hash-set! nud-table '- nud-unary-sign)

  ;; -------------------------------------------------------------------
  ;; led for an ordinary binary operator group : right = parse-expression at this operator's own bp
  ;; (left-associative) or bp-1 (right-associative), then creator builds/evaluates the result.
  ;; -------------------------------------------------------------------
  (define (make-led bp right-assoc?)
    (define rbp (if right-assoc? (- bp 1) bp))
    (lambda (op-tok left toks creator)
      (define-values (right toks1) (parse-expression toks rbp creator))
      (values (creator op-tok left right) toks1)))

  ;; -------------------------------------------------------------------
  ;; led for chained/n-ary in/equalities (x < y <= z ...) : replaces in-equalities.rkt's 3 states
  ;; (in/equalities-state-1, binary-in/equalities-state-2, n-ary-in/equality-state-3) for both the bare-
  ;; chain case and the chain-embedded-in-and/or/assignment case -- the enclosing operator's own rbp
  ;; boundary (via the ordinary Pratt loop) already stops this led from being entered when it shouldn't,
  ;; there is no need for a separate lower-precedence-than-in/equalities? lookahead. Collects one
  ;; (creator op left right) comparison per step, chaining left forward to the previous right (x<y<z :
  ;; compares x<y and y<z, not x<y and x<z) ; wraps the collected comparisons in (and ...) only when more
  ;; than one was found, so a single comparison keeps its plain binary shape.
  ;; builds the (and ...) wrapper as a literal list -- correct for a tree-building creator ; like
  ;; nud-unary-sign, not reachable from the live -runtime evaluating caller today (see file banner).
  ;; -------------------------------------------------------------------
  #;(define (led-relational op-tok left toks creator)
    (let loop ([cmp-left left] [op-tok op-tok] [toks toks] [comparisons '()])
      (define-values (right toks1) (parse-expression toks relational-bp creator))
      (define comparisons1 (cons (creator op-tok cmp-left right) comparisons))
      (define next-key (and (pair? toks1) (token-key (car toks1))))
      (if (and next-key (in/equalities-operator? next-key))
          (loop right (car toks1) (cdr toks1) comparisons1)
          (let ([cs (reverse comparisons1)])
            (if (null? (cdr cs))
                (values (car cs) toks1)
                (values (cons 'and cs) toks1))))))

  ;; the operands and operators of the chain are collected , then chained-comparisons (in-equalities.rkt , as the old
  ;; engine) builds the comparisons : an operand of two comparisons is evaluated once
  (define (led-relational op-tok left toks creator)
    (let loop ([op-tok op-tok] [toks toks] [rest-of-chain '()])
      (define-values (right toks1) (parse-expression toks relational-bp creator))
      (define rest-of-chain1 (cons right (cons op-tok rest-of-chain)))
      (define next-key (and (pair? toks1) (token-key (car toks1))))
      (if (and next-key (in/equalities-operator? next-key))
          (loop (car toks1) (cdr toks1) rest-of-chain1)
          (values (chained-comparisons left (reverse rest-of-chain1) creator) toks1))))

  ;; -------------------------------------------------------------------
  ;; build nud/led tables from infix-operators-lst-for-parser (operators-list.rkt, plain-datum groups,
  ;; tightest-binding group first) + operator-bp (operators.rkt) + right-associative? : one hash-table
  ;; entry per operator, so parse-time lookups never need member-generic/datum=? at all.
  ;; -------------------------------------------------------------------
  (let loop ((groups infix-operators-lst-for-parser)
	     (idx 0))
    (unless (null? groups)
      (define group (car groups))
      (define bp (* (- 14 idx) 10))
      (define relational-group? (and (pair? group) (in/equalities-operator? (car group))))
      (for-each (lambda (op)
		  (hash-set! led-table
			     op
			     (cons bp
				   (if relational-group?
				       led-relational
				       (make-led bp (right-associative? op))))))
		group)
      (loop (cdr groups) (+ idx 1))))

  ;; -------------------------------------------------------------------
  ;; pratt-infix-parser(terms creator) -> (list prefix-expr) : signature identical to
  ;; pre-check-!*-generic-infix-parser's canonical-path return shape, a drop-in replacement at the three
  ;; call sites of infix-with-precedence-to-prefix.rkt (behind use-pratt-parser?).
  ;; -------------------------------------------------------------------
  (define (pratt-infix-parser terms creator)
    (if (or (null? terms) (null? (cdr terms)))
        terms
        (let-values ([(result rest) (parse-expression terms -1 creator)])
          (list result))))

  

(export
	pratt-infix-parser)
 ; end module
