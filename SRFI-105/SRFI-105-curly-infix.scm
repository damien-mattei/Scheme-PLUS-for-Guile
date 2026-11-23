;; Translated from Scheme+ for Racket (SRFI-105-curly-infix.rkt) for Guile.

;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Scheme implementations (Racket,...) and Scheme+ by Damien Mattei , 2024 - 2025



(define-module (SRFI-105 SRFI-105-curly-infix)
  #:use-module (Scheme+ racket-compat))

	

	(use-modules
	(Scheme+ nfx)
	(Scheme+ condx)
	(Scheme+ alternating-parameters)
	(Scheme+ operators)
	(Scheme+ infix-with-precedence-to-prefix)
	(Scheme+ source-locations)
	(Scheme+ operators-list)
	((Scheme+ parser-declarations) #:select (verify-if-declaration-for-parser-then-store-it))
	((Scheme+ care-of-quotes) #:select (infix-top-level-only))
	((Scheme+ superscript) #:select (superscript-only?))
	(Scheme+ type))
	

; This is a simplified reference implementation of a curly-infix and
; neoteric reader, intended for a SRFI submission.
; If run, it invokes a curly-infix-reader
; (inside {...}, it accepts neoteric expressions).


(define global-port '())
(define source "unknown")	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;; globals variables that can be modified by coder or by pragma
	
(define srfi-strict #f) ; enable strict compatibility with SRFI 105 that will NOT force some $nfx$ apply almost everywhere

;; the pragma BEGIN-STRICT-SRFI-105-REGION or END-STRICT-SRFI-105-REGION read alone in { } : {BEGIN-STRICT-SRFI-105-REGION}
;; the { } gives the pragma back to the enclosing list , which drops it (without this the empty { } left () in the code)
(define strict-pragma-in-curly #f)

;; partially deprecated as the new parenthesis syntax and insertion of $nfx$ create a recursive parsing with infix autodetection
;; no more used : replaced by the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF (Scheme+/care-of-quotes) ,
;; still defined and exported because the backups of main.rkt display it
(define care-of-quote #t) ; keep quoted expression when #t (no $nfx$ will be inserted in *quoted* curly infix expressions),
;; usefull to use symbolic expressions
;; (but makes debugging harder because quoted expression to debug will not be the same as evaluated unquoted ones)

;; DEPRECATED : let it to #f
(define use-only-syntax-transformers #f) ; use only syntax transformers: syntax transformers are used by scheme+
					; when false the parser will partially do the job of syntax transformers
					; it will do the job for expression between { } but not for 'define and 'define+
					; letting it be done by syntax transformers

;; off by default, set by the pragmas ANNOTATION-ON / ANNOTATION-OFF (see process-annotation-pragma)
;; and reset to #f by main.rkt at the beginning of each file read
(define annot-flag #f) ; activate or deactivate parsing of annotations

(define (get-annot-flag)
  annot-flag)

(define (set-annot-flag! flag)
  (set! annot-flag flag))

;; #t as soon as a pragma ANNOTATION-ON has been read in the file : the annotation pass must then run on the file
;; (the pragmas act by region, see annot+.rkt), reset to #f by main.rkt at the beginning of each file read
(define annot-on-read #f)

(define (get-annot-on-read)
  annot-on-read)

(define (set-annot-on-read! flag)
  (set! annot-on-read flag))

;; the pragmas ANNOTATION-ON / ANNOTATION-OFF : datum is a datum read in the file, either an element
;; of a list (my-read-delimited-list, for example inside a module form) or a datum at the top level
;; of the file (main.rkt).
;; The pragmas act by region : they are no more swallowed, they stay in the code as markers for the annotation pass
;; (annot+.rkt) that removes them. A pragma acts on the elements that follow it in the same list, until the next
;; pragma or the end of the list. annot-flag is the last pragma read (used by the REPL), annot-on-read tells if the
;; annotation pass must run on the file.
;; old comment : The annotation pass is applied when the whole file has been read, so the
;; last pragma read is the one that counts. The pragmas are swallowed : they do not remain in the code.
(define (process-annotation-pragma datum) ; return #t when datum is one of the pragmas
  (cond ((eq? datum ANNOTATION-ON)
	 (set! annot-flag #t)
	 (set! annot-on-read #t)
	 #t)
	((eq? datum ANNOTATION-OFF)
	 (set! annot-flag #f)
	 #t)
	(else #f)))
;; the value of srfi-strict written in this source code : the state at the beginning of each file
(define srfi-strict-at-start srfi-strict)

;; called at the beginning of each file : a strict region not closed by END-STRICT-SRFI-105-REGION does not go on
;; in the next file read (for example a module required by the previous one)
(define (reset-srfi-strict!)
  (set! srfi-strict srfi-strict-at-start))

;; a pragma BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION read at the top level of the file (before the
;; module form) or at the REPL , return #t when datum is one of the pragmas (inside a list they are processed by the reader)
(define (process-strict-pragma datum)
  (cond ((eq? datum 'BEGIN-STRICT-SRFI-105-REGION)
	 (set! srfi-strict #t)
	 #t)
	((eq? datum 'END-STRICT-SRFI-105-REGION)
	 (set! srfi-strict #f)
	 #t)
	(else #f)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; library procedures and macro
(define insert cons)

;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))



;; procedures and global variables to deal with quoted,quasiquoted,unquoted,etc binary curly infix regions

;; Notes on the modified algorithm:
;; the original curly infix parser of SRFI 105 is written in a style which is functional recursive
;; instead of using some stack. Modifying this algorithm should have be by adding an extra parameter
;; defining the region context (quoted/unquoted) but this schema would have cause to pass
;; an extra parameter in a lot of functions with the risk of an error in coding and a lot of work.
;; I had intially and preferred to add a more complex stack but with less modifications in
;; existing code. Then the modifications are only in parts dealing with quote,quasiquote,unquote
;; unquote-splacing , sexpr,and of course process-curly but not in neoteric expressions
;; and others intermediate procedures.

;; instantiate an empty stack and provide methods (push,pop...)

;; note: the stack code is so simple that it is better inlined and hard to put in a module
(define stack '())

(define (push x) (set! stack (cons x stack)))

(define (pop)
  (when (null? stack)
    (read-error "SRFI-105-curly-infix : pop : EMPTY STACK ERROR")) 
  (define x (car stack))
  (set! stack (cdr stack))
  x)


;; definitions for forcing or releasing the parsing of quoted (pseudoquoted too) expressions

;; #t : quoted region (quote,quasiquote)
;; #f : unquoted region (unquote,unquote-splicing)
(define region-quote #f) ; intial value at launching

(define (find*quote) ; quote , quasiquote but NOT unquote
  (push region-quote) ; store the current mode on top of the stack
  (set! region-quote #t))

(define (find-unquote*)
  (push region-quote) ; store the current mode on top of the stack
  (set! region-quote #f))

(define (end-region) ; called when we find the end of a region
  (set! region-quote (pop))) ; fallback to the previous mode stored on the stack




(define (check*quote datum)
  (if (quote-or-quasi? datum)
      (find*quote) ; return unspecified which is true
      #f))

(define (check-unquote* datum)
  (if (unquote-or-splicing? datum)
      (find-unquote*) ; return unspecified which is true
      #f))

(define (check*quote* datum)
  (or (check*quote datum)
      (check-unquote* datum)))


(define comment #f) ; used for comment removal

  ;; Guile : #t when the last datum given by curly-infix-read was a datum comment #;<datum> at the top level , to be dropped
  ;; (the reader of #lang in Racket drops it , main.rkt)
  (define (read-was-a-datum-comment?) comment)

(define cpt 0) ; used for internal debug , will count the datum and display debug infor for the nth data

;; DEPRECATED
;(define dpth 0) ; depth in sexpr

(define postfix-flag #f) ; when we find a postfix operator annotation

;; replaced by verify-if-declaration-for-parser-then-store-it of Scheme+/parser-declarations (postfix operators , procedures , data)
#;(define (verify-if-declare-postfix-operator-then-store-it datum) ; TODO the same in SRFI 110
  (when (and (list? datum)
             (not (null? datum))
             (eq? (car datum) 'declare-postfix-operator)
             (not comment)) ; also test for comment flag !!!
    ;(set! postfix-flag #t) ; decomment if you want to delete expression from result file and modify the region calling this function
    (insert-in-postfix-lst (cadr datum))))
    

  ; ------------------------------
  ; Curly-infix support procedures
  ; ------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix? op lyst)
    (cond
     ((null? lyst) #t)
     ((not (pair? lyst)) #f)
     ((not (equal? op (car lyst))) #f) ; fail - operators not the same
     ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
     (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ;; now imported from Scheme+/alternating-parameters
  ;; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  ;; (alternating-parameters '(< 3 < y <= z))
  ;; '(< < <=)
  ;; (define (alternating-parameters lyst)
  ;;   (if (or (null? lyst) (null? (cdr lyst)))
  ;;     lyst
  ;;     (cons (car lyst) (alternating-parameters (cddr lyst)))))



  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
(define (transform-mixed-infix lyst)
  ;;(display "lyst=") (display lyst) (newline)
  (cons '$nfx$ lyst)) ; ($nfx$ a b c ...)



  ; Given curly-infix lyst, map it to its final internal format.
(define (process-curly lyst)

  ;;(display "SRFI-105 : process-curly lyst=") (display lyst) (newline)

  (if use-only-syntax-transformers

      (cond 

       ;; E.G., map {} to ().
       
       ((not (pair? lyst)) lyst) ; E.G., map {} to ().


       
       ;; Map {a} to a.
       
       ((null? (cdr lyst)) ; Map {a} to a.

	(if srfi-strict
	    (car lyst)  ; original version
	    
	    ;; {(3.7 + 1)}
	    ;; ($nfx$ (3.7 + 1))
	    ;; 4.7
	    
	    ;;{3.7}
	    ;;($nfx$ 3.7)
	    ;;3.7
	    (list '$nfx$ (car lyst)))) ; ($nfx$ a)

       

       ;; Map {a b} to (a b).
       
       ((and (pair? (cdr lyst))
	     (null? (cddr lyst))) ; Map {a b} to (a b).

	(if srfi-strict
	    lyst

	    ;; '{abs (3.7 + 1)}
	    ;; '($nfx$ abs (3.7 + 1))

	    ;; {abs (3.7 + 1)}
	    ;; ($nfx$ abs (3.7 + 1))
	    ;; 4.7

	    ;; (define (h x y) {abs ((cos (x + y)) * (sin (x - y))) } )
	    ;; (define (h x y) ($nfx$ abs ((cos (x + y)) * (sin (x - y)))))
	    ;; #<eof>
	    ;; (h  .2 .3)
	    ;; (h 0.2 0.3)
	    ;; 0.08761206554319241
	    (cons '$nfx$ lyst))) ; ($nfx$ a b)



       ;; Map {a OP b [OP c...]} to (OP a b [c...])
       
       ;; deal quoted and quasi-quoted the old way
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '(- (2 + 3) (5 - 7) 2)
       
       ;; '{(2 + 3) - (5 - 7)}
       ;; '(- (2 + 3) (5 - 7))
       ((and (simple-infix-list? lyst)
	     (or (and care-of-quote
		      region-quote)
		 srfi-strict)) ; Map {a OP b [OP c...]} to (OP a b [c...])
	
	(cons (cadr lyst)
	      (alternating-parameters lyst))) ; put the list in prefix (at least at top level, nested lists stays as they are)
       
       ;; comment above force this (which is not what i want):
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '($nfx$ (2 + 3) - (5 - 7) - 2)

       ;; '{(2 + 3) - (5 - 7)}
       ;; '($nfx$ (2 + 3) - (5 - 7))

       ;; `{{2 + 3} - ,{2 + 1}}
       ;; `($nfx$ ($nfx$ 2 + 3) - ,($nfx$ 2 + 1))
       ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 + 1)>
       ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
       ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
       ;; '($nfx$ ($nfx$ 2 + 3) - 3)


       ;; general case
       
       (#t ; will insert $nfx$ in front of list
	(transform-mixed-infix lyst))) 


      
      ;; `{{2 + 3} - ,{2 + 1}}
      ;; `(- (+ 2 3) ,($nfx$ 2 + 1))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (2 + 1)>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
      ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
      ;; '(- (+ 2 3) 3)

      ;; {x <- (1 + 2 + 3) - (4 + 5)}
      ;; ($nfx$ x <- (1 + 2 + 3) - (4 + 5))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (x <- (1 + 2 + 3) - (4 + 5))>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax x> .#<syntax <-> .#<syntax (1 + 2 + 3)> .#<syntax -> .#<syntax (4 + 5)>)
      ;; $nfx$ : parsed-args=.#<syntax (<- x (- (+ 1 2 3) (+ 4 5)))>
      ;; #<eof>
      ;; x
      ;; x
      ;; -3

      

      ;; '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}}


      ;; '(or (and (not a) (not b) (not c) (not d))
      ;;      (and (not a) (not b) (not c) d)
      ;;      (and (not a) (not b) c (not d))
      ;;      (and (not a) b (not c) d)
      ;;      (and (not a) b c (not d))
      ;;      (and (not a) b c d)
      ;;      (and a (not b) (not c) (not d))
      ;;      (and a (not b) (not c) d)
      ;;      (and a (not b) c (not d))
      ;;      (and c (not d)))
      ;; '(or (and (not a) (not b) (not c) (not d))
      ;;      (and (not a) (not b) (not c) d)
      ;;      (and (not a) (not b) c (not d))
      ;;      (and (not a) b (not c) d)
      ;;      (and (not a) b c (not d))
      ;;      (and (not a) b c d)
      ;;      (and a (not b) (not c) (not d))
      ;;      (and a (not b) (not c) d)
      ;;      (and a (not b) c (not d))
      ;;      (and c (not d)))


      ;; #<eof>
      ;; {expr <- '(((not a) and (not b) and (not c) and (not d)) or ((not a) and (not b) and (not c) and d) or ((not a) and (not b) and c and (not d)) or ((not a) and b and (not c) and d) or ((not a) and b and c and (not d)) or ((not a) and b and c and d) or (a and (not b) and (not c) and (not d)) or (a and (not b) and (not c) and d) or (a and (not b) and c and (not d)) or (c and (not d)))}


      ;; ($nfx$
      ;;  expr
      ;;  <-
      ;;  '(((not a) and (not b) and (not c) and (not d))
      ;;    or
      ;;    ((not a) and (not b) and (not c) and d)
      ;;    or
      ;;    ((not a) and (not b) and c and (not d))
      ;;    or
      ;;    ((not a) and b and (not c) and d)
      ;;    or
      ;;    ((not a) and b and c and (not d))
      ;;    or
      ;;    ((not a) and b and c and d)
      ;;    or
      ;;    (a and (not b) and (not c) and (not d))
      ;;    or
      ;;    (a and (not b) and (not c) and d)
      ;;    or
      ;;    (a and (not b) and c and (not d))
      ;;    or
      ;;    (c and (not d))))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (expr <- (quote (((not a) and...>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax expr> .#<syntax <-> .#<syntax (quote (((not a) and (not b) ...>)
      ;; $nfx$ : parsed-args=.#<syntax (<- expr (quote (((not a) and...>


      ;; #<eof>
      ;; expr


      ;; expr
      ;; '(((not a) and (not b) and (not c) and (not d))
      ;;   or
      ;;   ((not a) and (not b) and (not c) and d)
      ;;   or
      ;;   ((not a) and (not b) and c and (not d))
      ;;   or
      ;;   ((not a) and b and (not c) and d)
      ;;   or
      ;;   ((not a) and b and c and (not d))
      ;;   or
      ;;   ((not a) and b and c and d)
      ;;   or
      ;;   (a and (not b) and (not c) and (not d))
      ;;   or
      ;;   (a and (not b) and (not c) and d)
      ;;   or
      ;;   (a and (not b) and c and (not d))
      ;;   or
      ;;   (c and (not d)))


      ;; #<eof>

      ;; warning the above expression is now parsed and result in: (no more depending of care of quote flag)
      ;;expr
      ;; '(or (and (not a) (not b) (not c) (not d))
      ;;      (and (not a) (not b) (not c) d)
      ;;      (and (not a) (not b) c (not d))
      ;;      (and (not a) b (not c) d)
      ;;      (and (not a) b c (not d))
      ;;      (and (not a) b c d)
      ;;      (and a (not b) (not c) (not d))
      ;;      (and a (not b) (not c) d)
      ;;      (and a (not b) c (not d))
      ;;      (and c (not d)))


      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; else : limited use of  syntax transformers
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; {3 * 5 + 2}
      ;; (+ (* 3 5) 2)
      ;; 17
      
      (condx

       ;; E.G., map {} to ().
       
       ((not (pair? lyst)) lyst) ; E.G., map {} to ().

       ;; {a . z} : an improper list , as in SRFI 105 (without this : cddr contract violation)
       ((not (list? lyst)) (transform-mixed-infix lyst)) ; ($nfx$ a . z)


       ;; Map {a} to a.
       
       ((null? (cdr lyst)) ; Map {a} to a.

	(if #;srfi-strict (or srfi-strict region-quote) ; in a quote {a} is a , as in SRFI 105 : the quotes are respected (Scheme+/care-of-quotes)
	    (car lyst)  ; original version
	    
	    ;; {(3.7 + 1)}
	    ;; ($nfx$ (3.7 + 1))
	    ;; 4.7
	    
	    ;;{3.7}
	    ;;($nfx$ 3.7)
	    ;;3.7
	    (nfx (car lyst)))) ; ($nfx$ a)
	    ;;(read-error "SRFI-105-curly-infix : process-curly : DEBUG lyst =" lyst)))

       ;; Map {a b} to (a b).
       
       ((and (pair? (cdr lyst))
	     (null? (cddr lyst))) ; Map {a b} to (a b).

	(if #;srfi-strict (or srfi-strict region-quote) ; in a quote {a b} is (a b) , as in SRFI 105
	    lyst

	    ;; '{abs (3.7 + 1)}
	    ;; '($nfx$ abs (3.7 + 1))

	    ;; {abs (3.7 + 1)}
	    ;; ($nfx$ abs (3.7 + 1))
	    ;; 4.7

	    ;; (define (h x y) {abs ((cos (x + y)) * (sin (x - y))) } )
	    ;; (define (h x y) ($nfx$ abs ((cos (x + y)) * (sin (x - y)))))
	    ;; #<eof>
	    ;; (h  .2 .3)
	    ;; (h 0.2 0.3)
	    ;; 0.08761206554319241
	    (apply nfx lyst))) ; ($nfx$ a b)

       
       ;; list of operators
       (exec
	(define operands (alternating-parameters lyst))
	(define oper (cadr lyst)) ; first operator of list
	(define mbr+- (and (or (eq? oper '+) (eq? oper '-)) ; the +/- chained-sign ambiguity (superscript) only makes sense when the repeating operator itself is + or - ,otherwise a bare + or - operand (e.g. {x := +}) is a legitimate value, not a disguised sign
			   (or (member '+ operands) ; there could be + - + + , superscripts ,so operators could be wrong
			       (member '- operands)))) ; note that when superscript is not deal here (superscript could not be in place of operators)
					; {x := +} -> #f
					; {+ - 2} -> #t
					; {5 - 2} -> #f
	(define sil (simple-infix-list? lyst))
	(define infx (not (eq? oper 'if))) ; true infix, not Python 'statement if test else statement2'
	;;(read-error "SRFI-105-curly-infix : infx =" infx)
	); when all operators are the same

       
       ;; Map {a OP b [OP c...]} to (OP a b [c...])
       
       ;; deal quoted and quasi-quoted the old way
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '(- (2 + 3) (5 - 7) 2)
       
       ;; '{(2 + 3) - (5 - 7)}
       ;; '(- (2 + 3) (5 - 7))
       ; old condition :
       ; ((and infx ; true infix
       ;       sil ; simple infix list , when all operators are the same
       ;       (or (and care-of-quote
       ;                region-quote)
       ;           srfi-strict)) ; Map {a OP b [OP c...]} to (OP a b [c...])
       ((and sil ; simple infix list , when all operators are the same
	     (or srfi-strict ; SRFI 105 : (OP a b [c...]) , even with if
		 (and infx ; true infix , not the Python if of Scheme+
		      region-quote))) ; in a quote the operands are data , their ( ) are not converted (as in SRFI 105)
	
	(cons oper ; first operator of list
	      operands))  ; Map {a OP b [OP c...]} to (OP a b [c...])

       
       ;; comment above force this (which is not what i want):
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '($nfx$ (2 + 3) - (5 - 7) - 2)

       ;; '{(2 + 3) - (5 - 7)}
       ;; '($nfx$ (2 + 3) - (5 - 7))

       ;; `{{2 + 3} - ,{2 + 1}}
       ;; `($nfx$ ($nfx$ 2 + 3) - ,($nfx$ 2 + 1))
       ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 + 1)>
       ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
       ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
       ;; '($nfx$ ($nfx$ 2 + 3) - 3)

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


       
       ;; this allows some infix macros
       ;; (define-syntax +=
       ;;     (syntax-rules ()
       ;;       ({var1 _ var2} {var1 := var1 + var2})))

       ;; (define-syntax += (syntax-rules () ((_ var1 var2) (:= var1 (+ var1 var2))))) ; parsed result

       ;; {x := 3}
       ;; {x += 7}
       ;; x
       ;; 10

       ;; (define-syntax plus
       ;;     (syntax-rules ()
       ;;       ({var1 _ ...} {var1 + ...})))

       ;; (define-syntax plus (syntax-rules () ((_ var1 ...) (+ var1 ...)))) ; parsed result

       ;; {2 plus 3}
       ;; (plus 2 3) ; parsed result
       ;; 5

       ;; {2 plus 3 plus 4 plus 5 plus 6}
       ;; (plus 2 3 4 5 6)  ; parsed result
       ;; 20
       ((and infx ; true infix
	     sil ; simple infix list , when all operators are the same
	     (not mbr+-) ; could be there + - + + , superscripts ,so operators could be wrong
	     (not (ormap superscript-only? lyst)) ; a superscript (exponent) needs the general parser (superscript-only? : not * and · , that are also normal operators : {x := *} is (:= x *)) : {2 ⁴ ²} is (** 2 4 2) , not (⁴ 2 ²) , {- 2 ³} is (- (** 2 3))
	     (not (or (eq? oper 'not) (member-in-prefix-unary-lst oper))) ; {not not a} is (not (not a)) , {~ ~ a} is (~ (~ a)) : prefix operators (also those declared in the file) , grouped by the Scheme+ parser (group-prefix-operators)
	     (not (right-associative? oper))) ; {2 expt 3 expt 2} is not (expt 2 3 2) , expt has 2 arguments : the right associative operators are parsed by the precedence engine
					; {2 - + - 3 - 4} will not be parsed here but in another case of cond

	(define deep-terms (map (lambda (x) ; deep terms (operands) should be parsed by Scheme+
				  (!*prec-generic-infix-parser-rec-prepare x
									   (lambda (op a b)
									     (list op a b)))) ; creator
				operands))
	(cons oper
	      deep-terms))

       
       ;; general case
       
       (#t ; apply nfx to the list
	#;(apply nfx lyst)
	(cond (srfi-strict (transform-mixed-infix lyst)) ; SRFI 105 : ($nfx$ a b c ...)
	      (region-quote (infix-top-level-only lyst)) ; in a quote only the top level is infix , the ( ) are data (Scheme+/care-of-quotes)
	      (else (apply nfx lyst))))))) ; execute (nfx a b c ...)





  ; ------------------------------------------------
  ; Key procedures to implement neoteric-expressions
  ; ------------------------------------------------

  ; Read the "inside" of a list until its matching stop-char, returning list.
  ; stop-char needs to be closing paren, closing bracket, or closing brace.
  ; This is like read-delimited-list of Common Lisp.
  ; This implements a useful extension: (. b) returns b.
(define (my-read-delimited-list my-read stop-char port)
  
  (let*
      
      ((c   (peek-char port))) ; peek a char without really getting it out of the port
    
      (cond
       ((eof-object? c)
	(define error-message "Error: EOF in middle of list")
	(define-values (line col pos) (port-next-location port))
	(raise-read-eof-error error-message	 	 	 	 
 	 		      source	 	 
 	 		      line	 	 	 	 
 	 		      col	 	 	 	 
 	 		      pos	 	 	 	 
 	 		      #f)
	'()) ; error EOF
       
       ((eqv? c #\;) ; read a comment until end of line
        (consume-to-eol port)
        (my-read-delimited-list my-read stop-char port))
       
       ((my-char-whitespace? c) ; skip the white space
        (read-char port) ; really read the white space
        (my-read-delimited-list my-read stop-char port))
       
       ((char=? c stop-char) ; stop char ,return '() ? perheaps for non empty statement or because we really have an empty list
        (read-char port) ; really read the stop char
	;;(set! dpth (- dpth 1))
        '())
       
       ((or (eq? c #\)) (eq? c #\]) (eq? c #\})) ; if it was not one of the previous cases it is bad
        (read-char port) ; really read the bad char
        (read-error "Bad closing character"))

       (#t ; here we should be ready to read something serious (token, expression,...)
	;;(read-error "my-read=" my-read) ; my-read= #<procedure:curly-infix-read-real>
        (let* ((datum-start (location-start port)) ; here the spaces and the comments are skipped
	       (datum (my-read port)) ; should read a token, i think scheme read it and return a real scheme object (for example a quoted symbol or a string of type string), thus it is not returning code as string or as syntax (? to be checked on complex expression) but code as (quoted) sexpr.
	       (datum-location (location-from datum-start port))
	       (strict-srfi-105-pragma #f)
	       (annotation-pragma #f) ; ANNOTATION-ON / ANNOTATION-OFF found : drop the datum
	       (q-reg (check*quote* datum))) ; *quote* (i mean backquote,quasiquote ...) region and also set a local flag for entering a critical region

	  ;;(when q-reg
	  ;;(display "datum=") (display datum)(newline)) ; datum would contain quote,quasiquote,unquote,unquote-splicing,etc... push

	  ;; for test only
	  ;; (when (eq? datum 'newline)
	  ;;   (read-error "newline test passed"))

	  ;; (when (= cpt 1)
	  ;;   (define flg-str (string? datum))
	  ;;   (error "SRFI 105 curly infix : first datum =" (list datum flg-str)))
	  ;; (set! cpt (+ 1 cpt))

	
	  (when (eq? datum 'BEGIN-STRICT-SRFI-105-REGION) ; note: eq? is ok but equal? could be better
	    (set! strict-srfi-105-pragma #t)
	    (set! srfi-strict #t)
	    (when (eqv? stop-char #\}) ; {BEGIN-STRICT-SRFI-105-REGION}
	      (set! strict-pragma-in-curly datum)))

	  (when (eq? datum 'END-STRICT-SRFI-105-REGION)
	    (set! strict-srfi-105-pragma #t) ; should be let true !
	    (set! srfi-strict #f)
	    (when (eqv? stop-char #\}) ; {END-STRICT-SRFI-105-REGION}
	      (set! strict-pragma-in-curly datum)))

	  #;(set! annotation-pragma (process-annotation-pragma datum)) ; pragmas ANNOTATION-ON / ANNOTATION-OFF , swallowed like the strict pragmas
	  (process-annotation-pragma datum) ; pragmas ANNOTATION-ON / ANNOTATION-OFF : kept in the list, as markers of the regions for the annotation pass (annot+.rkt)

	  (cond ; in fact an if then else

	   ;; here we got chars ... (not symbols)
	   ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
	   ((eq? datum (string->symbol (string #\.))) ;; only this one works
	    
            (let ((datum2 (my-read port)))
              (consume-whitespace port) ; reading white space between . restargs ?
              (cond
               ((eof-object? datum2)
                (read-error "Early eof in (... .)\n")
                '())
               ((not (eqv? (peek-char port) stop-char))
                (read-error "Bad closing character after . datum"))
               (#t
                (read-char port)
                datum2))))
	   
	   
	   (#t
	    
	    ;; here we get the symbolic scheme expression (but it is constructed recursively,only at the end we get the correct full expression)

            ;; datum should be an expression , atom or list
	    
	    ;; (let ((expression 
	    ;; 	     (cons datum
	    ;; 		   (my-read-delimited-list my-read stop-char port))))

            ; also test for comment flag !!!
            #;(verify-if-declare-postfix-operator-then-store-it datum)
            ; postfix operators , procedures and macros defined or typed , data typed : what the infix parser must know
            (unless comment
              (verify-if-declaration-for-parser-then-store-it datum))
            #;(when (and (list? datum)
                       (not (null? datum))
                       (eq? (car datum) 'declare-postfix-operator)
                       (not comment))
              (set! postfix-flag #t)
              (insert-in-postfix-lst (cadr datum))
              ;(display "SRFI-105-curly-infix.rkt : (get-postfix-lst) :") (display (get-postfix-lst)) (newline)
              ;(error "SRFI-105 : postfix declaration found:" datum)
              )

	    (let ((expression '()))

	      (if (or strict-srfi-105-pragma annotation-pragma comment #;postfix-flag) ; drop the datum
                  (begin
                    (when comment
                      (set! comment #f)) ; reset the comment flag before
                    (when postfix-flag
                      (set! postfix-flag #f))
                    (set! expression (my-read-delimited-list my-read stop-char port))) ; drop the datum as it is a pragma directive or a commented expression (or postfix-declare)
		  ; keep the datum
		  #;(set! expression (cons datum ;; normal case
					 (my-read-delimited-list my-read stop-char port)))
		  (set! expression (record-datum! (cons datum ;; normal case
							(my-read-delimited-list my-read stop-char port))
						  datum
						  datum-location)))
	      
	      (when q-reg
		;;(display "expression=") (display expression) (newline) ; here we possibly have finished a *quote* region ,pop
		(end-region)) ; pop !

	      ;; for test only (dpth is not depth but near length and negative?)
	      ; but we can find here an exact expression
	      ;; (when (equal? expression '(list 1 2 3 (list 4 44) 5 67 8))
	      ;;   (error "test passed" dpth))
	      
	      ;; (when (equal? expression '(declare-postfix-operator fact))
	      ;;   (error "test passed :" expression))
	      
	      ;;(when (= dpth 2)
	      ;;	(error "SRFI-105 debug: expression=" expression))
	      ;(set! dpth (- dpth 1))
	      
	      expression))))))))







  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
(define (neoteric-process-tail port prefix)
  
  (let* ((c (peek-char port)))
    
    (cond
     
	 ((eof-object? c) prefix)

	 ;;   f = prefix
	 
	 ((char=? c #\( ) ; Implement f(x)
            (read-char port)
            (neoteric-process-tail port
				   (cons prefix (my-read-delimited-list neoteric-read-real #\) port))))

	   ((char=? c #\[ )  ; Implement f[x] 
	    (read-char port)
	    (neoteric-process-tail port
				   `($bracket-apply$ ,prefix ,@(my-read-delimited-list neoteric-read-real #\] port))))
	  
          ((char=? c #\{ )  ; Implement f{x}
            (read-char port)
            (neoteric-process-tail port
              (let ((tail (let ((lyst (my-read-delimited-list neoteric-read-real #\} port)))
			    #;(process-curly lyst)
			    (relink-symbols! (process-curly lyst) lyst)))) ; the symbols keep their locations
                (if (eqv? tail '())
                  (list prefix) ; Map f{} to (f), not (f ()).
                  (list prefix tail)))))
	  
          (#t prefix))))


  ; To implement neoteric-expressions, modify the reader so
  ; that [] and {} are also delimiters, and make the reader do this:
  ; (let* ((prefix
  ;           read-expression-as-usual
  ;       ))
  ;   (if (eof-object? prefix)
  ;     prefix
  ;     (neoteric-process-tail port prefix)))

  ; Modify the main reader so that [] and {} are also delimiters, and so
  ; that when #\{ is detected, read using my-read-delimited-list
  ; any list from that port until its matching #\}, then process
  ; that list with "process-curly", like this:
  ;   (process-curly (my-read-delimited-list #\} port))



  ; ------------------------------------------------
  ; Demo procedures to implement curly-infix and neoteric readers
  ; ------------------------------------------------

  ; This implements an entire reader, as a demonstration, but if you can
  ; update your existing reader you should just update that instead.
  ; This is a simple R5RS reader, with a few minor (common) extensions.
  ; The "my-read" is called if it has to recurse.
  (define (underlying-read my-read port)
    (let ((c (peek-char port)))
      (if (or (eof-object? c)
	      (char=? c #\;)
	      (my-char-whitespace? c))
	  (underlying-read-real my-read port) ; the location will be recorded by the recursive call
	  (let* ((start (location-start port))
		 (datum (underlying-read-real my-read port)))
	    (record-form! datum (location-from start port))
	    datum))))

  (define (underlying-read-real my-read port)
    (let* ((c (peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\;)
          (consume-to-eol port)
          (my-read port))
        ((my-char-whitespace? c)
          (read-char port)
          (my-read port))
	
        ((char=? c #\( ) ; start parsing list
	 ;(set! dpth (+ 1 dpth))
	 (read-char port)
         (my-read-delimited-list my-read #\) port))
	
        ((char=? c #\[ )

	  ;;(default-scheme-read port)) ;; this convert [ ... ] in ($bracket-list$ ...) in Kawa at least allowing Kawa special expressions such as: [1 <: 7]

          (read-char port)
          (my-read-delimited-list my-read #\] port))
	

	((char=? c #\{ )
         (read-char port)

	 #;(process-curly
		   (my-read-delimited-list neoteric-read-real #\} port))
	 (let ((lyst (my-read-delimited-list neoteric-read-real #\} port)))
	   (if (and (null? lyst)
		    strict-pragma-in-curly) ; {BEGIN-STRICT-SRFI-105-REGION} : the pragma is given back to the enclosing list , which drops it
	       (let ((pragma strict-pragma-in-curly))
		 (set! strict-pragma-in-curly #f)
		 pragma)
	       (begin
		 (set! strict-pragma-in-curly #f)
		 #;(process-curly lyst)
		 (relink-symbols! (process-curly lyst) lyst)))));) ; the symbols keep their locations in the infix expression
	
        ; Handle missing (, [, { :
        ((char=? c #\) )
          (read-char port)
          (read-error "Closing parenthesis without opening")
          (my-read port))
        ((char=? c #\] )
          (read-char port)
          (read-error "Closing bracket without opening")
          (my-read port))
        ((char=? c #\} )
          (read-char port)
          (read-error "Closing brace without opening")
          (my-read port))
	
        ((char=? c #\") ; Strings are delimited by ", so can call directly
	 (default-scheme-read port))
	
	;; here we should 'push' as it is quoted or backquoted (pseudoquote,quasiquote)
        ((char=? c #\') ; quote
         (read-char port)
	 ;;(display "char=") (display #\') (newline)
	 (find*quote)
	 (let ((mrp (my-read port)))
	   (end-region) ; pop !
           (list 'quote mrp)))
	
        ((char=? c #\`) ; quasiquote
         (read-char port)
	 ;;(display "char=") (display #\`) (newline)
	 (find*quote)
	 (let ((mrp (my-read port)))
	   (end-region) ; pop !
           (list 'quasiquote mrp)))

	;; (quasiquote (,(sin 0.3) 3))
	;; datum=quasiquote
	;; char=,
	;; expression=`(,(sin 0.3) 3)


	;; `(,(sin 0.3) 3)
	;; '(0.29552020666133955 3)


	;; #<eof>
	;; (quasiquote (,'(sin 0.3) 3))
	;; datum=quasiquote
	;; char=,
	;; char='
	;; expression=`(,'(sin 0.3) 3)


	;; `(,'(sin 0.3) 3)
	;; '((sin 0.3) 3)


	;; here we should 'pop' as we have to eval with unquote or unquote-splicing
        ((char=? c #\,) ; unquote
         (read-char port)
	 ;;(display "char=") (display #\,)
	 (find-unquote*)
	 
         (cond
	  
              ((char=? #\@ (peek-char port)) ; splicing
               (read-char port)
	       ;;(display #\@) (newline)
	       (let ((mrp (my-read port)))
		 (end-region) ; pop !
		 (list 'unquote-splicing mrp)))
	      
              (#t
	       ;;(newline)
	       (let ((mrp (my-read port)))
		 (end-region) ; pop !
		 (list 'unquote mrp)))))


	
        ;; ((ismember? c digits) ; Initial digit.
	;;  (read-number port '()))
	
        ((char=? c #\#) ;(read-error "debug call to process-sharp:"
                               (process-sharp my-read port));)
        
        ((char=? c #\.) (process-period port)) ; note: could lead to a number too

	;; parse the numbers like 0.3 -0.3 -.3 ,the identifier like 3i and (i suppose) +3i which is a complex number not an identifier 
	((or (char=? c #\+) ; Initial + or -
	     (char=? c #\-)
	     (ismember? c digits)) ; Initial digit. (without + or - and not starting with . but could be an identifier starting with digits...)
          ;(read-char port)
	  (read-number-or-identifier-starting-with-digits port))
	
        ;; ((or (char=? c #\+) ; Initial + or -
	;;      (char=? c #\-))  
        ;;   (read-char port)
        ;;   (if (ismember? (peek-char port) digits)
	;;       ;;(let ((tmp
	;; 	     (read-number port (list c))
	;; 	;;     ))
	;; 	;; (newline (current-error-port))
	;; 	;; (display tmp (current-error-port))
	;; 	;; (newline (current-error-port))
	;; 	;; tmp)
	      
	;;       ;;(let ((tmp
	;; 	     (string->symbol
	;; 	      (fold-case-maybe port
	;; 			       (list->string (cons c
	;; 						   (read-until-delim port neoteric-delimiters)))))
	;; 	;;     )) ;end declarative let
	;; 	;; (newline (current-error-port))
	;; 	;; (display tmp (current-error-port))
	;; 	;; (newline (current-error-port))
	;; 	;; tmp)
	;; 	))

	;; ((ismember? c digits) ; Initial digit. (without + or - and not starting with . but could be an identifier starting with digits...)
	;;  (read-number-or-identifier-starting-with-digits port #;'()))
	   
	
        (#t ; Nothing else.  Must be a symbol start.
         (string->symbol
	  (fold-case-maybe port
			   (list->string
			    (read-until-delim port neoteric-delimiters))))))))




  (define (curly-infix-read-real port)
    (set! global-port port)
    (underlying-read curly-infix-read-real port))


  ;; this is the entry routine
  (define (curly-infix-read . port-src)
    ;;(set! dpth 0)
    (set! comment #f)
    #;(when (and (not (null? port-src))
	       (not (null? (cdr port-src)))
	       (path? (cadr port-src)))
      (set! source (some-system-path->string (cadr port-src))))
    (when (and (not (null? port-src))
	       (not (null? (cdr port-src)))
	       (cadr port-src))
      (set! source (cadr port-src))) ; as given by Racket (a path , or something else in an editor) : the tools compare it to their file
    (if (null? port-src)
      (curly-infix-read-real (current-input-port))
      (curly-infix-read-real (car port-src))))

  ; Here's a real neoteric reader.
  ; The key part is that it implements [] and {} as delimiters, and
  ; after it reads in some datum (the "prefix"), it calls
  ; neoteric-process-tail to see if there's a "tail".
  (define (neoteric-read-real port)
    #;(let* ((prefix (underlying-read neoteric-read-real port)))
      (if (eof-object? prefix)
        prefix
        (neoteric-process-tail port prefix)))
    (let* ((c (peek-char port))
	   (start (and (char? c) ; the location of the prefix f of f(x) , f[x] or f{x}
		       (not (char=? c #\;))
		       (not (my-char-whitespace? c))
		       (location-start port)))
	   (prefix (underlying-read neoteric-read-real port))
	   (prefix-location (location-from start port)))
      (if (eof-object? prefix)
        prefix
        (let ((result (neoteric-process-tail port prefix)))
	  (record-prefix! result prefix prefix-location)
	  result))))

  ;; the symbol of a neoteric form is in the pairs built by neoteric-process-tail : (f x) or ($bracket-apply$ f x)
  (define (record-prefix! result prefix location)
    (when (and (symbol? prefix) (pair? result))
      (cond ((eq? (car result) prefix)
	     (record-element! result location))
	    ((and (pair? (cdr result)) (eq? (cadr result) prefix))
	     (record-element! (cdr result) location)))))
  

  (define (neoteric-read . port)
    (if (null? port)
      (neoteric-read-real (current-input-port))
      (neoteric-read-real (car port))))


  ; ------------------
  ; Support procedures
  ; ------------------

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define linefeed (integer->char #x000A))        ; #\newline aka \n.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define tab (integer->char #x0009))
  (define line-tab (integer->char #x000b))
  (define form-feed (integer->char #x000c))
  (define line-ending-chars (list linefeed carriage-return))
  (define whitespace-chars
    (list tab linefeed line-tab form-feed carriage-return #\space))

  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; we always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define foldcase-default #f)

  ; Returns a true value (not necessarily #t) if char ends a line.
  (define (char-line-ending? char) (memq char line-ending-chars))

  ; Returns true if item is member of lyst, else false.
  (define (ismember? item lyst)
     (pair? (member item lyst)))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (ismember? c whitespace-chars)))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; foldcase-default configuration value when processing symbols.
  ; The "string-foldcase" procedure isn't everywhere,
  ; so we use "string-downcase".
  (define (fold-case-maybe port s)
    (if foldcase-default
      (string-downcase s)
      s))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (peek-char port)))
      (cond
        ((not (or (eof-object? c)
                  (char-line-ending? c)))
          (read-char port)
          (consume-to-eol port)))))

  (define (consume-whitespace port)
    (let ((char (peek-char port)))
      (cond
        ((eof-object? char) char)
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (read-char port)
          (consume-whitespace port)))))

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
  ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
  ; shows a strong vote AGAINST "#" being a delimiter.
  ; Having the "#" as a delimiter means that you cannot have "#" embedded
  ; in a symbol name, which hurts backwards compatibility, and it also
  ; breaks implementations like Chicken (has many such identifiers) and
  ; Gambit (which uses this as a namespace separator).
  ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
  ; (but consistent with R5RS, probably R7RS, and several implementations).
  ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
  (define neoteric-delimiters
    (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
		  #\" #\;)                 ; Could add #\# or #\|
	    whitespace-chars))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (peek-char port)))
      ;; (newline (current-error-port))
      ;; (display c (current-error-port))
      ;; (newline (current-error-port))
      (cond
         ((eof-object? c) '())
         ((ismember? c delims) '())
         (#t (cons (read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    ;; (display "Error: ")
    ;; (display message)
    ;; (display "\n")
    (define error-message (string-append "Error: " message))
    (define-values (line col pos) (port-next-location global-port))
    (raise-read-error error-message	 	 	 	 
 	 	      source	 	 
 	 	      line	 	 	 	 
 	 	      col	 	 	 	 
 	 	      pos	 	 	 	 
 	 	      #f)
    ;;(error error-message)
    ;;'()
    )

(define (read-number port starting-lyst)
  ;;(newline (current-error-port))
  ;;(display starting-lyst (current-error-port)) (newline (current-error-port))

  ;;(let ((tmp 
	 (string->number (list->string
			  (append starting-lyst
				  (read-until-delim port neoteric-delimiters))))   ;;)) ; end declarative let
    ;; (newline (current-error-port))
    ;; (display tmp (current-error-port))
    ;; (newline (current-error-port))
    ;; tmp)
	 )

;; added by D.MATTEI
(define (read-number-or-identifier-starting-with-digits port #;starting-lyst)
  ;;(newline (current-error-port))
  ;;(display starting-lyst (current-error-port)) (newline (current-error-port))

  (let* ((str-number-or-identifier (list->string
				    ;(append starting-lyst
					    (read-until-delim port neoteric-delimiters)));)
	 (number (string->number str-number-or-identifier))) ; end declarative let
    (if number ; string->number return #f if it was not possible to convert it in a number
	number
	(string->symbol str-number-or-identifier))))


  ; detect #| or |#
  (define (nest-comment port)
    (let ((c (read-char port)))
      (cond
        ((eof-object? c))
        ((char=? c #\|)
          (let ((c2 (peek-char port)))
            (if (char=? c2 #\#)
                (read-char port)
                (nest-comment port))))
        ((char=? c #\#)
          (let ((c2 (peek-char port)))
            (when (char=? c2 #\|)
                (begin
                  (read-char port)
                  (nest-comment port)))
            (nest-comment port)))
        (#t
          (nest-comment port)))))

  ; (string->value "#hash((a . 1) (b . 2))")
  ; '#hash((a . 1) (b . 2))
  ; courtesy of ChatGPT:
  (define (string->value s)
    (read (open-input-string s)))

  
  (define (process-sharp my-read port)
    ; We've peeked a # character.  Returns what it represents.
    (read-char port) ; Remove #
    (cond
      ((eof-object? (peek-char port)) (peek-char port)) ; If eof, return eof.
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port)))
          (cond
            ((char-ci=? c #\t)  #t)
            ((char-ci=? c #\f)  #f)
	    ;; should be for parsing binary,octal,hexadecimal,etc numbers
            ((ismember? c '(#\i #\e #\b #\o #\d #\x
                            #\I #\E #\B #\O #\D #\X))
              (read-number port (list #\# (char-downcase c))))
            ((char=? c #\( )  ; Vector.
	     (list->vector (my-read-delimited-list my-read #\) port)))

	    ;; hash table : #hash(("a" . 1) ("b" . 20)) support written for racket scheme
	    ;; Racket's regular expressions special syntax
            ; and other hash table types :hasheq,...
	    ((char=? c #\h) (if (not (and (char=? (read-char port) #\a)
					  (char=? (read-char port) #\s)
					  (char=? (read-char port) #\h)))
                                (read-error "process-sharp : awaiting hash : token not found")
                                (cond ((char=? (peek-char port) #\() ; look at the char without getting it from port
                                       (read-char port) ; really get the char
                                       ; recreate '#hash(list ...) ,example : '#hash((a . 1) (b . 2))
                                       (string->value (string-append "#hash"
                                                                     (format "~a" (my-read-delimited-list my-read #\) port))))) ; write the list in a string
                                      ((and (char=? (read-char port) #\e) ; really get the char
                                            (char=? (read-char port) #\q)) ; here we got #hasheq
                                       (let ((cht (read-char port))) ; should be v or (
                                         (cond ((char=? cht #\() ; here we got #hasheq(
                                                ; recreate '#hasheq(list ...) ,example : '#hasheq((a . 1) (b . 2))
                                                (string->value (string-append "#hasheq"
                                                                              (format "~a" (my-read-delimited-list my-read #\) port))))) ; write the list in a string
                                               ((char=? cht #\v) ; here we got #hasheqv
                                                (if (char=? (read-char port) #\()
                                                    ; recreate '#hasheqv(list ...) ,example : '#hasheqv((a . 1) (b . 2))
                                                    (string->value (string-append "#hasheqv"
                                                                                  (format "~a" (my-read-delimited-list my-read #\) port)))) ; write the list in a string
                                                    (read-error "process-sharp : awaiting hasheqv( : token not found")))
                                               (else
                                                (read-error "process-sharp : awaiting hasheq( or hasheqv( : token not found")))))
                                      (else
                                       (read-error "process-sharp : awaiting hasheq : partial token not found")))))
                                     
	    ((char=? c #\\) (process-char port))
	    
            ; This supports SRFI-30 #|...|#
            ; #| ... |# is pure "atmosphere" (like a ; comment or white
            ; space), it is NOT a datum comment (that is #; , handled
            ; below). After skipping it, we used to unconditionally call
            ; (my-read port) again to fetch "the real next datum" and
            ; return it directly. That breaks when the block comment is
            ; the last thing before the enclosing list's closing
            ; delimiter (or before EOF at top level): (my-read port)
            ; then tries to read a whole new datum, peeks the closing
            ; #\) #\] or #\} , and underlying-read raises "Closing
            ; parenthesis/bracket/brace without opening" even though
            ; that delimiter is perfectly legitimate one level up.
            ; Fix: after the comment, peek (without consuming) past any
            ; remaining white space / ; comments. If what follows is
            ; EOF or a closing delimiter, there is no next datum to read
            ; here: mark this as a dropped datum, exactly like #; does,
            ; so the caller (my-read-delimited-list) safely loops and
            ; notices the stop-char or EOF itself instead of us trying
            ; (and failing) to read past it.
            ((char=? c #\|)
              (nest-comment port)
              (consume-whitespace port)
              (let ((c2 (peek-char port)))
                (cond
                  ((eof-object? c2) c2) ; nothing follows: let EOF propagate, as before
                  ((or (char=? c2 #\))
                       (char=? c2 #\])
                       (char=? c2 #\}))
                    (set! comment #t)
                    (void)) ; dropped, like #; ; caller notices the delimiter itself
                  (#t (my-read port)))))
	    
            ; If #!xyz, consume xyz and recurse.
            ; In a real reader, consider handling "#! whitespace" per SRFI-22,
            ; and consider "#!" followed by / or . as a comment until "!#".
            ((char=? c #\!) (my-read port) (my-read port))

            ;; commented nested scheme expressions
	    ;; (+ 1 #;2 3)
	    ;; 4
	    ;; (+ 1 #;(+ 1 2) 3)
	    ;; 4
	    ;; (+ 1 #;{1 + 2} 3)
	    ;; 4
            ;; (cons 1 2 #;(foo bar))
            ;; (cons 1 2)
            ;;'(1 . 2)
	    #;((char=? c #\;) ;(read-error "SRFI-105 REPL : Unsupported #; extension"))
             (let ((comy (my-read port))) ; commented expression
               (set! comment #t)
               comy) ; must be returned but will be dropped later
             ;(read-error "debug #; "
                     ;(my-read port)
             );)
            ; #;#; a b comments two datums (as in Racket) : when the datum read is itself commented by a #; (the flag
            ; comment is set) , it is dropped and this #; comments the next datum
	    ((char=? c #\;)
             (let loop ()
               (set! comment #f)
               (let ((comy (my-read port))) ; commented expression
                 (if comment
                     (loop) ; the datum read was #;datum : read the datum that this #; comments
                     (begin
                       (set! comment #t)
                       comy))))) ; must be returned but will be dropped later

            
	    ;; this remove #lang racket on anything else but i do not want it to be like that
	    ;; removing any line starting with #l is not the good way
	    ;; i should instead skip the first #lang ... line of the input file
	    ;; ((char=? c #\l) ;; #lang ...
	    ;;  (consume-to-eol port)
	    ;;  (my-read port))
	    
	    ;; read #:blabla
	    ((char=? c #\:) (string->keyword ;; also add #: in front of argument
			     (list->string
			      ;;(append (list #\# #\:)
				      (read-until-delim port neoteric-delimiters))));;)

	    ;; Racket's regular expressions special syntax
	    ((char=? c #\r) (if (not (char=? (read-char port) #\x))
				(read-error "process-sharp : awaiting regexp : character x not found")
				(let ((str (my-read port)))
				  (if (not (string? str))
				      (read-error "process-sharp : awaiting regexp : string not found")
				      (list 'regexp str)))))

	    ((char=? c #\p) (if (not (char=? (read-char port) #\x))
				(read-error "process-sharp : awaiting regexp : character x not found")
				(let ((str (my-read port)))
				  (if (not (string? str))
				      (read-error "process-sharp : awaiting pregexp : string not found")
				      (list 'pregexp str)))))
	    
	    ;; read #'blabla ,deal with syntax objects
	    ((char=? c #\') (list 'syntax (my-read port)))
	    ;; deal syntax with backquote, splicing,...
	    ((char=? c #\`) (list 'quasisyntax (my-read port)))
	    ((char=? c #\,) (if (char=? (peek-char port) #\@)
				(begin
				  (read-char port)
				  (list 'unsyntax-splicing (my-read port)))
				(list 'unsyntax (my-read port))))
	    
	    (#t (read-error (string-append "SRFI-105-curly-infix.rkt , SRFI-105 REPL :"
					   "Unsupported # extension"
					   " unsupported character causing this message is character:"
					   (string c)))))))))



  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (read-char port) ; Remove .
    (let ((c (peek-char port)))
      (cond ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
       ((eof-object? c) (string->symbol (string #\.)))  ;; only this one works with Racket Scheme
        ;;((eof-object? c) '.) ; period eof; return period. ;; do not works with Racket Scheme
        ((ismember? c digits)  ; in case it wasn't a single . but the starting of a number
         ;;(read-number port (list #\.)))  ; period digit - it's a number.
	 (let ((num (read-number port (list #\.))))
	   ;;.3f
	   ;;. REPL-Scheme-PLUS.rkt:36:5: Error: period digit must be a number
            (if num
                num
                (read-error "period digit must be a number"))))
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))



  
  ;; characters support
  
  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (peek-char port)) (peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (string-downcase (list->string (cons c rest)))))
                (cond
                  ; Implement R6RS character names, see R6RS section 4.2.6.
                  ; As an extension, we will ALWAYS accept character names
                  ; of any case, no matter what the case-folding value is.
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "tab") tab)
                  ((string-ci=? rest-string "nul") (integer->char #x0000))
                  ((string-ci=? rest-string "alarm") (integer->char #x0007))
                  ((string-ci=? rest-string "backspace") (integer->char #x0008))
                  ((string-ci=? rest-string "linefeed") (integer->char #x000A))
                  ((string-ci=? rest-string "vtab") (integer->char #x000B))
                  ((string-ci=? rest-string "page") (integer->char #x000C))
                  ((string-ci=? rest-string "return") (integer->char #x000D))
                  ((string-ci=? rest-string "esc") (integer->char #x001B)) ; why not return #\esc ?
		  ((string-ci=? rest-string "escape") (integer->char #x001B)) ; R7RS
                  ((string-ci=? rest-string "delete") (integer->char #x007F))
                  ; Additional character names as extensions:
                  ((string-ci=? rest-string "ht") tab)
                  ((string-ci=? rest-string "cr") (integer->char #x000d))
                  ((string-ci=? rest-string "bs") (integer->char #x0008))

		  ;; u: unicode ? char with code number in hexadecimal , example #\u1b (27 in decimal -> escape)
		  ((char-ci=? c #;(string-ref rest-string 0) #\u) 
		   (integer->char (string->number (substring rest-string 1) 16))) ; consider it was in hexadecimal
		   
                  (#t
		   (read-error "Invalid character name"))))))))))

;; Record the original read location, in case it's changed later:
(define default-scheme-read read)



(export
	curly-infix-read
	read-was-a-datum-comment?
	care-of-quote
	srfi-strict
	use-only-syntax-transformers
	get-annot-flag
	set-annot-flag!
	get-annot-on-read
	set-annot-on-read!
	process-annotation-pragma
	process-strict-pragma
	reset-srfi-strict!)
 ; end module
