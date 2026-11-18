;; parser-common.scm

;; What the two parsers of Scheme+ for Guile share : curly-infix2prefix4guile.scm (SRFI 105 , curly infix) and
;; sweet-infix2prefix4guile.scm (SRFI 110 , sweet-expressions) : the type annotation , the generic arithmetic of Guile
;; in the annotated code , the check of the assignments , the header of the file.

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


(define-module (SRFI-105 parser-common)
  #:use-module (ice-9 regex)
  #:use-module ((SRFI-105 annot+) #:select (annot init-annot-data
					     remove-types-and-annotation-markers
					     insert-top-level-markers-in-module))
  #:export (annotate-or-remove-types
	    check-assignments
	    skip-header
	    count-lines))


(define stderr (current-error-port))
(define stdout (current-output-port))

;; the tables of the type annotation , created by init-annot-data
(define ovrld-ht #f)
(define ovrld-ht-n-arity #f)
(define ovrld-ht-proc #f)
(define annot-ht-lst #f)


;; Guile : the flonum and fixnum operations written by the type annotation (fl+ , fx* , fl</fx ...) are replaced by the
;; generic operations of Guile. In Guile fl+ , fx+ ... ((rnrs arithmetic flonums) , (rnrs arithmetic fixnums)) are
;; procedures that check their arguments , not compiled inline : the code annotated with them was 5 to 11 times
;; slower than the same code with the generic operations , that the compiler of Guile compiles inline (measured on
;; the benchmarks of the manual , 2026-11-17). The other rewritings of the annotation (square brackets , overloads ,
;; assignments of vectors , procedures typed by the context) are kept : they are faster in Guile too.
;; orig-+ ... are the original operations (Scheme+ exports them) : an overloaded + of the program is not called.
(define generic-arithmetic
  '((fl+ . orig-+) (fl- . orig--) (fl* . orig-*) (fl/ . orig-/)
    (fl< . orig-<) (fl> . orig->) (fl<= . orig-<=) (fl>= . orig->=) (fl= . orig-=) (fl≠ . orig-≠)
    (fx+ . orig-+) (fx- . orig--) (fx* . orig-*)
    (fx< . orig-<) (fx> . orig->) (fx<= . orig-<=) (fx>= . orig->=) (fx= . orig-=) (fx≠ . orig-≠)
    (flabs . abs) (fxabs . abs) (flsqrt . sqrt) (flexp . exp) (fllog . log) (flsin . sin) (flcos . cos)
    (fltan . tan) (flatan . atan) (flexpt . orig-expt) (flmin . min) (flmax . max) (fxmin . min) (fxmax . max)
    (flfloor . floor) (flceiling . ceiling) (flround . round) (fltruncate . truncate)
    (fxquotient . quotient) (fxremainder . remainder) (fxmodulo . modulo) (fxnot . lognot)
    (fx->fl . exact->inexact) (->fl . exact->inexact)
    (fl+/fx . orig-+) (fx/fl+ . orig-+) (fl-/fx . orig--) (fx/fl- . orig--) (fl*/fx . orig-*) (fx/fl* . orig-*)
    (fl//fx . orig-/) (fx/fl/ . orig-/) (flmin/fx . min) (fx/flmin . min)
    (fl</fx . orig-<) (fx/fl< . orig-<) (fl>/fx . orig->) (fx/fl> . orig->) (fl<=/fx . orig-<=) (fx/fl<= . orig-<=)
    (fl>=/fx . orig->=) (fx/fl>= . orig->=) (fl=/fx . orig-=) (fx/fl= . orig-=) (fl≠/fx . orig-≠) (fx/fl≠ . orig-≠)))

(define (generic-arithmetic-of x)
  (cond ((symbol? x) (let ((p (assq x generic-arithmetic))) (if p (cdr p) x)))
	((and (pair? x) (memq (car x) '(quote quasiquote syntax quasisyntax))) x)
	((pair? x) (cons (generic-arithmetic-of (car x)) (generic-arithmetic-of (cdr x))))
	(else x)))


;; the type annotation when a pragma ANNOTATION-ON has been read in the file (annot-on-read? : get-annot-on-read of the
;; reader) , otherwise only the removal of the type forms and of the markers of the pragmas
(define (annotate-or-remove-types forms-read annot-on-read?)
  (define forms (insert-top-level-markers-in-module forms-read))
  (cond ((null? forms) '()) ; only pragmas , no code
	(annot-on-read?
	 (let* ((module-form? (and (null? (cdr forms)) ; the file is a single (module ...) form
				   (list? (car forms))
				   (not (null? (car forms)))
				   (eq? 'module (car (car forms)))))
		(modul (if module-form?
			   (car forms)
			   `(module aschemeplusprogram guile ,@forms)))) ; the annotation parser gets a module
	   (init-annot-data ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst)
	   (let ((annotated (generic-arithmetic-of
			     (annot modul ovrld-ht ovrld-ht-n-arity ovrld-ht-proc annot-ht-lst))))
	     (if module-form?
		 (list annotated)
		 (cdddr annotated))))) ; remove the envelope : keep the forms of the file
	(else
	 (remove-types-and-annotation-markers forms))))


;; In Guile <- and := only assign (in Racket they define a name not yet bound) : an assignment to a name that is
;; defined nowhere in the program (nor in Guile , nor in Scheme+) is reported on stderr , the definition is written
;; with <+ (or define , declare). The test is made on the whole file : a name bound anywhere in it (definition ,
;; parameter , variable of a let or a do ...) is accepted , and a variable of an imported module of the program can not be
;; known : it is a warning , the conversion is made.

(define assignment-operators-first '(<- ← := :=> <v ⇜)) ; (<- target ... expr)
(define assignment-operators-last '(-> → =: v> ⇝)) ; (-> expr target ...)

(define (formals->names f) ; the names of formal parameters , with #:optional #:key #:rest and (name default)
  (cond ((symbol? f) (list f))
	((pair? f) (append (formals->names (car f)) (formals->names (cdr f))))
	(else '())))

(define (bound-names forms)
  (define names (make-hash-table))
  (define (add! l) (for-each (lambda (n) (when (symbol? n) (hash-set! names n #t))) l))
  (define (bindings->names bs) ; ((x e) ...) or (((a b) e) ...)
    (if (list? bs)
	(apply append (map (lambda (b) (if (pair? b) (formals->names (car b)) (formals->names b))) bs))
	'()))
  (let walk ((x forms))
    (when (pair? x)
      (let ((head (car x)) (args (cdr x)))
	(when (and (symbol? head) (pair? args))
	  (case head
	    ((define define* define-values define-syntax define-public define-macro def defvar <+ ⥆ :+ declare
	      define-syntax-rule define-record-type define-inline)
	     (add! (formals->names (if (memq head '(declare)) args (car args)))))
	    ((+> +: ⥅) (add! (formals->names (cdr args))))
	    ((lambda lambda* λ lambda+ case-lambda)
	     (if (eq? head 'case-lambda)
		 (for-each (lambda (c) (when (pair? c) (add! (formals->names (car c))))) args)
		 (add! (formals->names (car args)))))
	    ((let let* letrec letrec* let-values let*-values letrec-values do)
	     (if (symbol? (car args)) ; named let
		 (begin (add! (list (car args)))
			(when (pair? (cdr args)) (add! (bindings->names (cadr args)))))
		 (add! (bindings->names (car args)))))
	    (else #f))))
      (walk (car x))
      (walk (cdr x))))
  names)

(define (known-binding? name)
  (or (module-variable (resolve-module '(guile)) name)
      (module-variable (resolve-interface '(Scheme+)) name)))

;; parser : the name of the parser , for the messages
(define (check-assignments forms parser)
  (define names (bound-names forms))
  (define reported (make-hash-table))
  (define (check! target form)
    (for-each (lambda (n)
		(when (and (symbol? n)
			   (not (hash-ref names n #f))
			   (not (known-binding? n))
			   (not (hash-ref reported n #f)))
		  (hash-set! reported n #t)
		  (display parser stderr)
		  (display " : warning : " stderr)
		  (write n stderr)
		  (display " is assigned but defined nowhere in the program , in : " stderr)
		  (write form stderr) (newline stderr)
		  (display "  (in Guile <- and := only assign : define it with <+ , define or declare)" stderr)
		  (newline stderr)))
	      (cond ((symbol? target) (list target))
		    ((and (list? target) (pair? target) (not (eq? (car target) '$bracket-apply$))
			  (and-map symbol? target))
		     target) ; (a b ...) of values
		    (else '()))))
  (let walk ((x forms))
    (when (pair? x)
      (when (and (symbol? (car x)) (list? x) (>= (length x) 3))
	(cond ((memq (car x) assignment-operators-first)
	       (for-each (lambda (t) (check! t x)) (reverse (cdr (reverse (cdr x))))))
	      ((memq (car x) assignment-operators-last)
	       (for-each (lambda (t) (check! t x)) (cddr x)))))
      (unless (memq (car x) '(quote quasiquote syntax quasisyntax))
	(walk (car x))
	(walk (cdr x))))))


;; the header of the file : spaces and comments , a shebang (#!...guile... !#) kept in the output , the directives
;; #!curly-infix , #lang reader SRFI-105 (or SRFI-110) , #reader SRFI-105 (or SRFI-110) (a file written for Racket)
;; skipped (as by curly-infix2prefix4racket.rkt of SRFI 105 and of SRFI 110 : at the beginning of the file ,
;; #!curly-infix does not change the reading of the sweet-expressions) , #!r6rs.
;; Returns two values : the text after the header , and #t when the directive #!r6rs has been read.
(define (skip-header text)
  (define header-line
    (make-regexp (string-append "^([[:space:]]+|;[^\n]*\n"
				"|#!curly-infix[[:blank:]]*\n"
				"|#lang reader SRFI-1(05|10)[^\n]*\n"
				"|#reader SRFI-1(05|10)[^\n]*\n)")))
  (define r6rs (make-regexp "^#!r6rs[[:blank:]]*\n"))
  (define racket-shebang (make-regexp "^#![^\n]*racket[^\n]*\n"))
  (define directive (make-regexp "^#!(curly-infix|sweet|no-sweet|fold-case|no-fold-case)[[:blank:]\n;]"))
  (let loop ((t text) (r6rs? #f))
    (cond ((regexp-exec header-line t)
	   => (lambda (m) (loop (substring t (match:end m 0)) r6rs?)))
	  ((regexp-exec racket-shebang t) ; a file written for Racket : the line is kept as a comment (Guile would read #!
	   => (lambda (m)                 ; as the beginning of a block comment closed by !#)
		(display ";; " stdout)
		(display (match:substring m 0) stdout)
		(loop (substring t (match:end m 0)) r6rs?)))
	  ((and (string-prefix? "#!" t)
		(not (regexp-exec directive t))
		(not (string-prefix? "#!r6rs" t))
		(string-contains t "\n!#"))
	   => (lambda (i) ; the shebang of a Guile script , #! ... !# , is kept
		(let* ((close (+ i 3))
		       (end (or (string-index t #\newline close) (- (string-length t) 1))))
		  (display (substring t 0 (+ end 1)) stdout)
		  (loop (substring t (+ end 1)) r6rs?))))
	  ((regexp-exec r6rs t)
	   => (lambda (m)
		(display "Detected R6RS code: #!r6rs" stderr) (newline stderr) (newline stderr)
		(loop (substring t (match:end m 0)) #t)))
	  (else (values t r6rs?)))))


(define (count-lines s)
  (let loop ((i 0) (n 0))
    (cond ((= i (string-length s)) n)
	  ((char=? (string-ref s i) #\newline) (loop (+ i 1) (+ n 1)))
	  (else (loop (+ i 1) n)))))
