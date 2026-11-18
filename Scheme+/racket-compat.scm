;; racket-compat.scm

;; Guile Scheme version

;; The modules of Scheme+ for Guile are translated from Scheme+ for Racket. This module gives them the few forms and
;; procedures of racket/base they use and that Guile does not have , or has with another meaning :
;;
;; - when , unless , cond , case and do accept definitions in their bodies (internal definitions) , as in Racket ;
;; - the procedures of racket/base missing in Guile (add1 , sub1 , void , rest , empty? , displayln , ...) ;
;; - format , printf with the Racket meaning (format returns a string) ;
;; - hash-ref with a default that can be a procedure (called when the key is missing) , as in Racket ;
;; - raise-syntax-error (syntax-violation in Guile) ;
;; - the flonum and fixnum operations with their Racket names (fl< , fx< , ...).

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


(define-module (Scheme+ racket-compat)

  #:use-module ((srfi srfi-1) #:select (first second third fourth fifth last))
  #:use-module ((system syntax) #:select (syntax?))
  #:use-module ((ice-9 format) #:select ((format . format-ice-9)))
  #:use-module ((ice-9 regex) #:select (match:substring match:count))
  #:use-module ((srfi srfi-1) #:select (every any fold))
  #:use-module ((ice-9 exceptions) #:select (exception-with-message? exception-message))
  #:use-module ((srfi srfi-11) #:select (let-values let*-values))
  #:use-module ((rnrs bytevectors) #:select (bytevector?))
  #:use-module ((rnrs arithmetic flonums) #:prefix r6rs:)
  #:use-module ((rnrs arithmetic fixnums) #:prefix r6rs:)

  ;; forms and procedures of Guile replaced by their Racket meaning
  #:replace (when unless cond case do with-syntax
	     format hash-ref
	     array-ref array-set!)

  #:re-export (first second third fourth fifth last
	       syntax?
	       let-values let*-values)

  #:export (add1 sub1 void rest empty empty? displayln printf eprintf
	    make-hash hash-keys hash-has-key?
	    raise-syntax-error syntax-e syntax->list
	    andmap ormap hash? bytes?
	    arithmetic-shift bitwise-and bitwise-ior bitwise-xor bitwise-not
	    string->keyword port-next-location raise-read-error raise-read-eof-error
	    with-handlers exn:fail? exn-message foldl for/fold list* fprintf void? open-output-nowhere
	    regexp regexp-match
	    fl+ fl- fl* fl/ fl< fl> fl<= fl>= fl= flabs flsqrt flexp fllog flsin flcos fltan flatan flexpt flmin flmax
	    flfloor flceiling flround fltruncate
	    flonum? ->fl fx->fl
	    fx+ fx- fx* fxquotient fxremainder fxmodulo fxnot fx< fx> fx<= fx>= fx= fxabs fxmin fxmax
	    fixnum?))


;; ---------- forms with internal definitions in their bodies

(define-syntax when
  (syntax-rules ()
    ((_ test body ...) (if test (let () body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test body ...) (if test #f (let () body ...)))))

(define-syntax cond
  (syntax-rules (else =>)
    ((_) (if #f #f))
    ((_ (else body ...)) (let () body ...))
    ((_ (test => receiver) clause ...)
     (let ((t test)) (if t (receiver t) (cond clause ...))))
    ((_ (test) clause ...)
     (or test (cond clause ...)))
    ((_ (test body ...) clause ...)
     (if test (let () body ...) (cond clause ...)))))

(define-syntax case
  (syntax-rules (else =>)
    ((_ key clause ...)
     (let ((k key)) (case-key k clause ...)))))

(define-syntax case-key
  (syntax-rules (else =>)
    ((_ k) (if #f #f))
    ((_ k (else => receiver)) (receiver k))
    ((_ k (else body ...)) (let () body ...))
    ((_ k ((datum ...) => receiver) clause ...)
     (if (memv k '(datum ...)) (receiver k) (case-key k clause ...)))
    ((_ k ((datum ...) body ...) clause ...)
     (if (memv k '(datum ...)) (let () body ...) (case-key k clause ...)))))

(define-syntax do
  (syntax-rules ()
    ((_ ((var init step ...) ...) (test expr ...) command ...)
     (let loop ((var init) ...)
       (if test
	   (let () (if #f #f) expr ...)
	   (begin
	     (let () (if #f #f) command ...)
	     (loop (do-step var step ...) ...)))))))

(define-syntax do-step
  (syntax-rules ()
    ((_ var) var)
    ((_ var step) step)))

;; with-syntax of Racket : a value that is not a syntax object is converted with datum->syntax , in the lexical
;; context of the with-syntax form (the Racket documentation) ; the with-syntax of Guile keeps it as a raw datum ,
;; and a raw symbol in the output of a macro is an error ("encountered raw symbol in macro output")
;; Only the raw symbols are converted : the syntax objects inside the value are kept as they are (as datum->syntax of
;; Racket does) , and the lists stay lists (a template #'(...) of Guile gives a list of syntax objects).
(define (->syntax-like-racket ctx v)
  (let convert ((v v))
    (cond ((syntax? v) v)
	  ((symbol? v) (datum->syntax ctx v))
	  ((pair? v) (cons (convert (car v)) (convert (cdr v))))
	  ((vector? v) (list->vector (map convert (vector->list v))))
	  (else v))))

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      ((k ((p e) ...) b1 b2 ...)
       ;; as the with-syntax of Guile (psyntax) , with the conversion of the values
       #'(syntax-case (list (->syntax-like-racket #'k e) ...) ()
	   ((p ...) (let () b1 b2 ...)))))))


;; ---------- the arrays of SRFI 25 (used by Scheme+ for Racket) on the arrays of Guile : the indexes can be given in a
;; vector , and array-set! takes the value after the indexes ((array-set! a i j v) , Guile : (array-set! a v i j))

(define (array-ref a . indexes)
  (if (and (= (length indexes) 1) (vector? (car indexes)))
      (apply (@ (guile) array-ref) a (vector->list (car indexes)))
      (apply (@ (guile) array-ref) a indexes)))

(define (array-set! a . indexes-and-value)
  (let* ((rev (reverse indexes-and-value))
	 (value (car rev))
	 (indexes (reverse (cdr rev))))
    (if (and (= (length indexes) 1) (vector? (car indexes)))
	(apply (@ (guile) array-set!) a value (vector->list (car indexes)))
	(apply (@ (guile) array-set!) a value indexes))))


;; ---------- procedures of racket/base

(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))
(define (void . args) (if #f #f))
(define (rest lst) (cdr lst))
(define empty '())
(define (empty? x) (null? x))
(define (displayln x . port) (apply display x port) (apply newline port))

;; Racket format : (format form v ...) returns a string
(define (format form . args) (apply format-ice-9 #f form args))
(define (printf form . args) (display (apply format form args)))
(define (eprintf form . args) (display (apply format form args) (current-error-port)))


;; hash tables : make-hash is an equal?-based mutable table as in Racket ; the default of hash-ref is called when
;; it is a procedure
(define (make-hash . assocs)
  (define h (make-hash-table))
  (when (pair? assocs)
    (for-each (lambda (p) (hash-set! h (car p) (cdr p))) (car assocs)))
  h)


(define (hash-ref h key . default)
  (define v ((@ (guile) hash-get-handle) h key))
  (cond (v (cdr v))
	((null? default) (error "hash-ref: no value found for key" key))
	((procedure? (car default)) ((car default)))
	(else (car default))))

(define (hash-keys h) (hash-map->list (lambda (k v) k) h))
(define (hash-has-key? h key) (if ((@ (guile) hash-get-handle) h key) #t #f))


;; exceptions : (with-handlers ((predicate handler) ...) body ...) of Racket
(define-syntax with-handlers
  (syntax-rules ()
    ((_ ((pred handler) ...) body ...)
     (call-with-handlers (list (cons pred handler) ...) (lambda () body ...)))))

(define (call-with-handlers handlers thunk)
  (with-exception-handler
   (lambda (e)
     (let loop ((hs handlers))
       (cond ((null? hs) (raise-exception e))
	     (((caar hs) e) ((cdar hs) e))
	     (else (loop (cdr hs))))))
   thunk
   #:unwind? #t))

;; every error of Guile is an exn:fail of Racket
(define (exn:fail? e) #t)
(define (exn-message e)
  (if (exception-with-message? e) (exception-message e) (format "~a" e)))

;; lists
(define foldl fold) ; (foldl proc init lst) , proc receives the element then the accumulator , as fold of SRFI 1
(define list* cons*)

;; (for/fold ((acc init)) ((e lst)) body ...) : the only form of for/fold used , one accumulator and one list
(define-syntax for/fold
  (syntax-rules ()
    ((_ ((acc init)) ((e lst)) body ...)
     (fold (lambda (e acc) body ...) init lst))))

;; output
(define (fprintf port form . args) (display (apply format form args) port))
(define (open-output-nowhere) (%make-void-port "w"))
(define (void? x) (unspecified? x))

;; keywords
(define (string->keyword s) (symbol->keyword (string->symbol s)))

;; ports : (port-next-location port) gives the line (from 1) , the column (from 0) and the position (from 1) as in
;; Racket ; Guile does not count the position : #f
(define (port-next-location port)
  (values (+ 1 (port-line port)) (port-column port) #f))

;; errors of the reader (syntax/readerr of Racket)
(define (raise-read-error message source line column position span . rest)
  (error (string-append (if source (format "~a:~a:~a: " source line column) "") message)))
(define (raise-read-eof-error message source line column position span)
  (error (string-append (if source (format "~a:~a:~a: " source line column) "") message)))

;; bitwise operations
(define arithmetic-shift ash)
(define bitwise-and logand)
(define bitwise-ior logior)
(define bitwise-xor logxor)
(define bitwise-not lognot)

;; type predicates
(define (hash? x) (hash-table? x))
(define (bytes? x) (bytevector? x))

;; andmap and ormap of Racket : every and any of SRFI 1
(define andmap every)
(define ormap any)

;; regular expressions : (regexp-match (regexp str) s) gives the list of the matched strings or #f
(define (regexp str) (make-regexp str))
(define (regexp-match rx s)
  (let ((m (regexp-exec (if (string? rx) (make-regexp rx) rx) s)))
    (and m
	 (let loop ((i (- (match:count m) 1)) (acc '()))
	   (if (< i 0) acc (loop (- i 1) (cons (match:substring m i) acc)))))))

;; syntax-e : the datum of an identifier or of a literal (the parser uses it only on them)
(define (syntax-e s) (if (syntax? s) (syntax->datum s) s))

;; syntax->list : the list of the syntax objects of a syntax list , #f when it is not a list
(define (syntax->list s)
  (if (syntax? s)
      (syntax-case s () ((x ...) #'(x ...)) (_ #f))
      (and (list? s) s)))

;; (raise-syntax-error name message [expr [sub-expr]])
(define (raise-syntax-error name message . exprs)
  (syntax-violation name message
		    (if (pair? exprs) (car exprs) #f)
		    (if (and (pair? exprs) (pair? (cdr exprs))) (cadr exprs) #f)))


;; ---------- flonums and fixnums with the names of racket/flonum and racket/fixnum

(define fl+ r6rs:fl+)
(define fl- r6rs:fl-)
(define fl* r6rs:fl*)
(define fl/ r6rs:fl/)
(define fl< r6rs:fl<?)
(define fl> r6rs:fl>?)
(define fl<= r6rs:fl<=?)
(define fl>= r6rs:fl>=?)
(define fl= r6rs:fl=?)
(define flabs r6rs:flabs)
(define flsqrt r6rs:flsqrt)
(define flexp r6rs:flexp)
(define fllog r6rs:fllog)
(define flsin r6rs:flsin)
(define flcos r6rs:flcos)
(define fltan r6rs:fltan)
(define flatan r6rs:flatan)
(define flfloor r6rs:flfloor)
(define flceiling r6rs:flceiling)
(define flround r6rs:flround)
(define fltruncate r6rs:fltruncate)
(define flexpt r6rs:flexpt)
(define flmin r6rs:flmin)
(define flmax r6rs:flmax)
(define flonum? r6rs:flonum?)
(define (->fl x) (exact->inexact x))
(define (fx->fl x) (exact->inexact x))

;; the fixnum operations are the generic ones of Guile : the operations of (rnrs arithmetic fixnums) check their
;; arguments and are not compiled inline , + - * < ... of Guile are (measured 2026-11-18 : about 5 % of the time of the
;; back propagation program in fx+ / fx- , used by the square brackets for the indexes). A macro gives the operation of
;; Guile itself , that the compiler inlines (fx+ alone is the procedure +).
(define-syntax fx+ (identifier-syntax +))
(define-syntax fx- (identifier-syntax -))
(define-syntax fx* (identifier-syntax *))
(define fxnot r6rs:fxnot)
(define-syntax fxquotient (identifier-syntax quotient))
(define-syntax fxremainder (identifier-syntax remainder))
(define-syntax fxmodulo (identifier-syntax modulo))
(define-syntax fx< (identifier-syntax <))
(define-syntax fx> (identifier-syntax >))
(define-syntax fx<= (identifier-syntax <=))
(define-syntax fx>= (identifier-syntax >=))
(define-syntax fx= (identifier-syntax =))
(define-syntax fxabs (identifier-syntax abs))
(define-syntax fxmin (identifier-syntax min))
(define-syntax fxmax (identifier-syntax max))
(define fixnum? r6rs:fixnum?)
