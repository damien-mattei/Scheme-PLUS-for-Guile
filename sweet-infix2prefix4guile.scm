#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

;; sweet-infix2prefix4guile.scm

;; The parser of Scheme+ for Guile for the programs written in sweet-expressions (SRFI 110 : the indentation gives the
;; parenthesis , with the curly infix , the neoteric expressions and all of Scheme+ : infix with precedence , [ ] ,
;; superscripts , postfix , pragmas , type annotations). The code written with parenthesis gives the same forms as with
;; curly-infix2prefix4guile.scm. It writes the program in prefix Scheme on the standard output.
;; As specified by SRFI 110 , a parsing directive written alone at the beginning of a line (not at the beginning of the
;; file) changes the reading from the next datum : #!curly-infix reads the rest of the file with the rules of SRFI 105 ,
;; #!sweet goes back to the sweet-expressions , #!no-sweet reads with the read of Guile.
;; An empty line ends an expression.

;; usage :
;;   guile -L <directory of Scheme+ for Guile> sweet-infix2prefix4guile.scm [--verbose] program-sweet+.scm > program.scm
;; or , with the directory of Scheme+ for Guile in GUILE_LOAD_PATH :
;;   sweet-infix2prefix4guile.scm [--verbose] program-sweet+.scm > program.scm

;; Translated from curly-infix2prefix4racket.rkt (SRFI 110 for Racket) , the reader (SRFI-110 kernel) from kernel.rkt of
;; SRFI 110 for Racket.

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


(use-modules (ice-9 pretty-print)
	     (ice-9 textual-ports) ; get-string-all
	     (srfi srfi-11) ; let*-values
	     (SRFI-110 kernel)
	     ((SRFI-105 annot+) #:select (remove-annotation-markers))
	     (SRFI-105 parser-common)
	     ((Scheme+ parser-declarations) #:select (verify-if-declaration-for-parser-then-store-it
						      reset-declarations-for-parser!))
	     ((Scheme+ care-of-quotes) #:select (process-care-of-quotes)))


(define version "3.3") ; the version of SRFI 110 for Racket that this parser follows
(define scheme+-version "20.4")

(define stderr (current-error-port))
(define stdout (current-output-port))

(define verbose #f)

;; the type annotation , the generic arithmetic of Guile in the annotated code , the check of the assignments and
;; the header of the file are in (SRFI-105 parser-common) , shared with curly-infix2prefix4guile.scm


;; read all the forms of the file , the pragmas and the declarations for the parser are processed (not kept)
(define (read-forms in src)
  (let loop ((acc '()))
    ;; the directives #!curly-infix , #!sweet , #!no-sweet of SRFI 110 change the reader of the file from the next datum
    (define result (read-with-directives in src))
    (cond ((eof-object? result) (reverse acc))
	  ((begin
	     (process-annotation-pragma result) ; ANNOTATION-ON / ANNOTATION-OFF : stay in the code as markers of region
	     (or (process-strict-pragma result) ; BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION
		 (verify-if-declaration-for-parser-then-store-it result))) ; a declaration of postfix operators
	   (loop acc))
	  (else (loop (cons result acc))))))


(define (convert-file src)
  (set-annot-flag! #f) ; the pragmas act only on the file where they are written
  (set-annot-on-read! #f)
  (reset-srfi-strict!)
  (reset-declarations-for-parser!)
  (enable-sweet) ; SRFI 110 : a file starts in the indentation (sweet-expression) mode
  (when verbose
    (display (string-append "SRFI 110 Curly Infix , Neoteric and Sweet expressions v" version " for Scheme+ v"
			    scheme+-version " (Guile) by Damien Mattei.")
	     stderr)
    (newline stderr)
    (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)" stderr) (newline stderr) (newline stderr))
  (let*-values (((text) (call-with-input-file src get-string-all))
		((code flag-r6rs) (skip-header text))
		((in) (open-input-string code)))
    (set-port-filename! in src)
    (set-port-line! in (count-lines (substring text 0 (- (string-length text) (string-length code)))))
    (if flag-r6rs
	(let ((result (read-with-directives in src)))
	  (when (eof-object? result)
	    (error "ERROR: EOF : End Of File : " result))
	  (display "#!r6rs") (newline)
	  (pretty-print (remove-annotation-markers result) stdout)
	  (newline))
	(let ((result (read-forms in src)))
	  (when (null? result)
	    (error "ERROR: Empty program."))
	  (when verbose
	    (display "type annotation : " stderr) (display (if (get-annot-on-read) "on" "off") stderr) (newline stderr))
	  (let ((converted (annotate-or-remove-types (process-care-of-quotes result) (get-annot-on-read))))
	    (check-assignments converted "sweet-infix2prefix4guile.scm")
	    (for-each (lambda (expr) (pretty-print expr stdout))
		      converted))))))


(define (main args)
  (define options (cdr args))
  (when (member "--help" options)
    (display "sweet-infix2prefix4guile.scm documentation:") (newline) (newline)
    (display "guile -L <directory of Scheme+ for Guile> sweet-infix2prefix4guile.scm [options] file2parse.scm") (newline)
    (newline)
    (display "options:") (newline) (newline)
    (display "  --verbose : display information on stderr") (newline) (newline)
    (exit 0))
  (when (member "--verbose" options)
    (set! verbose #t))
  (when (null? options)
    (display "sweet-infix2prefix4guile.scm : a file name is needed (see --help)" stderr) (newline stderr)
    (exit 1))
  (let ((file-name (car (reverse options))))
    (when (string-prefix? "--" file-name)
      (error "filename start with -- ,this is confusing with options."))
    (convert-file file-name)))
