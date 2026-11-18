#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

;; curly-infix2prefix4guile.scm

;; The parser of Scheme+ for Guile : it reads a program written in Scheme+ (SRFI 105 Curly Infix , infix with
;; precedence , [ ] , superscripts , postfix , pragmas ANNOTATION-ON / ANNOTATION-OFF and CARE-OF-QUOTES-ON /
;; CARE-OF-QUOTES-OFF , type annotations) and writes the program in prefix Scheme on the standard output.
;; Guile has no reader that a module can replace (as #lang reader SRFI-105 in Racket) : the program is converted by this
;; parser , then the converted file is loaded or compiled by Guile.

;; usage :
;;   guile -L <directory of Scheme+ for Guile> curly-infix2prefix4guile.scm [--verbose] program+.scm > program.scm
;; or , with the directory of Scheme+ for Guile in GUILE_LOAD_PATH :
;;   curly-infix2prefix4guile.scm [--verbose] program+.scm > program.scm

;; Translated from curly-infix2prefix4racket.rkt (SRFI 105 for Racket).

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
	     (SRFI-105 SRFI-105-curly-infix)
	     ((SRFI-105 annot+) #:select (remove-annotation-markers))
	     (SRFI-105 parser-common)
	     ((Scheme+ parser-declarations) #:select (verify-if-declaration-for-parser-then-store-it
						      reset-declarations-for-parser!))
	     ((Scheme+ care-of-quotes) #:select (process-care-of-quotes)))


(define version "17.2") ; the version of SRFI 105 for Racket that this parser follows
(define scheme+-version "20.2")

(define stderr (current-error-port))
(define stdout (current-output-port))

(define verbose #f)

;; the type annotation , the generic arithmetic of Guile in the annotated code , the check of the assignments and
;; the header of the file are in (SRFI-105 parser-common) , shared with sweet-infix2prefix4guile.scm


;; read all the forms of the file , the pragmas and the declarations for the parser are processed (not kept)
(define (read-forms in src)
  (let loop ((acc '()))
    (define result (curly-infix-read in src))
    (cond ((eof-object? result) (reverse acc))
	  ((read-was-a-datum-comment?) (loop acc)) ; #;<datum> at the top level : dropped (curly-infix2prefix4racket keeps it)
	  ((begin
	     (process-annotation-pragma result)
	     (or (process-strict-pragma result) ; BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION
		 (verify-if-declaration-for-parser-then-store-it result))) ; a declaration of postfix operators
	   (loop acc))
	  (else (loop (cons result acc))))))


(define (convert-file src)
  (set-annot-flag! #f) ; the pragmas act only on the file where they are written
  (set-annot-on-read! #f)
  (reset-srfi-strict!)
  (reset-declarations-for-parser!)
  (when verbose
    (display (string-append "SRFI 105 Curly Infix v" version " for Scheme+ v" scheme+-version " (Guile) by Damien Mattei.")
	     stderr)
    (newline stderr)
    (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)" stderr) (newline stderr) (newline stderr))
  (let*-values (((text) (call-with-input-file src get-string-all))
		((code flag-r6rs) (skip-header text))
		((in) (open-input-string code)))
    (set-port-filename! in src)
    (set-port-line! in (count-lines (substring text 0 (- (string-length text) (string-length code)))))
    (if flag-r6rs
	(let ((result (curly-infix-read in src)))
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
	    (check-assignments converted "curly-infix2prefix4guile.scm")
	    (for-each (lambda (expr) (pretty-print expr stdout))
		      converted))))))


(define (main args)
  (define options (cdr args))
  (when (member "--help" options)
    (display "curly-infix2prefix4guile.scm documentation:") (newline) (newline)
    (display "guile -L <directory of Scheme+ for Guile> curly-infix2prefix4guile.scm [options] file2parse.scm") (newline)
    (newline)
    (display "options:") (newline) (newline)
    (display "  --verbose : display information on stderr") (newline) (newline)
    (exit 0))
  (when (member "--verbose" options)
    (set! verbose #t))
  (when (null? options)
    (display "curly-infix2prefix4guile.scm : a file name is needed (see --help)" stderr) (newline stderr)
    (exit 1))
  (let ((file-name (car (reverse options))))
    (when (string-prefix? "--" file-name)
      (error "filename start with -- ,this is confusing with options."))
    (convert-file file-name)))
