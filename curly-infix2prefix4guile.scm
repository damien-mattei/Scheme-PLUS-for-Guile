#!/usr/local/bin/guile -s
!#


;; infix optimizer with precedence operator by Damien Mattei


;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Guile by Damien Mattei

;; example of use with all infixes optimizations:

;./curly-infix2prefix4guile.scm    --infix-optimize --infix-optimize-slice ../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile+.scm > ../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile-optim-infix-slice.scm

;; options:

;; --srfi-105 : set strict compatibility mode with SRFI-105


;;(use-modules (ice-9 textual-ports)) ;; allow put-back characters
(use-modules (ice-9 pretty-print)
	     (srfi srfi-31) ; rec
	     (srfi srfi-1)) ; first ,third 

(include "rest.scm")
(include "operation-redux.scm")
(include "optimize-infix.scm")
(include "assignment-light.scm")
(include "block.scm")
(include "declare.scm")
(include "slice.scm")
(include "def.scm")
(include "optimize-infix-slice.scm")

(include "when-unless.scm")
(include "while-do.scm")

(define stderr (current-error-port))

(include "condx.scm")

(include "SRFI-105.scm")

(define srfi-105 #f)

;; quiet mode that do not display on standart error the code
(define verbose #f)

(define (literal-read-syntax src)

  (define in (open-input-file src))
  (define lst-code (process-input-code-tail-rec in))
  lst-code)
 

;; read all the expression of program
;; DEPRECATED (replaced by tail recursive version)
(define (process-input-code-rec in)
  (define result (curly-infix-read in))  ;; read an expression
  (if (eof-object? result)
      '()
      (cons result (process-input-code-rec in))))


;; read all the expression of program
;; a tail recursive version
(define (process-input-code-tail-rec in) ;; in: port


  (when verbose
	  (display "SRFI-105 Curly Infix parser with operator precedence by Damien MATTEI" stderr) (newline stderr)
	  (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)" stderr) (newline stderr) (newline stderr)

	  (when srfi-105
		(display "SRFI-105 strict compatibility mode is ON." stderr))
	  (newline stderr)

	  (newline stderr) 
  
	  (display "Parsed curly infix code result = " stderr) (newline stderr) (newline stderr))
  
  (define (process-input acc)
    
    (define result (curly-infix-read in))  ;; read an expression

    ;;(display (write result stderr) stderr) ;; without 'write' string delimiters disappears !
    ;;(display result stderr)
    ;;(write result stderr)

    (when verbose
	    (pretty-print result stderr)
	    (newline stderr)
	    (newline stderr))
    
    (if (eof-object? result)
	(reverse acc)
	(process-input (cons result acc))))
  
  (process-input '()))

  
  
; parse the input file from command line
(define cmd-ln (command-line))
;;(format #t "The command-line was:~{ ~w~}~%" cmd-ln)
;;(display "cmd-ln=") (display cmd-ln) (newline)

(define options (cdr cmd-ln))
;;(display "options= ") (display options) (newline)



(when (member "--help" options)
      (display "curly-infix2prefix4guile.scm documentation: (see comments in source file for more examples)") (newline) (newline) 
      (display "curly-infix2prefix4guile.scm [options] file2parse.scm") (newline) (newline)
      (display "options:") (newline)(newline)
      (display "  --srfi-105 : set strict compatibility mode with SRFI-105 ") (newline) (newline)
      (display "  --verbose : display code on stderr too ") (newline) (newline)
      (exit))


;; SRFI-105 strict compatibility option
(when (member "--srfi-105" options)
      (set! nfx-optim #f)
      (set! slice-optim #f))

(when (member "--verbose" options)
      (set! verbose #t))

(define file-name (car (reverse cmd-ln)))

(when (string=? (substring file-name 0 2) "--")
      (error "filename start with -- ,this is confusing with options."))

(define code-lst (literal-read-syntax file-name))



(define (dsp-expr expr)
  ;;(display (write expr)) ;; without 'write' string delimiters disappears !
  ;;(write expr)
  (pretty-print expr)
  ;;(display expr)
  (newline)
  (newline))



(for-each dsp-expr code-lst)


