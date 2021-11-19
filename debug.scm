;; debug Macros and Functions
;; 
       
;; Copyright 2021 Damien MATTEI

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


(define debug-mode #t) ;; global debug-mode definition (you must set it in the function to debug to avoid a lot of information on display) 

(define debug-mode-save debug-mode)

(define (debug-mode-on)
  (set! debug-mode-save debug-mode)
  (set! debug-mode #t))
    
(define (debug-mode-reload)
  (set! debug-mode debug-mode-save))

(define (debug-mode-off)
  (set! debug-mode #f))


;; (debug block) or (nodebug block)

(define-syntax debug
  (syntax-rules ()
    
    ((_ instruction)
     (begin
       (debug-mode-on)
       (when debug-mode
	     instruction)
       (debug-mode-reload)))
  
    ((_ instruction ...)
     (begin
       (debug-mode-on)
       (when debug-mode
	     instruction
	     ...)
       (debug-mode-reload)))))


(define-syntax nodebug
  (syntax-rules ()

    ((_ instruction ...)
     (when debug-mode
	   instruction
	   ...))
    
    ((_ instruction)
     (when debug-mode
	   instruction))))



;; display only if debug mode on for the functions below

(define-syntax debug-display
  (syntax-rules ()
    ((_ obj) (if debug-mode (display obj)))
    ((_ obj port) (if debug-mode (display obj port)))))


;; an enhanced newline that will only "new line" in debug mode LOL
(define-syntax debug-newline
  (syntax-rules ()
    ((_) (if debug-mode (newline)))))


(define-syntax debug-display-var-nl 
  (syntax-rules ()
    ((_ msg var) (if debug-mode (display-var-nl msg var)))))


;; debug with display-nl
(define-syntax debug-display-nl 
  (syntax-rules ()
    ((_ msg) (begin
	       ;;(display "debug.scm : debug-display-nl : debug-mode = ") ;; this has been added for debugging the debug macros !!! (and should be removed later)
	       ;;(display debug-mode)
	       ;;(newline)
	       (if debug-mode
		  (begin (display msg) 
			 (newline)))))))


;; continue with  display-msg-symb-nl
;; macros or function to display a variable with a message and add a newline
(define-syntax debug-display-msg-symb-nl 
  (syntax-rules ()
    ((_ msg symbl) (if debug-mode  
		       (begin
			 (display msg)
			 (display " ")
			 (display (symbol->string (quote symbl)))
			 (display " = ")
			 (display symbl)
			 (newline))))))


;; tired of rewriting debug macros starting from normal ones i wrote this one that encapsulate ANY one !!!
;; example(s):
;; (debug-only display-msg-var-nl "Sidonie : DBtoWebObserversKawa : work : (car wds-data-str-split) = " (car wds-data-str-split))
(define-syntax debug-only
  (syntax-rules ()

    ((_ fct-or-mac ...)
     ;;(begin
       ;;(display "debug.scm : debug-only : debug-mode = ") ;; this has been added for debugging the debug macros !!! (and should be removed later)
       ;;(display debug-mode)
       ;;(newline)
       (when debug-mode
	     (fct-or-mac ...)));)
    
    ((_ instruction)
     ;;(begin
       ;;(display "debug.scm : debug-only : debug-mode = ") ;; this has been added for debugging the debug macros !!! (and should be removed later)
       ;;(display debug-mode)
       ;;(newline)
       (when debug-mode
	     (instruction)))));)



