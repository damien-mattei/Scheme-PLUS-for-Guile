;; Translated from Scheme+ for Racket (repeat-until.rkt) for Guile.

; This file is part of Scheme+

;; Copyright 2021-2024 Damien MATTEI

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




(define-module (Scheme+ repeat-until)
  #:use-module (Scheme+ racket-compat))

	

  	(use-modules
	(Scheme+ nfx))


;; scheme@(guile-user)> (define i 5)
;; scheme@(guile-user)> (repeat (display i) (newline) (set! i (- i 1)) until (< i 0))
;; 5
;; 4
;; 3
;; 2
;; 1
;; 0

	(define-syntax repeat
	  
	  (syntax-rules (until)
	    
	    ((repeat b1 ...
		     until pred)
	     ;;until pred e1 ...) ; ellipsis not allowed here ! was to try to allow direct infix expressions too
	     ;; this can only be done the way i made it for 'if' : with-syntax and parser
	     
	     (let loop ()
	       b1
	       ...
	       (when (not pred) ;;(not ($nfx$ pred e1 ...))
		 (loop))))))


	
	    
	    

	

(export
	repeat)
 ; end library

