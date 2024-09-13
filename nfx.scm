;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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


;; (use-modules (nfx))


(define-module (nfx)

  ;;#:use-module ((guile))
  
  #:use-module (n-arity)
  #:use-module (infix-with-precedence-to-prefix)
  #:use-module (operators-list)
  
  #:export ($nfx$))



  

;; scheme@(guile-user)>  ($nfx$ 3 * 5 + 2)
;; $1 = 17


(define-syntax $nfx$

  (lambda (stx)
    
    (syntax-case stx ()

      ;; note that to have $nfx$ called you need at minimum to have 2 different operator causing an operator precedence question
      ;; and then at least those 2 operators must be between operands each, so there is a need for 3 operand
      ;; the syntax then looks like this : e1 op1 e2 op2 e3 ..., example : 3 * 4 + 2
      (($nfx$ e1 op1 e2 op2 e3 op ...) ; note: i add op because in scheme op ... could be non existent

	 (with-syntax
			 
		       ((parsed-args
			 ;; TODO : make n-arity for <- and <+ only (because could be false with ** , but not implemented in n-arity for now)
			
			
			  (n-arity ;; this avoids : '{x <- y <- z <- t <- u <- 3 * 4 + 1}
			   ;; SRFI-105.scm : !0 result = (<- (<- (<- (<- (<- x y) z) t) u) (+ (* 3 4) 1)) ;; fail set! ...
			   ;; transform in : '(<- x y z t u (+ (* 3 4) 1))
			   (!0-generic #'(e1 op1 e2 op2 e3 op ...) ; apply operator precedence rules
				       infix-operators-lst-for-parser-syntax
				       (lambda (op a b) (list op a b))))))
	   
	   (display "$nfx$ : parsed-args=") (display #'parsed-args) (newline)
	   #'parsed-args)))))



;; scheme@(guile-user)> {3 * 4 + 1}
;; $2 = 13

;; scheme@(guile-user)>  { #f or #f and (begin (display "BAD") (newline) #t)}
;; $3 = #f

;; scheme@(guile-user)> { 4 + 3 * 2 - 19 < 0 - 4}
;; $4 = #t

;; scheme@(guile-user)> (define c 300000)
;; scheme@(guile-user)> (define v 299990)
;; scheme@(guile-user)> (define t 30)
;; scheme@(guile-user)>  (define x 120)
;; scheme@(guile-user)> (declare xp)
;; scheme@(guile-user)> '{xp <- {x - v * t} / (sqrt {1 - v ** 2 / c ** 2})}
;; ($nfx$ xp <- ($nfx$ x - v * t) / (sqrt ($nfx$ 1 - v ** 2 / c ** 2)))
;; scheme@(guile-user)> { xp <- {x - v * t} / (sqrt {1 - v ** 2 / c ** 2}) }
;; scheme@(guile-user)> xp
;; $6 = -1102228130.2405226

;; scheme@(guile-user)> '{u <+ 3 * 5 + 4 - 1}
;; $7 = ($nfx$ u <+ 3 * 5 + 4 - 1)
;; scheme@(guile-user)> {u <+ 3 * 5 + 4 - 1}
;; !0-generic : terms=(#<syntax:unknown file:8:1 u> #<syntax:unknown file:8:3 <+> #<syntax:unknown file:8:6 3> #<syntax:unknown file:8:8 *> #<syntax:unknown file:8:10 5> #<syntax:unknown file:8:12 +> #<syntax:unknown file:8:14 4> #<syntax:unknown file:8:16 -> #<syntax:unknown file:8:18 1>)
;; !0-generic : rv=(#<syntax:unknown file:8:3 <+> #<syntax:unknown file:8:1 u> (#<syntax:unknown file:8:16 -> (#<syntax:unknown file:8:12 +> (#<syntax:unknown file:8:8 *> #<syntax:unknown file:8:6 3> #<syntax:unknown file:8:10 5>) #<syntax:unknown file:8:14 4>) #<syntax:unknown file:8:18 1>))
;; scheme@(guile-user)> u
;; $8 = 18

;; scheme@(guile-user)> (declare x y z t u)
;; scheme@(guile-user)> '{x <- y <- z <- t <- u <- 3 * 4 + 1}
;; $1 = ($nfx$ x <- y <- z <- t <- u <- 3 * 4 + 1)
;; scheme@(guile-user)> {x <- y <- z <- t <- u <- 3 * 4 + 1}
;; $nfx$ : parsed-args=#<syntax (<- x y z t u (+ (* 3 4) 1))>
;; scheme@(guile-user)> z
;; $2 = 13
