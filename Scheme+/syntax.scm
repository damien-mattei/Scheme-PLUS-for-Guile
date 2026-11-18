;; Translated from Scheme+ for Racket (syntax.rkt) for Guile.

;; This file is part of Scheme+

;; Copyright 2024-2025 Damien MATTEI

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


;; code from Scheme+R6RS

(define-module (Scheme+ syntax)
  #:use-module (Scheme+ racket-compat))
  
  


  (use-modules
	((srfi srfi-1) #:select (any member)))
  



  ;; (procedure->string +)
  ;; "+"

  ;; (procedure->string foo)
  ;; "foo"

  ;; (procedure->string (lambda (x) x))
  ;; #f  ; anonymous procedure : no name to extract
  ;; Guile : the name is given by procedure-name (Racket reads it in the printed form #<procedure:+>)
  (define (procedure->string p) ; #f when p is anonymous : there is no name to extract
    (define name (procedure-name p))
    (and name (symbol->string name)))


  (define (procedure->symbol p) ; #f when p is anonymous
    (define s (procedure->string p))
    (and s (string->symbol s)))

  ;; WRONG : do not work, always return obj1,obj2 .... because macro
  ;; > (procedure->symbol +)
  ;; '+
  ;; (define-syntax (procedure->symbol stx)
  ;;   (syntax-case stx ()
  ;;     [(_ proc)

  ;;      (with-syntax
	   
  ;; 	   ((str-proc
            
  ;;            (symbol->string  (syntax-e #'proc)))) ; should work also with syntax->datum
	 
  ;;        #'(string->symbol str-proc)
  ;;        )])) 
  

  ;; (datum=? #'* *)
  ;; #t

  ;; (define m *)
  ;; (define sm #'*)
  ;; (datum=? m sm)
  ;; #t


  ;; racket does not allow to syntax->datum an object not being (syntax something)
  (define (datum=? obj1 obj2)
       ;;(display "datum=? : obj1 =") (display obj1) (newline)
       ;;(display "datum=? : obj2 =") (display obj2) (newline)

       (when (syntax? obj1)
	 (set! obj1 (syntax->datum obj1)))

       (when (syntax? obj2)
	 (set! obj2 (syntax->datum obj2)))

       ;; if p has no name (anonymous procedure) procedure->symbol returns #f :
       ;; leave obj as the raw procedure then, so the final equal? falls back to
       ;; comparing the two procedures by identity instead of a fake placeholder
       (when (procedure? obj1)
	 (define name1 (procedure->symbol obj1))
	 (when name1 (set! obj1 name1)))

       (when (procedure? obj2)
	 (define name2 (procedure->symbol obj2))
	 (when name2 (set! obj2 name2)))

       (define rv (equal? obj1 obj2))
       ;;(display "datum=? rv =") (display rv) (newline)
       rv)


  ; free-identifier? seemed to be a good replacement for datum=? but then i have errors not understood
  ;(define datum=? free-identifier=?)
  #| cause errors : ../if-parser.rkt:94:0: free-identifier=?: contract violation
  expected: identifier?
  given: #<syntax:/home/mattei/Dropbox/git/SRFI-105-for-Racket/annot+.rkt:14:8 (remove-expr (cdr s) p?)> |#


  
  (define (secure-syntax->datum s)
    (if (syntax? s)
        (syntax->datum s)
        s))
  
  
;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > (define op-lst (list #'* #'+ #'- #'/))
;; > (member-generic #'+ op-lst)
;; #t



;; (member-generic '+ '(- + / *))
;; #t
;; (member-generic + (list - + / *))
;; #t
;; (member-generic + (list - / *))
;; #f
;; (member-generic '+ '(- / *))
;; #f

;; > (member-generic #'+ (list - + / *))
;;#f

  (define (member-generic x lst)
    ;;(display "member-generic : x=") (display x) (newline)
    ;;(display "member-generic : lst=") (display lst) (newline)
    (any (lambda (y)
	   (datum=? x y))
	 lst))


  
  ;; transform any syntax in a list if possible
  ;; note:this macro made a side effect on the input variable, modifying it
  (define-syntax var-syntax2list
    (syntax-rules ()
      ((_ var)

       
       (let ((var-inter #f)) ; intermediate variable

	 ;;(display "var-syntax2list : var=") (display var) (newline)
	 (when (syntax? var)
	       ;; (display "var-syntax2list : detected syntax,passing from syntax to list (will be used if it is a list)")
	       ;; (display " --> var-syntax2list : var=") (display var) (newline)
	       (set! var-inter (syntax->list var))
	       (when var-inter
		     ;;(display "var-syntax2list : got a list") (display " --> var-syntax2list : var=") (display var)(newline)(newline)
		     (set! var var-inter)))))))
      



(export
	datum=?
	member-generic
	procedure->symbol
	var-syntax2list
	secure-syntax->datum)
 ; end library


