;; Translated from Scheme+ for Racket (while-do.rkt) for Guile.

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


(define-module (Scheme+ while-do)
  #:use-module (Scheme+ racket-compat))


  

  (use-modules
	((guile) #:select ((do . do-scheme)))) ;; backup original Scheme 'do'



;; warning: 'do is already part of R6RS (reserved keyword) 'while is not in R5RS,R6RS, R7RS-small

;; but 'do in Scheme has a painful syntax

;; syntax defined in this file are inspired from Pascal language, C , Java,Javascript

;; scheme@(guile-user)> (use-modules (Scheme+R6RS))
;; scheme@(guile-user)> (define i 0)
;; scheme@(guile-user)> (define do '())
;; scheme@(guile-user)> (while {i < 4}
;;                           do
;;                              (display i)
;;                              (newline)
;;                              {i <- {i + 1}})
;; 0
;; 1
;; 2
;; 3
;; $1 = #f

;; (while {i < 4}
;;    do
;;      (display i)
;;      (newline)
;;      {i <- {i + 1}})

;; (define-syntax while
;;   (syntax-rules (while do)
;;     ((_ pred do b1 ...)
;;      (let loop () (when pred b1 ... (loop))))))

;; bug: 1+ return #f with SRFI 105 reader of Racket

;; (do ((i 1 (1+ i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (format #t "3**~s is ~s\n" i p))
;; 3**1 is 3
;; 3**2 is 9
;; 3**3 is 27
;; 3**4 is 81
;; $1 = 243

;; scheme@(guile-user)> (do ((i 1 (1+ i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (set! p (+ p i)))
;; $1 = 417


;; with a definition inside only the new version works:
;; (do ((i 1 (+ 1 i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (define x 7)
;;   (set! p (+ p i x)))
;; $3 = 1257


;; 'do is redefined here only to allow 'define in body as allowed in Scheme+R6RS
;; (define-syntax do

;;   (syntax-rules ()

;;     ((do ((var init step ...) ...)

;;          (test expr ...)

;;          command ...)

;;      (letrec

;;        ((loop

;;          (lambda (var ...)

;;            (if test

;;                ;;(begin
;; 	       (let ()

;;      		 ;;#f ; avoid empty begin but with (let () i don't care ! (bigloo use: (if #f #f) which creates a never executed instruction !)
;; 		 '() ;; avoid while-do-when-unless.scm: let: bad syntax (missing binding pairs or body) in: (let ())
;;                  expr ...)

;;                ;;(begin
;; 	       (let ()

;;                  command

;;                  ...

;;                  (loop (do "step" var step ...)

;;                        ...))))))

;;        (loop init ...)))

;;     ((do "step" x)

;;      x)

;;     ((do "step" x y)

;;      y)))



;; > (define i 0) 
;; > (do (display "toto") (newline) (set! i (+ i 1)) while (< i 4)) 
;; toto
;; toto
;; toto
;; toto
;;  this 'do' do not break the one of scheme:

;;  (do ((i 1 (+ i 1))
 ;;     (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (display p)(newline))
;; 3
;; 9
;; 27
;; 81
;; 243

;; > (do  (define j i)  (display "toto") (newline) (set! i (+ i 1)) while (< j 4)) 
;; toto
;; toto
;; toto
;; toto
;; toto
(define-syntax do
  (syntax-rules (while)

    ;; original do from scheme
    #;((do ((variable init step ...) ...) (test expr ...) body ...)
     (do-scheme ((variable init step ...) ...) (test expr ...) body ...))

    ;; original do from scheme , without body
    ((do ((variable init step ...) ...) (test expr ...))
     (do-scheme ((variable init step ...) ...) (test expr ...)))

    ;; original do from scheme , its body can contain definitions as in Scheme+ (let () is free at run time , the
    ;; (void) allows a body that ends with a definition : the value of the body is not used)
    ((do ((variable init step ...) ...) (test expr ...) body ...)
     (do-scheme ((variable init step ...) ...) (test expr ...) (let () body ... (void))))

    ;; do from scheme+
    ((do b1 ...
       while pred)
     (let loop () b1 ... (when pred (loop))))))




;; > (define x 0)
;; > (while (< x 10)
;;           (define y x)  
;;             (display y)
;;             (newline)
;;             (set! x (+ x 1)))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; > 
;; (define-syntax-rule (while condition body ...)
;;   (let loop ()
;;     (when condition
;;       body ...
;;       (loop))))


(define-syntax while

  (syntax-rules ()
    
    ((while condition body ...)
  
     (let loop ()
       (when condition
	     body ...
	     (loop))))))


;; note: as 'do' already exist in scheme it is not possible to define this:

;; (define-syntax while

;;   (syntax-rules (do)
    
;;     ((while condition do body ...)
  
;;      (let loop ()
;;        (when condition
;; 	     body ...
;; 	     (loop))))))






(eval-when (expand load eval)
  (module-replace! (current-module) '(while do)))
 ; end library

