;; source-locations.scm

;; Guile Scheme version

;; In Scheme+ for Racket this module keeps the location of the text of each form and identifier through the infix
;; conversion and the type annotation , to give the true line and column to the errors and to the tools of Racket.
;; In Guile the parser is external : it writes a converted file , and the locations of the source do not go through
;; it. This module keeps the same interface , and does nothing : the procedures return what the Racket ones return.

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


(define-module (Scheme+ source-locations)

  #:export (reset-source-locations!
	    enter-form!
	    leave-form!
	    call-with-located-errors
	    location-start
	    location-from
	    record-form!
	    record-element!
	    record-datum!
	    reset-read-atoms!
	    record-atom!
	    relink-read-atoms!
	    link-form!
	    link-cell!
	    relink-symbols!
	    relink-form!
	    datum->located-syntax))


(define* (reset-source-locations! #:optional (src #f)) (if #f #f))
(define (location-start port) #f)
(define (location-from start port) #f)
(define (record-form! form location) (if #f #f))
(define (record-element! cell location) (if #f #f))
(define (record-datum! cell datum location) cell)
(define (reset-read-atoms!) (if #f #f))
(define (record-atom! a location) (if #f #f))
(define (relink-read-atoms! expression) expression)
(define (link-form! new old) new)
(define (link-cell! new old) new)
(define (relink-symbols! out in) out)
(define (relink-form! new old) new)
(define (enter-form! s) #f)
(define (leave-form! saved) (if #f #f))
(define (call-with-located-errors thunk) (thunk))
(define (datum->located-syntax datum src) datum)
