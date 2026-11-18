;; Translated from Scheme+ for Racket (superscript-parser.rkt) for Guile.

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

;; this code parse the superscript part expression of a normal one

(define-module (Scheme+ superscript-parser)
  #:use-module (Scheme+ racket-compat))

	 ; for debug

	(use-modules
	(Scheme+ superscript)
	(Scheme+ def-function)
	(Scheme+ operators)
	(srfi srfi-13))

	;; parsing superscript ⁻⁺⁰¹²³⁴⁵⁶⁷⁸⁹ natural numbers
	;; see parser-superscript.odg or jpeg image file
	;; see also parse-superscript-arithmetic-syntax.odg

;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
	
	;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript) ? no more works?
	
;; Using the text editor vim, one can produce subscripted and superscripted numbers by using the digraphs control-k-ns for subscription and control-k-nS for superscription, where n is an Arabic numeral.

	
;; {3 · 5 + 2}
;; 17

;; {3 · 5 + 2 ³}
;; !*-generic-infix-parser : terms = ((.#<syntax +> (.#<syntax ·> .#<syntax 3> .#<syntax 5>) (** .#<syntax 2> 3)))
;; $nfx$ : parsed-args=.#<syntax (+ (· 3 5) (** 2 3))>
;; 23

;; {3 ² + 2 · 3 · 5 + 5 ²}
;; 64

;; {3 ⁻²}
;; 1/9

;; {2 ¹⁰}
;; 1024



(define (error-superscript lst)
  (error "Error parsing superscript : the superscript are in a wrong position in the list: " lst))

;; (superscript-operator-loop '(3 ²))
;; '(3 ** 2)

;; (superscript-operator-loop '(3 ²·³⁻⁴))
;; '(3 ** (2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:310:38 ·> 3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:285:38 -> 4))

;; (superscript-operator-loop '(3 ²·⁽³⁻⁴⁾))
;; '(3 ** (2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:310:38 ·> (3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:285:38 -> 4)))


(def (superscript-operator-loop lst)
  ;;(display "superscript-operator-loop : lst=") (display lst) (newline)
  (when (null? lst)
    (return lst))
  (define elem (car lst))
  (cond ((operator-symbol-or-syntax? elem) (cons elem ; we keep it in the resulting list
						 (superscript-operator-loop (cdr lst)))) ; and loop in the same machine state
	((superscript? elem) (error-superscript lst))
	(else
	 (cons elem ; we keep it in the resulting list
	       (sexpr-superscript (cdr lst))))))




(def (sexpr-superscript lst) ;  deal with **
  (when (null? lst)
    (return lst))
  (define elem (car lst))
  (cond ((operator-symbol-or-syntax? elem) (cons elem ; we keep it in the resulting list
						 (superscript-operator-loop (cdr lst)))) ; and loop in the superscript-operator-loop state

	;; call parse-superscript-token
	((superscript? elem) (cons '** ;'expt ;(syntax **) ;'** ; ** with superscript
				   (cons (parse-superscript-token elem) ; (generic-superscript-number->number elem) ; number
					 
					 ;;(superscript-operator-loop (cdr lst))))) ; forbid repeated superscript
					 (sexpr-superscript (cdr lst))))) ; loop and allow repeated superscript (follow automata)
	
	(else ; got an other sexpr,  we keep it in the resulting list
	       (cons elem
		     (sexpr-superscript (cdr lst)))))) ; loop in the same state



;; routines below only parse superscripts syntax in tree of tokens

;; car and cdr for strings
(define (first-string str)
  (string-ref str 0)) ; char

(define (rest-string str)
  (substring str 1)) ; string


(define (only-one? expr)
  (null? (cdr expr)))

(define (extract-atom-if-any L)
  (if (only-one? L)
      (car L)
      L))


;; index of the closing parenthesis matching an opening one already read , #f if it is missing
;; (the last closing parenthesis of the string is not the good one when there are several groups)
;; (matching-closing-parenthesis-index "1+1)·(1+2)")
;; 3
;; (matching-closing-parenthesis-index "4-(5-2))-2")
;; 7
(define (matching-closing-parenthesis-index str)
  (let loop ((i 0)
	     (depth 0)) ; number of nested opened parenthesis
    (cond ((= i (string-length str)) #f)
	  ((char=? (string-ref str i) #\() (loop (+ i 1) (+ depth 1)))
	  ((char=? (string-ref str i) #\)) (if (= depth 0)
					      i
					      (loop (+ i 1) (- depth 1))))
	  (else (loop (+ i 1) depth)))))


;; (parse-superscript-token "17")
;;17

;; (parse-superscript-token "¹⁷")
;;17

;; (parse-superscript-token "3+4")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:192:28 +> 4)

;; (parse-superscript-token "3+4-5")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:192:28 +> 4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:196:28 -> 5)

;; (parse-superscript-token "3*(4-5)")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:204:28 *> (4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:196:28 -> 5))

;; (parse-superscript-token "3+2i")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:207:28 +> 0+2i)

;; (parse-superscript-token "3*(4-5)-2")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:201:28 *> (4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> 5) .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> 2)

;;(parse-superscript-token "-5")
;; '(- 5)

;; (parse-superscript-token "alpha2")
;; 'alpha2

;; (parse-superscript-token "²·³⁻⁴")
;; '(2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:306:38 ·> 3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:281:38 -> 4)

(define (parse-superscript-token ss-tok)
  (extract-atom-if-any
   (state-0-syntax-analysis ss-tok)))





;; Syntax analysis (State 0)

(def (state-0-syntax-analysis ss-tok)  ; arguments are a superscript token in syntax or symbol in superscript characters
					
     ;; prepare data
     
     (define sd ss-tok)
     (when (syntax? ss-tok) ; could also be something not syntax,example:  ¹²  
       (set! sd (syntax->datum ss-tok))) ; convert it in datum (number,symbol,....)

     (define str-tok sd)
     (when (symbol? str-tok)
       (set! str-tok (symbol->string sd))) ; convert the possibly symbol ,possibly superscripted in string
     
     ;; convert the possibly superscript string in normal string (even if not necessary)
     (define tok (superscript->normal str-tok))

     (when (string-null? tok) ; end of expression to parse
       (return '())) ; create the NIL end of list expression
     
     (define c (first-string tok)) ; char
     (define nxt (rest-string tok)) ; string


     ;; tests (todo: note that i could have put all those tests in a 'cond' and use 'values' instead of 'return')
          
     ;; test for begin of a numeric with . or digit
     ;; -> state 2 number
     (when (or (char=? c #\.)
	       (char-numeric? c))
       ;; extract  the atom from list if exist
       (return
	(state-2-number (string c) nxt)))


     (when (char=? c #\+)
       (return 	(cons '+
		      (state-0-syntax-analysis nxt))))

     (when (char=? c #\-)
       ;;(display "superscript-parser : state-0-syntax-analysis : before cons #'-") (newline)
       ;; #'- had the lexical context of this module : in a module {3 ⁻²} gave "-: identifier's binding is ambiguous"
       (return 	(cons '- ;;#'- ;;- 
		      (state-0-syntax-analysis nxt))))

     (when (char=? c #\/)
       (return 	(cons '/
		      (state-0-syntax-analysis nxt))))

     (when (char=? c #\*)
       (return 	(cons '*
		      (state-0-syntax-analysis nxt))))

     (when (char=? c #\·)
       (return 	(cons '·
		      (state-0-syntax-analysis nxt))))
       

     ;; note: here symbol cannot be evaluated later , they are not syntax ! only an external pre-parser can prepare for later evaluation

     ;; test for alphabetic that should end as a symbol
     ;; -> state 1 symbol
     (when (char-alphabetic? c)
       (return
	(state-1-symbol (string c) nxt)))


   	   
     ;; opening parenthesis ( is a special case
     (when (char=? c #\() ;  we extract the nested substring expression
       ;; search for the closing parenthesis
       ;; the last closing parenthesis is wrong with several groups : ⁽¹⁺¹⁾·⁽¹⁺²⁾
       #;(define index-closing-parenthesis-far (string-index-right nxt #\)))
       (define index-closing-parenthesis-far (matching-closing-parenthesis-index nxt))
       (unless index-closing-parenthesis-far
	 (error "state-0-syntax-analysis : ) (closing parenthesis) is missing in this expression : " nxt))
       (define substr (substring nxt 0 index-closing-parenthesis-far))     
       (define parsed-tokens-sub-expr (state-0-syntax-analysis substr)) ; nested parsing

       (define after-nested-str (substring nxt (+ 1 index-closing-parenthesis-far))) ; the rest of string to parse after closing parenthesis (note: we skip the closing parenthesis)
       
       (return (cons parsed-tokens-sub-expr ; this create the nested parentherized expression ( .... )
		     (state-0-syntax-analysis after-nested-str)))) ; the other parsing that rest to parse

     (error "state-0-syntax-analysis : unknow case of character to handle in those expressions : " (list tok c nxt)))







		    
;; Lexical analysis (states 1,2,3)
(def (state-2-number p-tok ss-tok)  ; partial token, sub string tokens
     ;,result tokens list

     ;; return case
     (when (string-null? ss-tok)
       (define n (string->number p-tok)) ; partial token is completed, it should be a number
       (if n
	   (return (cons n '())) ; create NIL of list expression
	   (error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))

     ;; (if n
     ;; 	 (return n ss-tok) ; should return 2 values : parsed token and rest of string to tokenize
     ;; 	 (error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))

       
     (define c (first-string ss-tok)) ; char
     (define nxt (rest-string ss-tok)) ; string
     
     ;; test for decimal separator . or digit
     (when (or (char=? c #\.)
	       (char-numeric? c))
       (return (state-2-number (string-append p-tok (string c))
			       nxt))) ; recall procedure with 2 arguments : token and rest of string where to decode other tokens


     ;; operators
     ;; -> state 0 todo: factorize code (see Racket discourse question about syntax)
     (cond ((char=? c #\+) ; found an operator
	    (define n (string->number p-tok)) ; partial token is completed, it should be a number
	    (if n
		(return (cons n
			      (cons '+
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
	   ((char=? c #\-) ; found an operator
	    (define n (string->number p-tok)) ; partial token is completed, it should be a number
	    (if n
		(return (cons n
			      (cons '-
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
	   ((char=? c #\/) ; found an operator
	    (define n (string->number p-tok)) ; partial token is completed, it should be a number
	    (if n
		(return (cons n
			      (cons '/
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
		
	   ((char=? c #\*) ; found an operator
	    (define n (string->number p-tok)) ; partial token is completed, it should be a number
	    (if n
		(return (cons n
			      (cons '*
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
	   ((char=? c #\·) ; found an operator
	    (define n (string->number p-tok)) ; partial token is completed, it should be a number
	    (if n
		(return (cons n
			      (cons '·
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok))))
	 
    
 
     ;; (pure) imaginary number
     (when (or (char=? c #\i)
	       (char=? c #\I))
       (return (state-3-pure-imaginary-number (string-append p-tok (string c))
					      nxt)))

     ;; Electrical convention for (pure) imaginary number
     (when (char=? c #\j)
       (return (state-3-pure-imaginary-number (string-append p-tok (string #\i))
					      nxt)))
     
     ;; symbol
     (when (char-alphabetic? c)
       (return (state-1-symbol (string-append p-tok (string c))
			       nxt)))

     (error "superscript-parser : state 2 number : unknow case of character to handle at the begin of those expressions : " (list c nxt)))



;; (parse-superscript-token "alpha+4")
;; '(alpha #\+ 4)

;; (superscript-operator-loop '(3 ⁿ⁻⁴))
;; '(3 ** (n .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:372:34 -> 4))


;; (superscript-operator-loop '(3 ²·⁽ⁿ⁻⁴⁾))
;; '(3 ** (2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:318:38 ·> (n .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:372:34 -> 4)))

(def (state-1-symbol p-tok ss-tok) ; partial token, sub string tokens

     ;; return case
     (when (string-null? ss-tok)
       ;;(display "superscript-parser : state-1-symbol : return case") (newline)
       ;; a symbol as in the other cases (followed by an operator) : the syntax object was written #<syntax z> by the
       ;; converter (not readable) , its type was not found by the annotation , and it does not make the syntax
       ;; transformers work ({n ᶻ} in a $nfx$ expanded as a macro : z unbound with the syntax or with the symbol)
       #;(return (cons (datum->syntax #'nfx #; #'$nfx$ (string->symbol p-tok)) ; seems we can put anything as lexical context
		     '())) ; create the NIL of list expression
       (return (cons (string->symbol p-tok)
		     '()))) ; create the NIL of list expression
     
     
     (define c (first-string ss-tok)) ; char
     (define nxt (rest-string ss-tok)) ; string

   
     ;; test for decimal separator . or digit or letter
     (when (or (char=? c #\.)
	       (char-numeric? c)
	       (char-alphabetic? c))
       (return (state-1-symbol (string-append p-tok (string c))
			       nxt))) ; recall procedure with 2 arguments : token and rest of string where to decode other tokens


      ;; operators
     ;; -> state 0 todo: factorize code (see Racket discourse question about syntax)
     (cond ((char=? c #\+) ; found an operator
	    
	    (return (cons (string->symbol p-tok) ; create symbol
			  (cons '+
				(state-0-syntax-analysis nxt)))))
	   
	   ((char=? c #\-) ; found an operator
	    (return (cons (string->symbol p-tok) ; create symbol
			  (cons '-
				(state-0-syntax-analysis nxt)))))
	   
	   ((char=? c #\/) ; found an operator
	    (return (cons (string->symbol p-tok) ; create symbol
			  (cons '/
				(state-0-syntax-analysis nxt)))))
		
	   ((char=? c #\*) ; found an operator
	    (return (cons (string->symbol p-tok) ; create symbol
			  (cons '*
				(state-0-syntax-analysis nxt)))))
	   
	   ((char=? c #\·) ; found an operator
	    (return (cons (string->symbol p-tok) ; create symbol
			  (cons '·
				(state-0-syntax-analysis nxt))))))

         
     (error "superscript-parser : state 1 symbol : unknow case of character to handle in those expressions : " (list p-tok c nxt))

     ) ; end definition


;; (parse-superscript-token "3-2i")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:281:38 -> 0+2i)

(def (state-3-pure-imaginary-number p-tok ss-tok) ; partial token, sub string tokens

     ;; return case
     (when (string-null? ss-tok)
       (define z (string->number (string-append "+" p-tok))) ; partial token is completed, it should be a pure imaginary number
       (if z
	   (return (cons z
			 (state-0-syntax-analysis ss-tok))) ; continue the syntax analysis
	   (error "Error parsing superscript : state-3-pure-imaginary-number : bad format of string that can not be converted in number: " p-tok)))

     (define c (first-string ss-tok)) ; char
     (define nxt (rest-string ss-tok)) ; string

     ;; symbol
     (when (or (char-alphabetic? c)
	       (char-numeric? c))
       (return (state-1-symbol (string-append p-tok (string c))
			       nxt)))

     ;; decimal separator
     (when (char=? c #\.)
       (error "superscript-parser : state 3 pure imaginary number : unauthorized decimal separator in those expressions : " (list p-tok c nxt)))


     ;; operators
     ;; -> state 0 todo: factorize code (see Racket discourse question about syntax)
     (cond ((char=? c #\+) ; found an operator
	    (define z (string->number (string-append "+" p-tok))) ; partial token is completed, it should be a pure imaginary number
	    (if z
		(return (cons z
			      (cons '+
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
	   ((char=? c #\-) ; found an operator
	    (define z (string->number (string-append "+" p-tok))) ; partial token is completed, it should be a pure imaginary number
	    (if z
		(return (cons z
			      (cons '-
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
	   ((char=? c #\/) ; found an operator
	    (define z (string->number (string-append "+" p-tok))) ; partial token is completed, it should be a pure imaginary number
	    (if z
		(return (cons z
			      (cons '/
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
		
	   ((char=? c #\*) ; found an operator
	    (define z (string->number (string-append "+" p-tok))) ; partial token is completed, it should be a pure imaginary number
	    (if z
		(return (cons z
			      (cons '*
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok)))
     	
	   ((char=? c #\·) ; found an operator
	    (define z (string->number (string-append "+" p-tok))) ; partial token is completed, it should be a pure imaginary number
	    (if z
		(return (cons z
			      (cons '·
				    (state-0-syntax-analysis nxt))))
		(error "Error parsing superscript : state-2-number : bad format of string that can not be converted in number: " p-tok))))
	 
     
     
     (error "superscript-parser : state 3 pure imaginary number : unknow case of character to handle in those expressions : " (list p-tok c nxt)))

     


(export
	superscript-operator-loop
	parse-superscript-token)
 ; end module


;; (parse-superscript-token "3*(4-5)-2+3i")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:201:28 *> (4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> 5) .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> 2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:189:28 +> 0+3i)

;; (parse-superscript-token "3*(4-(5-2))-2+3i")
;; '(3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:201:28 *> (4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> (5 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> 2)) .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:193:28 -> 2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:189:28 +> 0+3i)


;; (parse-superscript-token "+-3*(4-(5-2))-2+3i")
;; '(.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:187:24 +> .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:191:24 -> 3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:292:38 *> (4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:275:38 -> (5 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:275:38 -> 2)) .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:191:24 -> 2 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:267:38 +> 0+3i)


;; (parse-superscript-token "+-3*(4-(5-2))/-2")
;; '(.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:187:24 +> .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:191:24 -> 3 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:304:38 *> (4 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:287:38 -> (5 .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:287:38 -> 2)) .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:195:24 /> .#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/superscript-parser.rkt:191:24 -> 2)


;; {2 ⁴}


;; {2 ⁴ ²}
;; 65536

;; {- 2 ³}
;; -8

;; {3 * - 2 ⁴ ² + 1} 
;; -196607

;; (define nn 3)
;; {3 ²·⁽ⁿⁿ⁻⁴⁾}
;; 1/9


;; (define n 3)
;; {3 ⁻²·⁽ⁿ⁻⁴⁾}
;; 9


;;(define (foo) (define n 3) {3 ⁻²·⁽ⁿ⁻⁴⁾})
;;(define (foo) (define n 3) (** 3 (- (· 2 (- n 4)))))
;;#<eof>
;;(foo)
;; 9
