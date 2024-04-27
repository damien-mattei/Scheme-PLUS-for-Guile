;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification and optimisation for Scheme implementations (Racket,...) and Scheme+ by Damien Mattei



(define nfx-optim #t)

(define slice-optim #t)

;; library procedures and macro
(define insert cons)

;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))


  ; ------------------------------
  ; Curly-infix support procedures
  ; ------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (define (even-and-op-prefix? op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f)
      ((not (equal? op (car lyst))) #f) ; fail - operators not the same
      ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
      (#t   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ; Return alternating parameters in a lyst (1st, 3rd, 5th, etc.)
  (define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
      lyst
      (cons (car lyst) (alternating-parameters (cddr lyst)))))



  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
(define (transform-mixed-infix lyst)
  ;;(display "lyst=") (display lyst) (newline)
  
  (if nfx-optim
      (begin
	(unless (infix? lyst) (error "ERROR: expression is not in infix notation: " lyst))
	(let ((e0 (!0 infix-operators-lst-for-parser lyst)))
	  ;;(display "!0 result = ") (display e0) (newline)
	  (let ((na (n-arity e0)))
	    ;;(display "na =") (display na) (newline)
	    na)))
      (cons '$nfx$ lyst)))



  ; Given curly-infix lyst, map it to its final internal format.
  (define (process-curly lyst)
    (cond
     ((not (pair? lyst)) lyst) ; E.G., map {} to ().
     ((null? (cdr lyst)) ; Map {a} to a.
       (car lyst))
     ((and (pair? (cdr lyst)) (null? (cddr lyst))) ; Map {a b} to (a b).
       lyst)
     ((simple-infix-list? lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
       (cons (cadr lyst) (alternating-parameters lyst)))
     (#t  (transform-mixed-infix lyst))))



;; usefull procedures and macro for the next part of code
(define (then=? arg)
  (or (equal? arg 'then) (equal? arg 'THEN)))

(define (else=? arg)
  (or (equal? arg 'else) (equal? arg 'ELSE)))


;; > (if #f else 3)
;; 3
;; > (if #t else 3)
;; > (if #t 2 else 3)
;; 2
;; > (if #t then 2 else 3)
;; 2
;; > (if #f then 2 else 3)
;; 3
;; > (if #f then 1 2 else 3 4)
;; 4
;; > (if #t then 1 2 else 3 4)
;; 2
;; > (if #t 1 2 3)
;; 3
;; > (if #t then 1 2 else 3 4 then 5)
;; . . SRFI-105.rkt:181:17: if: then after else near : '(then 5)
;; > (if #t then 1 2 else 3 4 else 5)
;; . . SRFI-105.rkt:181:17: if: 2 else inside near: '(else 5)
;; > (if #t else 1 2 then 3 4)
;; . . SRFI-105.rkt:181:17: if: then after else near : '(then 3 4)
;; > (if #t then 1 2 then 3 4)
;; . . SRFI-105.rkt:181:17: if: 2 then inside near: '(then 3 4)
(define (call-parse-if-args Largs) ; Largs = (test e1 ...)

  ;;(display "Largs=") (display Largs) (newline)
  (define lenL (length Largs))

  (when (< lenL 2)
	(error "if: too few arguments:" Largs))

  (define test (car Largs))
  (define e1 (cadr Largs))

  ; deal with the old 2 args 'if' but modified
  (condx ((and (= lenL 2) (then=? e1))
	  (error "if: syntax error,found (if test then) only: near " Largs))
	 ((and (= lenL 2) (else=? e1))
	  (error "if: syntax error,found (if test else) only: near " Largs))
	 ((= lenL 2) `(when ,test ,e1)) ; (if test e1)
	 (exec (define e2 (third Largs)))
	 ((and (= lenL 3) (then=? e1)) `(when ,test ; (if test then e2)
					      ,e2))
	 ((and (= lenL 3) (else=? e1)) `(unless ,test ; (if test else e2)
						,e2))
	 ((= lenL 3) `(if ,test
			  ,e1
			  ,e2))

	 (else
	  
	  (define L-then '())
	  (define L-else '())
	  (define cpt-then 0)
	  (define cpt-else 0)
	  			   
	  (define (parse-if-args L)
	    
	    (condx ((null? L) (set! L-then (reverse L-then))
		              (set! L-else (reverse L-else)))
		   
		   (exec (define ec (car L))
			 (define rstL (cdr L)))
			 		   
		   ((then=? ec) (when (= cpt-else 1)
				      (error "if: then after else near :" L))
		                (when (= cpt-then 1)
				      (error "if: 2 then inside near:" L))
		                (set! cpt-then (+ 1 cpt-then))
		                (parse-if-args rstL)) ; recurse
		   
		   ((else=? ec) (when (= cpt-else 1)
				      (error "if: 2 else inside near:" L))
		                (set! cpt-else (+ 1 cpt-else))
		                (parse-if-args rstL)) ; recurse

		   
		   ((and (>= cpt-then 1) (= cpt-else 0)) (insert-set! ec L-then)
		                                         (parse-if-args rstL)) ; recurse

		   
		   ((>= cpt-else 1) (insert-set! ec L-else)
		                    (parse-if-args rstL))  ; recurse
		   
		   (else ; start with 'then' directives but without 'then' keyword !
		    ;; i allow this syntax but this is dangerous:  risk of confusion with regular scheme syntax
		    
		    (insert-set! ec L-then)
		    
		    (set! cpt-then 1)
		    (parse-if-args rstL)))) ; recurse
	    
	    (define Lr (cdr Largs)) ; list of arguments of 'if' without the test
						    
	    (parse-if-args Lr) ; call the parsing of arguments
	    
	    (cond ((null? L-then) `(unless ,test
					   ,@L-else))
		  ((null? L-else) `(when ,test
					 ,@L-then))
		  (else `(if ,test
			     (let ()
			       ,@L-then)
			     (let ()
			       ,@L-else)))))))




  ; ------------------------------------------------
  ; Key procedures to implement neoteric-expressions
  ; ------------------------------------------------

  ; Read the "inside" of a list until its matching stop-char, returning list.
  ; stop-char needs to be closing paren, closing bracket, or closing brace.
  ; This is like read-delimited-list of Common Lisp.
  ; This implements a useful extension: (. b) returns b.
  (define (my-read-delimited-list my-read stop-char port)
    (let*
      ((c   (peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") '())
        ((eqv? c #\;)
          (consume-to-eol port)
          (my-read-delimited-list my-read stop-char port))
        ((my-char-whitespace? c)
          (read-char port)
          (my-read-delimited-list my-read stop-char port))
        ((char=? c stop-char)
          (read-char port)
          '())
        ((or (eq? c #\)) (eq? c #\]) (eq? c #\}))
          (read-char port)
          (read-error "Bad closing character"))
	
        (#t
	 (let ((datum (my-read port)))
	   (cond
	    ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
	    ((eq? datum (string->symbol (string #\.))) ;; only this one works
	     (let ((datum2 (my-read port)))
	       (consume-whitespace port)
	       (cond
		((eof-object? datum2)
		 (read-error "Early eof in (... .)\n")
		 '())
		((not (eqv? (peek-char port) stop-char))
		 (read-error "Bad closing character after . datum"))
		(#t
		 (read-char port)
		 datum2))))
	    
	    (#t
	     ;; here we get a symbolic scheme expression
	     (let ((datum2 (cons datum
				 (my-read-delimited-list my-read stop-char port))))

	       (when (and (list? datum2)
			  (not (null? datum2))
			  (equal? (car datum2) 'if))
		     
		     (define datum3 (call-parse-if-args (cdr datum2)))
		     
		     (set! datum2 datum3))
		
	       datum2))))))))

  

(define (parser-$bracket-apply$next-arguments port prefix)
  ;; create ($bracket-apply$next container args1 args2 ...)
  `($bracket-apply$next ,prefix ;; = container (vector,array,hash table ....)
			,@(optimizer-parse-square-brackets-arguments (my-read-delimited-list neoteric-read-real #\] port))))




  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
  (define (neoteric-process-tail port prefix)
      (let* ((c (peek-char port)))
        (cond
          ((eof-object? c) prefix)
          ((char=? c #\( ) ; Implement f(x)
            (read-char port)
            (neoteric-process-tail port
				   (cons prefix (my-read-delimited-list neoteric-read-real #\) port))))

	   ((char=? c #\[ )  ; Implement f[x]
	   (read-char port)
	   (if slice-optim
	       
	       (neoteric-process-tail port
				      (parser-$bracket-apply$next-arguments port prefix))
	       
	       (neoteric-process-tail port
                  (cons '$bracket-apply$
	   		(cons prefix
	   		      (my-read-delimited-list neoteric-read-real #\] port))))))
	  
          ((char=? c #\{ )  ; Implement f{x}
            (read-char port)
            (neoteric-process-tail port
              (let ((tail (process-curly
                      (my-read-delimited-list neoteric-read-real #\} port))))
                (if (eqv? tail '())
                  (list prefix) ; Map f{} to (f), not (f ()).
                  (list prefix tail)))))
          (#t prefix))))


  ; To implement neoteric-expressions, modify the reader so
  ; that [] and {} are also delimiters, and make the reader do this:
  ; (let* ((prefix
  ;           read-expression-as-usual
  ;       ))
  ;   (if (eof-object? prefix)
  ;     prefix
  ;     (neoteric-process-tail port prefix)))

  ; Modify the main reader so that [] and {} are also delimiters, and so
  ; that when #\{ is detected, read using my-read-delimited-list
  ; any list from that port until its matching #\}, then process
  ; that list with "process-curly", like this:
  ;   (process-curly (my-read-delimited-list #\} port))




; ------------------------------------------------
  ; Demo procedures to implement curly-infix and neoteric readers
  ; ------------------------------------------------

  ; This implements an entire reader, as a demonstration, but if you can
  ; update your existing reader you should just update that instead.
  ; This is a simple R5RS reader, with a few minor (common) extensions.
  ; The "my-read" is called if it has to recurse.
  (define (underlying-read my-read port)
    (let* ((c (peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\;)
          (consume-to-eol port)
          (my-read port))
        ((my-char-whitespace? c)
          (read-char port)
          (my-read port))
        ((char=? c #\( )
          (read-char port)
          (my-read-delimited-list my-read #\) port))

        ((char=? c #\[ )

	  ;;(default-scheme-read port)) ;; this convert [ ... ] in ($bracket-list$ ...) in Kawa at least allowing Kawa special expressions such as: [1 <: 7]

          (read-char port)
          (my-read-delimited-list my-read #\] port))

	((char=? c #\{ )
          (read-char port)
          (process-curly
            (my-read-delimited-list neoteric-read-real #\} port)))
        ; Handle missing (, [, { :
        ((char=? c #\) )
          (read-char port)
          (read-error "Closing parenthesis without opening")
          (my-read port))
        ((char=? c #\] )
          (read-char port)
          (read-error "Closing bracket without opening")
          (my-read port))
        ((char=? c #\} )
          (read-char port)
          (read-error "Closing brace without opening")
          (my-read port))
        ((char=? c #\") ; Strings are delimited by ", so can call directly
          (default-scheme-read port))
        ((char=? c #\')
          (read-char port)
          (list 'quote (my-read port)))
        ((char=? c #\`)
          (read-char port)
          (list 'quasiquote (my-read port)))
        ((char=? c #\,)
          (read-char port)
            (cond
              ((char=? #\@ (peek-char port))
                (read-char port)
                (list 'unquote-splicing (my-read port)))
              (#t
                (list 'unquote (my-read port)))))
        ((ismember? c digits) ; Initial digit.
          (read-number port '()))
        ((char=? c #\#) (process-sharp my-read port))
        ((char=? c #\.) (process-period port))
        ((or (char=? c #\+) (char=? c #\-))  ; Initial + or -
          (read-char port)
          (if (ismember? (peek-char port) digits)
            (read-number port (list c))
            (string->symbol (fold-case-maybe port
              (list->string (cons c
                (read-until-delim port neoteric-delimiters)))))))
        (#t ; Nothing else.  Must be a symbol start.
          (string->symbol (fold-case-maybe port
            (list->string
              (read-until-delim port neoteric-delimiters))))))))

  (define (curly-infix-read-real port)
    (underlying-read curly-infix-read-real port))

  (define (curly-infix-read . port)
    (if (null? port)
      (curly-infix-read-real (current-input-port))
      (curly-infix-read-real (car port))))

  ; Here's a real neoteric reader.
  ; The key part is that it implements [] and {} as delimiters, and
  ; after it reads in some datum (the "prefix"), it calls
  ; neoteric-process-tail to see if there's a "tail".
  (define (neoteric-read-real port)
    (let* ((prefix (underlying-read neoteric-read-real port)))
      (if (eof-object? prefix)
        prefix
        (neoteric-process-tail port prefix))))

  (define (neoteric-read . port)
    (if (null? port)
      (neoteric-read-real (current-input-port))
      (neoteric-read-real (car port))))


  ; ------------------
  ; Support procedures
  ; ------------------

  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (define linefeed (integer->char #x000A))        ; #\newline aka \n.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define tab (integer->char #x0009))
  (define line-tab (integer->char #x000b))
  (define form-feed (integer->char #x000c))
  (define line-ending-chars (list linefeed carriage-return))
  (define whitespace-chars
    (list tab linefeed line-tab form-feed carriage-return #\space))

  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; we always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define foldcase-default #f)

  ; Returns a true value (not necessarily #t) if char ends a line.
  (define (char-line-ending? char) (memq char line-ending-chars))

  ; Returns true if item is member of lyst, else false.
  (define (ismember? item lyst)
     (pair? (member item lyst)))

  ; Create own version, in case underlying implementation omits some.
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (ismember? c whitespace-chars)))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; foldcase-default configuration value when processing symbols.
  ; The "string-foldcase" procedure isn't everywhere,
  ; so we use "string-downcase".
  (define (fold-case-maybe port s)
    (if foldcase-default
      (string-downcase s)
      s))

  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (peek-char port)))
      (cond
        ((not (or (eof-object? c)
                  (char-line-ending? c)))
          (read-char port)
          (consume-to-eol port)))))

  (define (consume-whitespace port)
    (let ((char (peek-char port)))
      (cond
        ((eof-object? char) char)
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (read-char port)
          (consume-whitespace port)))))

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" has a delimiter.  However, R5RS does not, and
  ; R7RS probably will not - http://trac.sacrideo.us/wg/wiki/WG1Ballot3Results
  ; shows a strong vote AGAINST "#" being a delimiter.
  ; Having the "#" as a delimiter means that you cannot have "#" embedded
  ; in a symbol name, which hurts backwards compatibility, and it also
  ; breaks implementations like Chicken (has many such identifiers) and
  ; Gambit (which uses this as a namespace separator).
  ; Thus, this list does NOT have "#" as a delimiter, contravening R6RS
  ; (but consistent with R5RS, probably R7RS, and several implementations).
  ; Also - R7RS draft 6 has "|" as delimiter, but we currently don't.
  (define neoteric-delimiters
    (append (list #\( #\) #\[ #\] #\{ #\}  ; Add [] {}
		  #\" #\;)                 ; Could add #\# or #\|
	    whitespace-chars))

  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (peek-char port)))
      (cond
         ((eof-object? c) '())
         ((ismember? c delims) '())
         (#t (cons (read-char port) (read-until-delim port delims))))))

  (define (read-error message)
    (display "Error: ")
    (display message)
    (display "\n")
    (error message)
    '())

  (define (read-number port starting-lyst)
    (string->number (list->string
      (append starting-lyst
        (read-until-delim port neoteric-delimiters)))))

  ; detect #| or |#
  (define (nest-comment port)
    (let ((c (read-char port)))
      (cond
        ((eof-object? c))
        ((char=? c #\|)
          (let ((c2 (peek-char port)))
            (if (char=? c2 #\#)
                (read-char port)
                (nest-comment port))))
        ((char=? c #\#)
          (let ((c2 (peek-char port)))
            (when (char=? c2 #\|)
                (begin
                  (read-char port)
                  (nest-comment port)))
            (nest-comment port)))
        (#t
          (nest-comment port)))))

  (define (process-sharp my-read port)
    ; We've peeked a # character.  Returns what it represents.
    (read-char port) ; Remove #
    (cond
      ((eof-object? (peek-char port)) (peek-char port)) ; If eof, return eof.
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port)))
          (cond
            ((char-ci=? c #\t)  #t)
            ((char-ci=? c #\f)  #f)
            ((ismember? c '(#\i #\e #\b #\o #\d #\x
                            #\I #\E #\B #\O #\D #\X))
              (read-number port (list #\# (char-downcase c))))
            ((char=? c #\( )  ; Vector.
	     (list->vector (my-read-delimited-list my-read #\) port)))

	    ;; hash table : #hash(("a" . 1) ("b" . 20)) support to write...

	    ((char=? c #\\) (process-char port))
            ; This supports SRFI-30 #|...|#
            ((char=? c #\|) (nest-comment port) (my-read port))
            ; If #!xyz, consume xyz and recurse.
            ; In a real reader, consider handling "#! whitespace" per SRFI-22,
            ; and consider "#!" followed by / or . as a comment until "!#".
            ((char=? c #\!) (my-read port) (my-read port))

	    ;; > (+ 1 #;2 3)
	    ;; 4
	    ;; > (+ 1 #;(+ 1 2) 3)
	    ;; 4
	    ;; > (+ 1 #;{1 + 2} 3)
	    ;; 4
	    ((char=? c #\;) ;(read-error "SRFI-105 REPL : Unsupported #; extension"))
	     (my-read port) (my-read port))

	    ;; this remove #lang racket on anything else but i do not want it to be like that
	    ;; removing any line starting with #l is not the good way
	    ;; i should instead skip the first #lang ... line of the input file
	    ;; ((char=? c #\l) ;; #lang ...
	    ;;  (consume-to-eol port)
	    ;;  (my-read port))


	    ;; read #:blabla
	    ((char=? c #\:) ;; added by D.MATTEI for Guile to read Key-words beginning by #: (not symbols)
	     (symbol->keyword
	      (list->symbol
	       (read-until-delim port neoteric-delimiters))))
	    
	    ;; origingal code:
	    ;; (list->string
	    ;;  (append (list #\# #\:)
	    ;; 	     (read-until-delim port neoteric-delimiters))))



	    
	    ;; read #'blabla ,deal with syntax objects
	    ;;((char=? c #\') (list 'syntax (curly-infix-read port)))
	    ((char=? c #\') (list 'syntax (my-read port)))
	    ;; deal syntax with backquote, splicing,...
	    ((char=? c #\`) (list 'quasisyntax (my-read port)))
	    ((char=? c #\,) (if (char=? (peek-char port) #\@)
				(begin
				  (read-char port)
				  (list 'unsyntax-splicing (my-read port)))
				(list 'quasisyntax (my-read port))))
	    (#t (read-error (string-append "SRFI-105 REPL :"
					   "Unsupported # extension"
					   " unsupported character causing this message is character:"
					   (string c)))))))))



  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (read-char port) ; Remove .
    (let ((c (peek-char port)))
      (cond ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
       ((eof-object? c) (string->symbol (string #\.)))  ;; only this one works with Racket Scheme
        ;;((eof-object? c) '.) ; period eof; return period. ;; do not works with Racket Scheme
       ;;((eof-object? c) 'period) ;; this one annihilate the processing using dummy 'period !
        ((ismember? c digits)
          (read-number port (list #\.)))  ; period digit - it's a number.
        (#t
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))

  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (peek-char port)) (peek-char port))
      (#t
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (#t
              (let ((rest-string (list->string (cons c rest))))
                (cond
                  ; Implement R6RS character names, see R6RS section 4.2.6.
                  ; As an extension, we will ALWAYS accept character names
                  ; of any case, no matter what the case-folding value is.
                  ((string-ci=? rest-string "space") #\space)
                  ((string-ci=? rest-string "newline") #\newline)
                  ((string-ci=? rest-string "tab") tab)
                  ((string-ci=? rest-string "nul") (integer->char #x0000))
                  ((string-ci=? rest-string "alarm") (integer->char #x0007))
                  ((string-ci=? rest-string "backspace") (integer->char #x0008))
                  ((string-ci=? rest-string "linefeed") (integer->char #x000A))
                  ((string-ci=? rest-string "vtab") (integer->char #x000B))
                  ((string-ci=? rest-string "page") (integer->char #x000C))
                  ((string-ci=? rest-string "return") (integer->char #x000D))
                  ((string-ci=? rest-string "esc") (integer->char #x001B))
                  ((string-ci=? rest-string "delete") (integer->char #x007F))
                  ; Additional character names as extensions:
                  ((string-ci=? rest-string "ht") tab)
                  ((string-ci=? rest-string "cr") (integer->char #x000d))
                  ((string-ci=? rest-string "bs") (integer->char #x0008))
                  (#t (read-error "Invalid character name"))))))))))

;; Record the original read location, in case it's changed later:
(define default-scheme-read read)

