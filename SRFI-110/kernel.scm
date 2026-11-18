;; Translated from SRFI 110 for Racket (kernel.rkt , SRFI-110-for-Racket) for Guile.

;;; kernel.rkt
;;; Implementation of the sweet-expressions project by readable mailinglist.
;;;
;;; Copyright (C) 2005-2014 by David A. Wheeler and Alan Manuel K. Gloria
;;;
;;; This software is released as open source software under the "MIT" license:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; Warning: For portability use eqv?/memv (not eq?/memq) to compare characters.
;;; A "case" is okay since it uses "eqv?".

;;; This file includes:
;;; - Compatibility layer - macros etc. to try to make it easier to port
;;;   this code to different Scheme implementations (to deal with different
;;;   module systems, how to override read, getting position info, etc.)
;;; - Re-implementation of "read", beginning with its support procedures.
;;;   There is no standard Scheme mechanism to override just *portions*
;;;   of read, and we MUST make {} delimiters, so we have to re-implement read.
;;;   We also need to get control over # so we know which ones are comments.
;;;   If you're modifying a Scheme implementation, just use that instead.
;;; - Curly Infix (c-expression) implementation
;;; - Neoteric expression (n-expression) implementation
;;; - Sweet-expression (t-expression) implementation
;;; - Implementation of writers for curly-infix and neoteric expressions -
;;;   a "-simple" implementation, and a separate -shared/-cyclic version.
;;;   The "-simple" one is separate so that you can use just it.
;;; Note that a lot of the code is in the compatibility layer and
;;; re-implementation of "read"; implementing the new expression languages
;;; is actually pretty easy.


; -----------------------------------------------------------------------------
; Compatibility Layer
; -----------------------------------------------------------------------------
; The compatibility layer is composed of:
;
;   (readable-kernel-module-contents (exports ...) body ...)
;   - a macro that should package the given body as a module, or whatever your
;     scheme calls it (chicken eggs?), preferably with one of the following
;     names, in order of preference, depending on your Scheme's package naming
;     conventions/support
;       (readable kernel)
;       readable/kernel
;       readable-kernel
;       sweetimpl
;   - The first element after the module-contents name is a list of exported
;     procedures.  This module shall never export a macro or syntax, not even
;     in the future.
;   - If your Scheme requires module contents to be defined inside a top-level
;     module declaration (unlike Guile where module contents are declared as
;     top-level entities after the module declaration) then the other
;     procedures below should be defined inside the module context in order
;     to reduce user namespace pollution.
;
;   (my-peek-char port)
;   (my-read-char port)
;   - Performs I/O on a "port" object.
;   - The algorithm assumes that port objects have the following abilities:
;     * The port automatically keeps track of source location
;       information.  On R5RS there is no source location
;       information that can be attached to objects, so as a
;       fallback you can just ignore source location, which
;       will make debugging using sweet-expressions more
;       difficult.
;   - "port" or fake port objects are created by the make-read procedure
;     below.
;   
;   (make-read procedure)
;   - The given procedure accepts exactly 1 argument, a "fake port" that can
;     be passed to my-peek-char et al.
;   - make-read creates a new procedure that supports your Scheme's reader
;     interface.  Usually, this means making a new procedure that accepts
;     either 0 or 1 parameters, defaulting to (current-input-port).
;   - If your Scheme doesn't keep track of source location information
;     automatically with the ports, you may again need to wrap it here.
;   - If your Scheme needs a particularly magical incantation to attach
;     source information to objects, then you might need to use a weak-key
;     table in the attach-sourceinfo procedure below and then use that
;     weak-key table to perform the magical incantation.
;
;   (invoke-read read port)
;   - Accepts a read procedure, which is a (most likely built-in) procedure
;     that requires a *real* port, not a fake one.
;   - Should unwrap the fake port to a real port, then invoke the given
;     read procedure on the actual real port.
;
;   (get-sourceinfo port)
;   - Given a fake port, constructs some object (which the algorithm treats
;     as opaque) to represent the source information at the point that the
;     port is currently in.
;
;   (attach-sourceinfo pos obj)
;   - Attaches the source information pos, as constructed by get-sourceinfo,
;     to the given obj.
;   - obj can be any valid Scheme object.  If your Scheme can only track
;     source location for a subset of Scheme object types, then this procedure
;     should handle it gracefully.
;   - Returns an object with the source information attached - this can be
;     the same object, or a different object that should look-and-feel the
;     same as the passed-in object.
;   - If source information cannot be attached anyway (your Scheme doesn't
;     support attaching source information to objects), just return the
;     given object.
;
;   (replace-read-with f)
;   - Replaces your Scheme's current reader.
;   - Replace 'read and 'get-datum at the minimum.  If your Scheme
;     needs any kind of involved magic to handle load and loading
;     modules correctly, do it here.
;
;   (parse-hash no-indent-read char fake-port)
;   - a procedure that is invoked when an unrecognized, non-R5RS hash
;     character combination is encountered in the input port.
;   - this procedure is passed a "fake port", as wrapped by the
;     make-read procedure above.  You should probably use my-read-char
;     and my-peek-char in it, or at least unwrap the port (since
;     make-read does the wrapping, and you wrote make-read, we assume
;     you know how to unwrap the port).
;   - if your procedure needs to parse a datum, invoke
;     (no-indent-read fake-port).  Do NOT use any other read procedure.  The
;     no-indent-read procedure accepts exactly one parameter - the fake port
;     this procedure was passed in.
;     - no-indent-read is either a version of curly-infix-read, or a version
;       of neoteric-read; this specal version accepts only a fake port.
;       It is never a version of sweet-read.  You don't normally want to
;       call sweet-read, because sweet-read presumes that it's starting
;       at the beginning of the line, with indentation processing still
;       active.  There's no reason either must be true when processing "#".
;   - At the start of this procedure, both the # and the character
;     after it have been read in.
;   - The procedure returns one of the following:
;       #f  - the hash-character combination is invalid/not supported.
;       (normal value) - the datum has value "value".
;       (scomment ()) - the hash-character combination introduced a comment;
;             at the return of this procedure with this value, the
;             comment has been removed from the input port.
;             You can use scomment-result, which has this value.
;       (datum-commentw ()) - #; followed by whitespace.
;       (abbrev value) - this is an abbreviation for value "value"
;
;   hash-pipe-comment-nests?
;   - a Boolean value that specifies whether #|...|# comments
;     should nest.
;
;   my-string-foldcase
;   - a procedure to perform case-folding to lowercase, as mandated
;     by Unicode.  If your implementation doesn't have Unicode, define
;     this to be string-downcase.  Some implementations may also
;     interpret "string-downcase" as foldcase anyway.

;; modification for Scheme implementations (Racket,...) and Scheme+ by Damien Mattei , 2025



(define-module (SRFI-110 kernel)
  #:use-module (Scheme+ racket-compat))

  

  

  (use-modules
	(Scheme+ nfx)
	(Scheme+ condx)
	(Scheme+ alternating-parameters)
	(Scheme+ operators)
	(Scheme+ infix-with-precedence-to-prefix)
	(Scheme+ operators-list)
	((Scheme+ parser-declarations) #:select (verify-if-declaration-for-parser-then-store-it))
	((Scheme+ care-of-quotes) #:select (infix-top-level-only))
	((Scheme+ superscript) #:select (superscript-only?))
	(Scheme+ type)) ; brings ANNOTATION-ON and ANNOTATION-OFF
    
    (use-modules
	((Scheme+ source-locations) #:select (location-start location-from record-form! record-atom! reset-read-atoms! relink-read-atoms!)))
    (use-modules ((srfi srfi-4) #:select (list->u8vector))
		 ((rnrs exceptions) #:select (guard))
		 ((ice-9 rdelim) #:select (read-line))
		 ((ice-9 textual-ports) #:select (get-string-n))) ; read-string of (ice-9 rdelim) followed by unread-string loses a character
    
    


(define global-port '())
(define source "unknown")
(define global-source #f) ; the source given by Racket to the reader (a path , or something else in an editor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	
;; globals variables that can be modified by coder or by pragma
	
(define srfi-strict #f) ; enable strict compatibility with SRFI 105 that will NOT force some $nfx$ apply almost everywhere

;; the pragma BEGIN-STRICT-SRFI-105-REGION or END-STRICT-SRFI-105-REGION read alone in { } : {BEGIN-STRICT-SRFI-105-REGION}
;; the { } gives the pragma back to the enclosing list , which drops it (without this the empty { } left () in the code)
(define strict-pragma-in-curly #f)

;; partially deprecated as the new parenthesis syntax and insertion of $nfx$ create a recursive parsing with infix autodetection
;; no more used : replaced by the pragmas CARE-OF-QUOTES-ON / CARE-OF-QUOTES-OFF (Scheme+/care-of-quotes) ,
;; still defined and exported because the backups of main.rkt display it
(define care-of-quote #t) ; keep quoted expression when #t (no $nfx$ will be inserted in *quoted* curly infix expressions),
;; usefull to use symbolic expressions
;; (but makes debugging harder because quoted expression to debug will not be the same as evaluated unquoted ones)

;; DEPRECATED : let it to #f
(define use-only-syntax-transformers #f) ; use only syntax transformers: syntax transformers are used by scheme+
					; when false the parser will partially do the job of syntax transformers
					; it will do the job for expression between { } but not for 'define and 'define+
					; letting it be done by syntax transformers

;; off by default, set by the pragmas ANNOTATION-ON / ANNOTATION-OFF (see process-annotation-pragma)
;; and reset to #f by main.rkt at the beginning of each file read
(define annot-flag #f) ; activate or deactivate parsing of annotations

(define (get-annot-flag)
  annot-flag)

(define (set-annot-flag! flag)
  (set! annot-flag flag))

;; #t as soon as a pragma ANNOTATION-ON has been read in the file : the annotation pass must then run on the file
;; (the pragmas act by region, see annot+.rkt of SRFI 105), reset to #f by main.rkt at the beginning of each file read
(define annot-on-read #f)

(define (get-annot-on-read)
  annot-on-read)

(define (set-annot-on-read! flag)
  (set! annot-on-read flag))

;; the pragmas ANNOTATION-ON / ANNOTATION-OFF : datum is a datum read in the file, either an element
;; of a list (my-read-delimited-list, for example inside a module form) or a datum at the top level
;; of the file (main.rkt).
;; The pragmas act by region : they are no more swallowed, they stay in the code as markers for the annotation pass
;; (annot+.rkt of SRFI 105) that removes them. A pragma acts on the elements that follow it in the same list, until
;; the next pragma or the end of the list. annot-flag is the last pragma read (used by the REPL), annot-on-read tells
;; if the annotation pass must run on the file.
;; old comment : The annotation pass is applied when the whole file has been read, so the
;; last pragma read is the one that counts. The pragmas are swallowed : they do not remain in the code.
(define (process-annotation-pragma datum) ; return #t when datum is one of the pragmas
  (cond ((eq? datum ANNOTATION-ON)
	 (set! annot-flag #t)
	 (set! annot-on-read #t)
	 #t)
	((eq? datum ANNOTATION-OFF)
	 (set! annot-flag #f)
	 #t)
	(else #f)))

;; the value of srfi-strict written in this source code : the state at the beginning of each file
(define srfi-strict-at-start srfi-strict)

;; called at the beginning of each file : a strict region not closed by END-STRICT-SRFI-105-REGION does not go on
;; in the next file read (for example a module required by the previous one)
(define (reset-srfi-strict!)
  (set! srfi-strict srfi-strict-at-start))

;; a pragma BEGIN-STRICT-SRFI-105-REGION / END-STRICT-SRFI-105-REGION read at the REPL or at the top level of a file ,
;; return #t when datum is one of the pragmas (inside a list they are processed by the reader ; at the top level of a
;; file in sweet-expressions sweet-read has already changed the mode , the pragma is then only removed)
(define (process-strict-pragma datum)
  (cond ((eq? datum 'BEGIN-STRICT-SRFI-105-REGION)
	 (set! srfi-strict #t)
	 #t)
	((eq? datum 'END-STRICT-SRFI-105-REGION)
	 (set! srfi-strict #f)
	 #t)
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; library procedures and macro
(define insert cons)

;; insert and set 
(define-syntax insert-set!
  (syntax-rules ()
    ((_ expr var)
     (set! var (insert expr var)))))



;; procedures and global variables to deal with quoted,quasiquoted,unquoted,etc binary curly infix regions

;; Notes on the modified algorithm:
;; the original curly infix parser of SRFI 105 is written in a style which is functional recursive
;; instead of using some stack. Modifying this algorithm should have be by adding an extra parameter
;; defining the region context (quoted/unquoted) but this schema would have cause to pass
;; an extra parameter in a lot of functions with the risk of an error in coding and a lot of work.
;; I had intially and preferred to add a more complex stack but with less modifications in
;; existing code. Then the modifications are only in parts dealing with quote,quasiquote,unquote
;; unquote-splacing , sexpr,and of course process-curly but not in neoteric expressions
;; and others intermediate procedures.

;; instantiate an empty stack and provide methods (push,pop...)

;; note: the stack code is so simple that it is better inlined and hard to put in a module
(define stack '())

(define (push x) (set! stack (cons x stack)))

(define (pop)
  (when (null? stack)
    (read-error "SRFI-105-curly-infix : pop : EMPTY STACK ERROR")) 
  (define x (car stack))
  (set! stack (cdr stack))
  x)


;; definitions for forcing or releasing the parsing of quoted (pseudoquoted too) expressions

;; #t : quoted region (quote,quasiquote)
;; #f : unquoted region (unquote,unquote-splicing)
(define region-quote #f) ; intial value at launching

(define (find*quote) ; quote , quasiquote but NOT unquote
  (push region-quote) ; store the current mode on top of the stack
  (set! region-quote #t))

(define (find-unquote*)
  (push region-quote) ; store the current mode on top of the stack
  (set! region-quote #f))

(define (end-region) ; called when we find the end of a region
  (set! region-quote (pop))) ; fallback to the previous mode stored on the stack



;; definitions for testing equality to quoted regions
(define (quote-or-quasi? datum)
  (or (eq? datum 'quote)
      (eq? datum 'quasiquote)))

;; definitions for testing equality to unquoted regions and setting and storing the region modes
(define (unquote-or-splicing? datum)
  (or (eq? datum 'unquote)
      (eq? datum 'unquote-splicing)))

(define (check*quote datum)
  (if (quote-or-quasi? datum)
      (find*quote) ; return unspecified which is true
      #f))

(define (check-unquote* datum)
  (if (unquote-or-splicing? datum)
      (find-unquote*) ; return unspecified which is true
      #f))

(define (check*quote* datum)
  (or (check*quote datum)
      (check-unquote* datum)))


(define comment #f) ; use for comment removal

;; #t while the elements of a list ( ) [ ] { } are read (see my-read-delimited-list) , #f at the level of the lines of the
;; sweet-expressions : the datum comments #; and the block comments #| |# are then treated as in the original SRFI 110
(define in-list #f)

(define postfix-flag #f) ; when we find a postfix operator annotation



; On Guile 2.0, the define-module part needs to occur separately from
; the rest of the compatibility checks, unfortunately.  Sigh.

; Early setups, e.g., enable define-syntax

; Type annotations (":" ...), chicken compatible format.
; Ignored on other platforms. See: http://wiki.call-cc.org/man/4/Types
(define-syntax : (syntax-rules () ((_ . rest) (values))))
  (define-syntax no-values (syntax-rules () ((_) (values))))



; Implementation specific extension to flush output on ports.
(define-syntax flush-output-port
    (syntax-rules () ((_ port) (force-output port)))) ; not cond expression

; Special cases for those Scheme implementations which do that not
; support define-syntax.
(define-syntax readable-kernel-module-contents
      (syntax-rules ()
        ((readable-kernel-module-contents exports body ...)
          (begin body ...))))
  ; There is no standard Scheme mechanism to unread multiple characters.
  ; Therefore, the key productions and some of their supporting procedures
  ; return both the information on what ended their reading process,
  ; as well the actual value (if any) they read before whatever stopped them.
  ; That way, procedures can process the value as read, and then pass on
  ; the ending information to whatever needs it next.  This approach,
  ; which we call a "non-tokenizing" implementation, implements a tokenizer
  ; via procedure calls instead of needing a separate tokenizer.
  ; The ending information can be:
  ; - "stopper" - this is returned by productions etc. that do NOT
  ;     read past the of a line (outside of paired characters and strings).
  ;     It is 'normal if it ended normally (e.g., at end of line); else it's
  ;     'sublist-marker ($), 'group-split-marker (\\), 'collecting (<*),
  ;     'collecting-end (*>), 'scomment (special comments like #|...|#), or
  ;     'abbrevw (initial abbreviation with whitespace after it).
  ; - "new-indent" - this is returned by productions etc. that DO read
  ;     past the end of a line.  Such productions typically read the
  ;     next line's indent to determine if they should return.
  ;     If they should, they return the new indent so callers can
  ;     determine what to do next.  A "*>" should return even though its
  ;     visible indent level is length 0; we handle this by prepending
  ;     all normal indents with "^", and "*>" generates a length-0 indent
  ;     (which is thus shorter than even an indent of 0 characters).

  (define-syntax let-splitter
    (syntax-rules ()
      ((let-splitter (full first-value second-value) expr body ...)
        (let* ((full expr)
               (first-value (car full))
               (second-value (cadr full)))
               body ...))))





      ;; below only use with sweet, probably to fix
      (define (type-of x) #f)
      (define (type? x) #f)
      (define (type-printer x) (values))
      (define (type-num-slots x) 0)
      (define (slot-ref a b c) (values))
    
    ; A do-nothing.
    (define (init-sweet) (no-values))

    ; We use my-* procedures so that the
    ; "port" automatically keeps track of source position.
    ; On Schemes where that is not true (e.g. Racket, where
    ; source information is passed into a reader and the
    ; reader is supposed to update it by itself) we can wrap
    ; the port with the source information, and update that
    ; source information in the my-* procedures.

    (define (my-peek-char port) (peek-char port))
    (define (my-read-char port) (read-char port))

    ; this wrapper procedure wraps a reader procedure
    ; that accepts a "fake" port above, and converts
    ; it to an R5RS-compatible procedure.  On Schemes
    ; which support source-information annotation,
    ; but use a different way of annotating
    ; source-information from Guile, this procedure
    ; should also probably perform that attachment
    ; on exit from the given inner procedure.

    (define (make-read f)
       (lambda args
         (let ((real-port (if (null? args)
                              (current-input-port)
                              (car args))))
	   (set! global-port real-port)
	   (when (and (not (null? args))
                 (not (null? (cdr args)))
                 (string? (cadr args)))
             (set! source (cadr args))) ; source filename
	   (when (and (not (null? args))
		      (not (null? (cdr args))))
	     (set! global-source (cadr args))) ; the source given by Racket , as it is , for the location of the read errors
           #;(f real-port)
	   (reset-read-atoms!)
	   (relink-read-atoms! (f real-port)))))

    ; invoke the given "actual" reader, most likely
    ; the builtin one, but make sure to unwrap any
    ; fake ports.
    (define (invoke-read read port)
      (read port))
    ; R5RS doesn't have any method of extracting
    ; or attaching source location information.
    (define (get-sourceinfo port) (location-start port))
      (define (attach-sourceinfo pos obj)
	(let ((location (location-from pos global-port)))
	  (if (pair? obj)
	      (record-form! obj location)
	      (record-atom! obj location))
	  obj))

    ; R5RS has no hash extensions
    (define (parse-hash no-indent-read char fake-port) #f)

    ; Hash-pipe comment is not in R5RS, but support
    ; it as an extension, and make them nest.
    (define hash-pipe-comment-nests? #t)

    ; If your Scheme supports "string-foldcase", use that instead of
    ; string-downcase:
    (define (my-string-foldcase s)
      (string-downcase s))

    ; Somehow get SRFI-69 and SRFI-1
    (use-modules
	(srfi srfi-1)
	((srfi srfi-69) #:hide (make-hash-table hash-table?))
	((srfi srfi-69) #:select ((make-hash-table . srfi-69-make-hash-table) (hash-table? . srfi-69-hash-table?))))


(define (replace-read-with f)
    (set! mutable-read f) ; mutable-read is a copy of read, so it is mutable, read can not be modified in Racket
    ;;#f
    )

; keyword creation

; -----------------------------------------------------------------------------
; Module declaration and useful utilities
; -----------------------------------------------------------------------------
(readable-kernel-module-contents
  ; exported procedures
  (; tier read procedures
   curly-infix-read neoteric-read sweet-read
   ; set read mode
   set-read-mode
   ; replacing the reader
   replace-read restore-traditional-read
   enable-curly-infix enable-neoteric enable-sweet
   ; Various writers.
   curly-write-simple neoteric-write-simple
   curly-write curly-write-cyclic curly-write-shared
   neoteric-write neoteric-write-cyclic neoteric-write-shared)


  ; Should we fold case of symbols by default?
  ; #f means case-sensitive (R6RS); #t means case-insensitive (R5RS).
  ; Here we'll set it to be case-sensitive, which is consistent with R6RS
  ; and guile, but NOT with R5RS.  Most people won't notice, I
  ; _like_ case-sensitivity, and the latest spec is case-sensitive,
  ; so let's start with #f (case-sensitive).
  ; This doesn't affect character names; as an extension,
  ; We always accept arbitrary case for them, e.g., #\newline or #\NEWLINE.
  (define is-foldcase #f)

  (define keyword-prefix #f) ; Accept ":STUFF" as keyword?  
  (define keyword-suffix #f) ; Accept "STUFF:" as keyword?  

  ; special tag to denote comment return from hash-processing

  ; Define the whitespace characters, in relatively portable ways
  ; Presumes ASCII, Latin-1, Unicode or similar.
  (define tab (integer->char #x0009))             ; #\ht aka \t.
  (define linefeed (integer->char #x000A))        ; #\newline aka \n. FORCE it.
  (define carriage-return (integer->char #x000D)) ; \r.
  (define line-tab (integer->char #x000D))
  (define form-feed (integer->char #x000C))
  (define vertical-tab (integer->char #x000b))
  (define space '#\space)

  ; Period symbol.  A symbol starting with "." is not
  ; validly readable in R5RS, R6RS, R7RS (except for
  ; the peculiar identifier "..."); with the
  ; R6RS and R7RS the print representation of
  ; string->symbol(".") should be |.| .  However, as an extension, this
  ; Scheme reader accepts "." as a valid identifier initial character,
  ; in part because guile permits it.
  ; For portability, use this formulation instead of '. in this
  ; implementation so other implementations don't balk at it.
  (define period-symbol (string->symbol "."))

  (define line-ending-chars (list linefeed carriage-return))

  ; This definition of whitespace chars is derived from R6RS section 4.2.1.
  ; R6RS doesn't explicitly list the #\space character, be sure to include!
  (define whitespace-chars-ascii
     (list tab linefeed line-tab form-feed carriage-return #\space))
  ; Note that we are NOT trying to support all Unicode whitespace chars.
  (define whitespace-chars whitespace-chars-ascii)

  ; If #t, handle some constructs so we can read and print as Common Lisp.
  (define common-lisp #f)

  ; If #t, return |...| symbols as-is, including the vertical bars.
  (define literal-barred-symbol #f)

  ; Returns a true value (not necessarily #t)
  (: char-line-ending? (* --> boolean))
  (define (char-line-ending? char) (memv char line-ending-chars))

  ; Create own version, in case underlying implementation omits some.
  (: my-char-whitespace? (char --> boolean))
  (define (my-char-whitespace? c)
    (or (char-whitespace? c) (memv c whitespace-chars)))

  ; Consume an end-of-line sequence, ('\r' '\n'? | '\n'), and nothing else.
  ; Don't try to handle reversed \n\r (LFCR); doing so will make interactive
  ; guile use annoying (EOF won't be correctly detected) due to a guile bug
  ; (in guile before version 2.0.8, peek-char incorrectly
  ; *consumes* EOF instead of just peeking).
  (: consume-end-of-line (input-port -> *))
  (define (consume-end-of-line port)
    (let ((c (my-peek-char port)))
      (cond
        ((eqv? c carriage-return)
          (my-read-char port)
          (when (eqv? (my-peek-char port) linefeed)
              (my-read-char port)))
        ((eqv? c linefeed)
         (my-read-char port)))))

  (: consume-to-eol (input-port -> undefined))
  (define (consume-to-eol port)
    ; Consume every non-eol character in the current line.
    ; End on EOF or end-of-line char.
    ; Do NOT consume the end-of-line character(s).
    (let ((c (my-peek-char port)))
      (cond
        ((and (not (eof-object? c)) (not (char-line-ending? c)))
          (my-read-char port)
          (consume-to-eol port)))))

  (: consume-to-whitespace (input-port -> (or eof null)))
  (define (consume-to-whitespace port)
    ; Consume to whitespace
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((my-char-whitespace? c)
          '())
        (else
          (my-read-char port)
          (consume-to-whitespace port)))))

  (define debugger-output #f)
  ; Quick utility for debugging.  Display marker, show data, return data.
  (define (debug-show marker data)
    (cond
      (debugger-output
        (display "DEBUG: ")
        (display marker)
        (display " = ")
        (write data)
        (display "\n")))
    data)

  (: my-read-delimited-list (:reader-proc: char input-port -> *))
  (define (my-read-delimited-list my-read stop-char port)
    ; Read the "inside" of a list until its matching stop-char, returning list.
    ; stop-char needs to be closing paren, closing bracket, or closing brace.
    ; This is like read-delimited-list of Common Lisp, but it
    ; calls the specified reader instead.
    ; This implements a useful extension: (. b) returns b. This is
    ; important as an escape for indented expressions, e.g., (. \\)
    (consume-whitespace port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port)))
      (cond
        ((eof-object? c) (read-error "EOF in middle of list") c)
        ((char=? c stop-char)
          (my-read-char port)
          (attach-sourceinfo pos '()))
        ((memv c '(#\) #\] #\}))  (read-error "Bad closing character") c)
	
        (else
         (let* (#;(datum (my-read port)) ; should read a token
		(datum (let ((saved in-list)) ; should read a token , inside a list
			 (set! in-list #t)
			 (let ((d (my-read port)))
			   (set! in-list saved)
			   d)))
		(strict-srfi-105-pragma #f)
		(annotation-pragma #f) ; ANNOTATION-ON / ANNOTATION-OFF found : drop the datum
		(q-reg (check*quote* datum))) ; *quote* (i mean backquote,quasiquote ...) region and also set a local flag for entering a critical region

	   ;;(when q-reg
	   ;;(display "datum=") (display datum)(newline)) ; datum would contain quote,quasiquote,unquote,unquote-splicing,etc... push

	   ;; for test only
	   ;; (when (eq? datum 'newline)
	   ;;   (read-error "newline test passed"))
	   
	   (when (eq? datum 'BEGIN-STRICT-SRFI-105-REGION) ; note: eq? is ok but equal? could be better
	     (set! strict-srfi-105-pragma #t)
	     (set! srfi-strict #t)
	     (when (eqv? stop-char #\}) ; {BEGIN-STRICT-SRFI-105-REGION}
	       (set! strict-pragma-in-curly datum)))

	   (when (eq? datum 'END-STRICT-SRFI-105-REGION)
	     (set! strict-srfi-105-pragma #t)
	     (set! srfi-strict #f)
	     (when (eqv? stop-char #\}) ; {END-STRICT-SRFI-105-REGION}
	       (set! strict-pragma-in-curly datum)))

	   #;(set! annotation-pragma (process-annotation-pragma datum)) ; pragmas ANNOTATION-ON / ANNOTATION-OFF , swallowed like the strict pragmas
	   (process-annotation-pragma datum) ; pragmas ANNOTATION-ON / ANNOTATION-OFF : kept in the list, as markers of the regions for the annotation pass (annot+.rkt of SRFI 105)

           (cond
	    ;; here we got chars ... (not symbols)
	    ;; processing period . is important for functions with variable numbers of parameters: (fct arg1 . restargs)
               ((and (eq? datum period-symbol) (char=? c #\.))
                 (let ((datum2 (my-read port)))
                   (consume-whitespace port) ; reading white space between . restargs ?
                   (cond
                     ((eof-object? datum2)
                      (read-error "Early eof in (... .)")
                      '())
                     ((not (eqv? (my-peek-char port) stop-char))
                      (read-error "Bad closing character after . datum")
                      datum2)
                     (else
                       (my-read-char port)
                       datum2))))
               (else
		;; original code commented:
                ;; (attach-sourceinfo pos
                ;;   (cons datum
                ;;     (my-read-delimited-list my-read stop-char port))))))))))
		;; here we get the symbolic scheme expression (but it is constructed recursively,only at the end we get the correct full expression)


					; also test for comment flag !!!
		; postfix operators , procedures and macros defined or typed , data typed : what the infix parser must know
		(unless comment
		  (verify-if-declaration-for-parser-then-store-it datum))
		#;(when (and (list? datum)
			   (not (null? datum))
			   (eq? (car datum) 'declare-postfix-operator)
			   (not comment))
		  ;(set! postfix-flag #t)
		  (insert-in-postfix-lst (cadr datum))
					;(display "kernel.rkt : (get-postfix-lst) :") (display (get-postfix-lst)) (newline)
					;(error "SRFI-110 : postfix declaration found:" datum)
		  )
		
		(let ((expression '()))

		  (if #;strict-srfi-105-pragma (or strict-srfi-105-pragma annotation-pragma comment #;postfix-flag) ; comment should now take in account at other place of the parsing, yes but buggy !!! (as the original version of SRFI 105 too) so i re-activate this version (SRFI 105 corrected)
                      (begin
			(when comment
                          (set! comment #f)) ; reset the comment flag before - SRFI 105 corrected
			(when postfix-flag
			  (set! postfix-flag #f))
			(set! expression (my-read-delimited-list my-read stop-char port))) ; drop the datum as it is a pragma directive or a commented expression
		      (set! expression (cons datum ;; normal case
					     (my-read-delimited-list my-read stop-char port))))
		  
		  (when q-reg
		    ;;(display "expression=") (display expression) (newline) ; here we possibly have finished a *quote* region ,pop
		    (end-region)) ; pop !

		  ;; for test only
		  ;; (when (equal? expression '(newline))
		  ;;   (read-error "(newline) test passed"))
		  
		  expression))))))))

; -----------------------------------------------------------------------------
; Read preservation, replacement, and mode setting
; -----------------------------------------------------------------------------

  (: default-scheme-read :reader-proc:)
  (define default-scheme-read read)
  (: replace-read :reader-proc:)
  (define replace-read replace-read-with)
  (define (restore-traditional-read) (replace-read-with default-scheme-read))

  (begin (define (enable-curly-infix)
      (when (not (or (eq? mutable-read curly-infix-read)
                     (eq? mutable-read neoteric-read)
                     (eq? mutable-read sweet-read)))
        (replace-read curly-infix-read)))
    
    (define (enable-neoteric)
      (when (not (or (eq? mutable-read neoteric-read)
                     (eq? mutable-read sweet-read)))
        (replace-read neoteric-read))))
  
  (define (enable-sweet)
    (replace-read sweet-read))

  ; (: set-read-mode deprecated) ; Should it be deprecated?
  (define (set-read-mode mode port)
    ; TODO: Should be per-port
    (cond
      ((eq? mode 'common-lisp)
        (set! common-lisp #t) #t)
      ((eq? mode 'literal-barred-symbol)
        (set! literal-barred-symbol #t) #t)
      ((eq? mode 'fold-case)
        (set! is-foldcase #t) #t)
      ((eq? mode 'no-fold-case)
        (set! is-foldcase #f) #t)
      ((eq? mode 'keyword-prefix)
        (set! keyword-prefix #t) #t)
      ((eq? mode 'no-keyword-prefix)
        (set! keyword-prefix #f) #t)
      ((eq? mode 'keyword-suffix)
        (set! keyword-suffix #t) #t)
      ((eq? mode 'no-keyword-suffix)
        (set! keyword-suffix #f) #t)
      (else (display "Warning: Unknown mode") #f)))

  ; defini en double?
  ;(define keyword-prefix #f) ; Accept ":STUFF" as keyword?  
  ;(define keyword-suffix #f) ; Accept "STUFF:" as keyword?  
; -----------------------------------------------------------------------------
; Scheme Reader re-implementation
; -----------------------------------------------------------------------------

; We have to re-implement our own Scheme reader.
; This takes more code than it would otherwise because many
; Scheme readers will not consider [, ], {, and } as delimiters
; (they are not required delimiters in R5RS and R6RS).
; Thus, we cannot call down to the underlying reader to implement reading
; many types of values such as symbols.
; If your Scheme's "read" also considers [, ], {, and } as
; delimiters (and thus are not consumed when reading symbols, numbers, etc.),
; then underlying-read could be much simpler.
; We WILL call default-scheme-read on string reading (the ending delimiter
; is ", so that is no problem) - this lets us use the implementation's
; string extensions if any.

  ; Identifying the list of delimiter characters is harder than you'd think.
  ; This list is based on R6RS section 4.2.1, while adding [] and {},
  ; but removing "#" from the delimiter set.
  ; NOTE: R6RS has "#" as a delimiter.  However, R5RS does not, and
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
                   #\' #\` #\,  ; Not required delimiters; do for error check
                   #\" #\;)     ; Could add #\# or #\|
             whitespace-chars))

  (: consume-whitespace (input-port -> undefined))
  (define (consume-whitespace port)
    (let ((char (my-peek-char port)))
      (cond
        ((eof-object? char) (no-values))
        ((eqv? char #\;)
          (consume-to-eol port)
          (consume-whitespace port))
        ((my-char-whitespace? char)
          (my-read-char port)
          (consume-whitespace port))
        (else (no-values)))))

  (: read-until-delim (input-port (list-of char) -> (list-of char)))
  (define (read-until-delim port delims)
    ; Read characters until eof or a character in "delims" is seen.
    ; Do not consume the eof or delimiter.
    ; Returns the list of chars that were read.
    (let ((c (my-peek-char port)))
      (cond
         ((eof-object? c) '())
         ((memv c delims) '())
         (else (my-read-char port)
	       (cons c (read-until-delim port delims))))))

  
  (: read-error (* -> . *))
  (define (read-error message)
    (let-values (((line col pos) (port-next-location global-port)))
	((if (string-contains-ci message "eof") raise-read-eof-error raise-read-error)
	 (string-append "Error: " message)
	 (or global-source source)
	 line col pos (and pos 1)))
    
    (display "Error: " (current-error-port))
    (display message (current-error-port))
    (newline (current-error-port))
    
    #;(cond-expand ; added 2025
     (racket
      (define-values (line col pos) (port-next-location global-port))
      (display " in source file " (current-error-port))
      (display source (current-error-port))
      (display " at position: line=" (current-error-port))
      (display line (current-error-port))
      (display " column=" (current-error-port))
      (display col (current-error-port))
      (display " character offset in source file=" (current-error-port))
      (display pos (current-error-port))
      (newline (current-error-port))))

    (flush-output-port (current-error-port))
    (raise-exception 'readable)
    '())


  ; Return the number by reading from port, and prepending starting-lyst.
  ; Returns #f if it's not a number.
  (: read-number (input-port (list-of char) -> (or number boolean)))
  (define (read-number port starting-lyst)
    (string->number (list->string
		     (append starting-lyst
			     (read-until-delim port neoteric-delimiters)))))

  ;; added by D.MATTEI
  ;; not used for now, TODO: use later
  (define (read-number-or-identifier-starting-with-digits port starting-lyst)
    ;;(newline (current-error-port))
    ;;(display starting-lyst (current-error-port)) (newline (current-error-port))

    (let* ((str-number-or-identifier (list->string
				      (append starting-lyst
					      (read-until-delim port neoteric-delimiters))))
	   (number (string->number str-number-or-identifier))) ; end declarative let
      (if number ; string->number return #f if it was not possible to convert it in a number
	  number
	  (string->symbol str-number-or-identifier))))


  ; Return list of digits read from port; may be empty.
  (: read-digits (input-port -> (or (pair (or eof char) *) null)))
  (define (read-digits port)
    (let ((c (my-peek-char port)))
      (case c
        ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
          (cons (my-read-char port) (read-digits port)))
        (else '())))) ; not cond expression

  ;; FIXME See comment in read-digits: It might be a good idea to use
  ;; something better than a-lists in such frequently executed code.
  (define char-name-values
    ; Includes standard names and guile extensions; see
    ; http://www.gnu.org/software/guile/manual/html_node/Characters.html
    '((space #x0020)
      (newline #x000a)
      (nl #x000a)
      (tab #x0009)
      (nul #x0000)
      (null #x0000)
      (alarm #x0007)
      (backspace #x0008)
      (linefeed #x000a)
      (vtab #x000b)
      (page #x000c)
      (return #x000d)
      (esc #x001b)
      (delete #x007f)
      ; Additional character names as extensions:
      (soh #x0001)
      (stx #x0002)
      (etx #x0003)
      (eot #x0004)
      (enq #x0005)
      (ack #x0006)
      (bel #x0007)
      (bs #x0008)
      (ht #x0009)
      (lf #x000a)
      (vt #x000b)
      (ff #x000c)
      (np #x000c) ; new page
      (cr #x000d)
      (so #x000e)
      (si #x000f)
      (dle #x0010)
      (dc1 #x0011)
      (dc2 #x0012)
      (dc3 #x0013)
      (dc4 #x0014)
      (nak #x0015)
      (syn #x0016)
      (etb #x0017)
      (can #x0018)
      (em #x0019)
      (sub #x001a)
      (fs #x001c)
      (gs #x001d)
      (rs #x001e)
      (sp #x0020)
      (del #x007f)
      ; Other non-guile names.
      ; rubout noted in: http://docs.racket-lang.org/reference/characters.html
      ; It's also required by the Common Lisp spec.
      (rubout #x007f)))
  
  (: process-char (input-port -> (or eof char null)))
  (define (process-char port)
    ; We've read #\ - returns what it represents.
    (cond
      ((eof-object? (my-peek-char port)) (my-peek-char port))
      (else
        ; Not EOF. Read in the next character, and start acting on it.
        (let ((c (my-read-char port))
              (rest (read-until-delim port neoteric-delimiters)))
          (cond
            ((null? rest) c) ; only one char after #\ - so that's it!
            (else ; More than one char; do a lookup.
             (let* ((rest-string (string-downcase (list->string (cons c rest))))
		    (cname (string->symbol
			     rest-string))
                     (found (assq cname char-name-values)))
                ;; (if found
                ;;     (integer->char (cadr found))
                ;;     (read-error "Invalid character name")))))))))
		(cond (found (integer->char (cadr found))) ; added 2025
		      ;; u: unicode ? char with code number in hexadecimal , example #\u1b (27 in decimal -> escape)
		      ((char-ci=? c #\u) (integer->char (string->number (substring rest-string 1) 16)))
		      (else
                       (read-error "Invalid character name"))))))))))

  ; If fold-case is active on this port, return string "s" in folded case.
  ; Otherwise, just return "s".  This is needed to support our
  ; is-foldcase configuration value when processing symbols.
  ; TODO: Should be port-specific
  (: fold-case-maybe (input-port string -> string))
  (define (fold-case-maybe port s)
    (if is-foldcase
        (my-string-foldcase s)
        s))

  (: process-directive (string -> undefined))
  (define (process-directive dir) ; TODO could allow to deal with all the directives i use :BEGIN-STRICT-SRFI-105-REGION
    ; if written starting with #!directive : #!BEGIN-STRICT-SRFI-105-REGION, #!END-STRICT-SRFI-105-REGION,....
    (cond
      ; TODO: These should be specific to the port.
      ((string-ci=? dir "sweet")
        (enable-sweet))
      ((string-ci=? dir "no-sweet")
        (restore-traditional-read))
      ((string-ci=? dir "curly-infix")
        (replace-read curly-infix-read))
      ((string-ci=? dir "fold-case")
        (set! is-foldcase #t))
      ((string-ci=? dir "no-fold-case")
        (set! is-foldcase #f))
      ((string-ci=? dir "keyword-prefix")
        (set! keyword-prefix #t))
      ((string-ci=? dir "no-keyword-prefix")
        (set! keyword-prefix #f))
      ((string-ci=? dir "keyword-suffix")
        (set! keyword-suffix #t))
      ((string-ci=? dir "no-keyword-suffix")
        (set! keyword-suffix #f))
      (else (display "Warning: Unknown process directive"))))

  ; Consume characters until "!#"
  ;; FIXME other procedures skipping chars have names beginning with consume- here.!
  (: non-nest-comment (input-port -> undefined))
  (define (non-nest-comment port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c)
          (no-values))
        ((char=? c #\!)
          (let ((c2 (my-peek-char port)))
            (if (char=? c2 #\#)
                (begin
                  (my-read-char port)
                  (no-values))
                (non-nest-comment port))))
        (else
          (non-nest-comment port)))))

  (: process-sharp-bang (input-port -> :reader-token:))
  (define (process-sharp-bang port)
    (let ((c (my-peek-char port)))
      (cond
        ((char=? c #\space) ; SRFI-22
          (consume-to-eol port)
          (consume-end-of-line port)
          scomment-result) ; Treat as comment.
        ((memv c '(#\/ #\.)) ; Multi-line, non-nesting #!/ ... !# or #!. ...!#
          (non-nest-comment port)
          scomment-result) ; Treat as comment.
        ((char-alphabetic? c)  ; #!directive
          (process-directive
            (list->string (read-until-delim port neoteric-delimiters)))
          scomment-result)
        ((or (eqv? c carriage-return) (eqv? c #\newline))
          ; Extension: Ignore lone #!
          (consume-to-eol port)
          (consume-end-of-line port)
          scomment-result) ; Treat as comment.
        (else (read-error "Unsupported #! combination")))))

  ; A cons cell always has a unique address (as determined by eq?);
  ; we'll use this special cell to tag unmatched datum label references.
  ; An unmatched datum label will have the form
  ; (cons unmatched-datum-label-tag NUMBER)
  (define unmatched-datum-label-tag
    (cons 'unmatched-datum-label-tag 'label))
  (define (is-matching-label-tag? number value)
    (and
      (pair? value)
      (eq?  (car value) unmatched-datum-label-tag)
      (eqv? (cdr value) number)))

  ; Patch up datum, starting at position,
  ; so that all labels "number" are replaced
  ; with the value of "replace".
  ; Note that this actually OVERWRITES PORTIONS OF A LIST with "set!",
  ; so that we can maintain "eq?"-ness.  This is really wonky,
  ; but datum labels are themselves wonky.
  ; "skip" is a list of locations that don't need (re)processing; it
  ; should be a hash table but those aren't portable.

  (begin (define (patch-datum-label-tail number replace position skip)
      (let ((new-skip (cons position skip)))
	(cond
         ((and (pair? position)
               (not (eq? (car position) unmatched-datum-label-tag))
               (not (memq position skip)))
          (if (is-matching-label-tag? number (car position))
              (set-car! position replace) ; Yes, "set!" !!
              (patch-datum-label-tail number replace (car position) new-skip))
          (if (is-matching-label-tag? number (cdr position))
              (set-cdr! position replace) ; Yes, "set!" !!
              (patch-datum-label-tail number replace (cdr position) new-skip))
	  (no-values))
         ((vector? position)
          (do ((len (vector-length position))
               (k 0 (+ k 1)))
              ((>= k len) (no-values))
            (let ((x (vector-ref position k)))
              (if (is-matching-label-tag? number x)
                  (vector-set! position k replace)
                  (patch-datum-label-tail number replace x new-skip)))))
         (else (no-values)))))

    (define (patch-datum-label number starting-position)
      (if (is-matching-label-tag? number starting-position)
          (read-error "Illegal reference as the labelled object itself"))
      (patch-datum-label-tail number starting-position starting-position '())
      starting-position))

  ; Gobble up the to-gobble characters from port, and return # ungobbled.
  (define (gobble-chars port to-gobble)
    (if (null? to-gobble)
        0
        (cond
          ((char-ci=? (my-peek-char port) (car to-gobble))
            (my-read-char port)
            (gobble-chars port (cdr to-gobble)))
          (else (length to-gobble)))))

  (: scomment-result :reader-token:)
  (define scomment-result '(scomment ()))

  (: process-sharp (:reader-proc: input-port -> *))
  (define (process-sharp no-indent-read port)
    ; We've read a # character.  Returns what it represents as
    ; (stopper value); ('normal value) is value, ('scomment ()) is comment.
    ; Since we have to re-implement process-sharp anyway,
    ; the vector representation #(...) uses my-read-delimited-list, which in
    ; turn calls no-indent-read.
    ; TODO: Create a readtable for this case.
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) scomment-result) ; If eof, pretend it's a comment.
        ((char-line-ending? c) ; Extension - treat # EOL as a comment.
          scomment-result) ; Note this does NOT consume the EOL!
        (else ; Try out different readers until we find a match.
          (my-read-char port)
          (or
            (and common-lisp
                 (parse-cl no-indent-read c port))
            (parse-hash no-indent-read c port)
            (parse-default no-indent-read c port)
            (read-error "Invalid #-prefixed string"))))))


  ; (string->value "#hash((a . 1) (b . 2))")
  ; '#hash((a . 1) (b . 2))
  ; courtesy of ChatGPT:
  (define (string->value s)
    (read (open-input-string s)))

  
  (: parse-default (:reader-proc: char input-port -> *))
  (define (parse-default no-indent-read c port)
              (cond ; Nothing special - use generic rules
                ((char-ci=? c #\t)
                  (if (memv (gobble-chars port '(#\r #\u #\e)) '(0 3))
                      '(normal #t)
                      (read-error "Incomplete #true")))
                ((char-ci=? c #\f)
                  (if (memv (gobble-chars port '(#\a #\l #\s #\e)) '(0 4))
                      '(normal #f)
                      (read-error "Incomplete #false")))
		
		;; should be for parsing binary,octal,hexadecimal,etc numbers
                ((memv c '(#\i #\e #\b #\o #\d #\x
                           #\I #\E #\B #\O #\D #\X))
                  (let ((num (read-number port (list #\# (char-downcase c)))))
                    (if num
                        `(normal ,num)
                        (read-error "Not a number after number start"))))
		
                ((char=? c #\( )  ; Vector.
                  (list 'normal (list->vector
				 (my-read-delimited-list no-indent-read #\) port))))

                ; hash tables as srfi 105 for racket
		;; hash table : #hash(("a" . 1) ("b" . 20)) support written for racket scheme
                ;; Racket's regular expressions special syntax
                ; and other hash table types :hasheq,...
                ((char=? c #\h) (if (not (and (char=? (read-char port) #\a)
                                              (char=? (read-char port) #\s)
                                              (char=? (read-char port) #\h)))
                                    (read-error "process-sharp : awaiting hash : token not found")
                                    (cond ((char=? (peek-char port) #\() ; look at the char without getting it from port
                                           (read-char port) ; really get the char
                                           ; recreate '#hash(list ...) ,example : '#hash((a . 1) (b . 2))
                                           (string->value (string-append "#hash"
                                                                         (format "~a" (my-read-delimited-list no-indent-read #\) port))))) ; write the list in a string
                                          ((and (char=? (read-char port) #\e) ; really get the char
                                                (char=? (read-char port) #\q)) ; here we got #hasheq
                                           (let ((cht (read-char port))) ; should be v or (
                                             (cond ((char=? cht #\() ; here we got #hasheq(
                                                    ; recreate '#hasheq(list ...) ,example : '#hasheq((a . 1) (b . 2))
                                                    (string->value (string-append "#hasheq"
                                                                                  (format "~a" (my-read-delimited-list no-indent-read #\) port))))) ; write the list in a string
                                                   ((char=? cht #\v) ; here we got #hasheqv
                                                    (if (char=? (read-char port) #\()
                                                        ; recreate '#hasheqv(list ...) ,example : '#hasheqv((a . 1) (b . 2))
                                                        (string->value (string-append "#hasheqv"
                                                                                      (format "~a" (my-read-delimited-list no-indent-read #\) port)))) ; write the list in a string
                                                        (read-error "process-sharp : awaiting hasheqv( : token not found")))
                                                   (else
                                                    (read-error "process-sharp : awaiting hasheq( or hasheqv( : token not found")))))
                                          (else
                                           (read-error "process-sharp : awaiting hasheq : partial token not found")))))
                
		
                ((char=? c #\u )  ; u8 Vector.
                  (cond
                    ((not (eqv? (my-read-char port) #\8 ))
                      (read-error "#u must be followed by 8"))
                    ((not (eqv? (my-read-char port) #\( ))
                      (read-error "#u8 must be followed by left paren"))
                    (else (list 'normal (list->u8vector
					 (my-read-delimited-list no-indent-read #\) port))))))
		
                ((char=? c #\\)
                  (list 'normal (process-char port)))

		;; Warning : buggy if #;(list something) ??? better use the enhanced version of SRFI 105 which correct this bug
                ; Handle #; (item comment).
                ;; ((char=? c #\;)
                ;;   (if (memv (my-peek-char port)
                ;;             `(#\space #\tab ,linefeed ,carriage-return))
                ;;     '(datum-commentw ())
                ;;     (begin
                ;;       (no-indent-read port)  ; Read the datum to be consumed.
                ;;       scomment-result))) ; Return comment

		;; commented nested scheme expressions as in SRFI-105 code
		;; (+ 1 #;2 3)
		;; 4
		;; (+ 1 #;(+ 1 2) 3)
		;; 4
		;; (+ 1 #;{1 + 2} 3)
		;; 4
		;; (cons 1 2 #;(foo bar))
		;; (cons 1 2)
		;;'(1 . 2)
		#;((char=? c #\;)
		 (let ((comy (no-indent-read port))) ; commented expression
		   (set! comment #t)
		   (list 'normal comy))) ; must be returned but will be dropped later
		; inside a list ( ) : the commented datum is returned and dropped by my-read-delimited-list (comment flag) ,
		; at the level of the lines of the sweet-expressions : the original SRFI 110 , #; followed by a white space
		; comments the next line (or the indented block) , #;datum the next datum
		((char=? c #\;)
		 (if in-list
		     (let ((comy (no-indent-read port))) ; commented expression
		       (set! comment #t)
		       (list 'normal comy)) ; must be returned but will be dropped later
		     (if (memv (my-peek-char port)
			       `(#\space #\tab ,linefeed ,carriage-return))
			 '(datum-commentw ())
			 (begin
			   (no-indent-read port)  ; Read the datum to be consumed.
			   scomment-result)))) ; Return comment
	       
		
                ; handle nested comments
		; This supports SRFI-30 #|...|#
		; #| ... |# is pure "atmosphere" (like a ; comment or
		; white space), it is not a datum comment (that is #; ,
		; handled above). We used to unconditionally return
		; scomment-result here and let underlying-read blindly
		; call (no-indent-read port) again to fetch "the real
		; next datum". That breaks when the block comment is the
		; last thing before the enclosing list's closing
		; delimiter: no-indent-read then tries to read a whole
		; new datum, peeks the closing #\) #\] or #\} , and
		; errors with "Closing parenthesis/bracket/brace without
		; opening" even though that delimiter is perfectly
		; legitimate one level up (same bug as was found and
		; fixed in SRFI-105-curly-infix.rkt).
		; Fix: after the comment, peek (without consuming) past
		; any remaining white space / ; comments. If what follows
		; is a closing delimiter, there is no next datum to read
		; here: mark this as a dropped datum (like #;), so the
		; caller (my-read-delimited-list) safely loops and
		; notices the stop-char itself instead of us trying (and
		; failing) to read past it. EOF is left untouched: the
		; existing recursion into underlying-read already handles
		; it cleanly (its own (eof-object? c) c branch).
		((and (char=? c #\|)
		      (not in-list)) ; at the level of the lines of the sweet-expressions : the end of line is kept
		 (nest-comment port)
		 scomment-result)
		((char=? c #\|)
                  (nest-comment port)
                  (consume-whitespace port)
                  (let ((c2 (my-peek-char port)))
                    (cond
                      ((eof-object? c2) scomment-result)
                      ((memv c2 '(#\) #\] #\}))
                        (set! comment #t)
                        (list 'normal (void))) ; dropped, like #;
                      (else scomment-result))))

                ((char=? c #\!)
                 (process-sharp-bang port))


		;; Racket's regular expressions special syntax and other stuff could be added here (see SRFI 105 code)
		;; read #:blabla
		((char=? c #\:) (list 'normal
				      (string->keyword ;; also add #: in front of argument
				       (list->string
					(read-until-delim port neoteric-delimiters)))))

		;; Racket's regular expressions special syntax
		((char=? c #\r) (if (not (char=? (my-read-char port) #\x))
				    (read-error "process-sharp : awaiting regexp : character x not found")
				    (let ((str (no-indent-read port)))
				      (if (not (string? str))
					  (read-error "process-sharp : awaiting regexp : string not found")
					  (list 'normal
						(list 'regexp str))))))

		((char=? c #\p) (if (not (char=? (my-read-char port) #\x))
				    (read-error "process-sharp : awaiting regexp : character x not found")
				    (let ((str (no-indent-read port)))
				      (if (not (string? str))
					  (read-error "process-sharp : awaiting pregexp : string not found")
					  (list 'normal
						(list 'pregexp str))))))

		
		
                ((memv c digits) ; Datum label, #num# or #num=...
                  (let* ((my-digits (read-digits port))
                         (label (string->number (list->string
                                                   (cons c my-digits)))))
                    (cond
                      ((eqv? (my-peek-char port) #\#)
                        (my-read-char port)
                        (list 'normal (cons unmatched-datum-label-tag label)))
                      ((eqv? (my-peek-char port) #\=)
                       (my-read-char port)
		       (if (my-char-whitespace? (my-peek-char port))
                           (read-error "#num= followed by whitespace"))
                       (list 'normal
                             (patch-datum-label label (no-indent-read port))))
                      (else
                       (read-error "Datum label #NUM requires = or #")))))
		
                ; R6RS abbreviations #' #` #, #,@
                ((char=? c #\')
                  '(abbrev syntax))
                ((char=? c #\`)
                  '(abbrev quasisyntax))
                ((char=? c #\,)
                  (let ((c2 (my-peek-char port)))
                    (cond
                      ((char=? c2 #\@)
                        (my-read-char port)
                        '(abbrev unsyntax-splicing))
                      (else
                        '(abbrev unsyntax)))))
                ((or (char=? c #\space) (char=? c tab))
                  ; Extension - treat # (space|tab) as a comment to end of line.
                  ; This is not required by SRFI-105 or SRFI-110, but it's
                  ; handy because "# " is a comment-to-EOL in many other
                  ; languages (Bourne shells, Perl, Python, etc.)
                  (consume-to-eol port)
                  scomment-result) ; Return comment
                (else #;#f
		 (read-error (string-append "SRFI-105 REPL :"
					   "Unsupported # extension"
					   " unsupported character causing this message is character:"
					   (string c))))))

  (: parse-cl (:reader-proc: char input-port -> (or (list symbol symbol) boolean)))
  (define (parse-cl no-indent-read c port)
    ; These are for Common Lisp; the "unsweeten" program
    ; can deal with the +++ ones.
    (cond
      ; In theory we could just abbreviate this as "function".
      ; However, this won't work in expressions like (a . #'b).
      ((char=? c #\')
        '(abbrev +++SHARP-QUOTE-abbreviation+++))
      ((char=? c #\:)
        '(abbrev +++SHARP-COLON-abbreviation+++))
      ((char=? c #\.)
        '(abbrev +++SHARP-DOT-abbreviation+++))
      ((char=? c #\+)
        '(abbrev +++SHARP-PLUS-abbreviation+++))
      ((char=? c #\P)
        '(abbrev +++SHARP-P-abbreviation+++))
      (else #f)))

  ; Translate "x" to Common Lisp representation if we're printing CL.
  ; Basically we use a very unusual representation, and then translate it back
  (: translate-cl (* --> *))
  (define translate-cl
    (let ((qq (string->symbol "+++CL-QUASIQUOTE-abbreviation+++"))
	  (uq (string->symbol "+++CL-UNQUOTE-abbreviation+++"))
	  (us (string->symbol "+++CL-UNQUOTE-SPLICING-abbreviation+++")))
      (lambda (x)
	(if common-lisp
	    (case x
	      ((quasiquote)       qq)
	      ((unquote)          uq)
	      ((unquote-splicing) us)
	      (else x)) ; not cond expression
	    x))))
                  
  ; detect #| or |#
  (: nest-comment (input-port -> undefined))
  (define (nest-comment fake-port)
    (let ((c (my-read-char fake-port)))
      (cond
        ((eof-object? c)
          (no-values))
        ((char=? c #\|)
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\#)
                (begin
                  (my-read-char fake-port)
                  (no-values))
                (nest-comment fake-port))))
        ((and hash-pipe-comment-nests? (char=? c #\#))
          (let ((c2 (my-peek-char fake-port)))
            (if (char=? c2 #\|)
                (begin
                  (my-read-char fake-port)
                  (nest-comment fake-port))
                (no-values))
            (nest-comment fake-port)))
        (else
          (nest-comment fake-port)))))

  (: digits (list-of char))
  (define digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

  (: process-period (input-port -> (or number boolean null symbol)))
  (define (process-period port)
    ; We've peeked a period character.  Returns what it represents.
    (my-read-char port) ; Remove .
    (let ((c (my-peek-char port)))
      (cond
        ((eof-object? c) period-symbol) ; period eof; return period.
        ((memv c '(#\' #\` #\, #\#)) period-symbol) ; End, for some CL code
        ((memv c digits) ; period digit - it's a number.
          (let ((num (read-number port (list #\.))))
            (if num
                num
                (read-error "period digit must be a number"))))
        (else
          ; At this point, Scheme only requires support for "." or "...".
          ; As an extension we can support them all.
          (string->symbol
            (fold-case-maybe port
              (list->string (cons #\.
                (read-until-delim port neoteric-delimiters)))))))))

  ; Read an inline hex escape (after \x), return the character it represents
  (: read-inline-hex-escape (input-port -> char))
  (define (read-inline-hex-escape port)
    (let* ((chars (read-until-delim port (append neoteric-delimiters '(#\;))))
           (n (string->number (list->string chars) 16)))
      (if (eqv? #\; (my-peek-char port))
          (my-read-char port)
          (read-error "Unfinished inline hex escape"))
      (if (not n)
          (read-error "Bad inline hex escape"))
      (integer->char n)))

  ; We're inside |...| ; return the list of characters inside.
  ; Do NOT call fold-case-maybe, because we always use literal values here.
  (: read-symbol-elements (input-port -> (list-of char)))
  (define (read-symbol-elements port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c) '())
        ((eqv? c #\|)    '()) ; Expected end of symbol elements
        ((eqv? c #\\)
          (let ((c2 (my-read-char port)))
            (cond
              ((eof-object? c2) '())
              ((eqv? c2 #\|)   (cons #\| (read-symbol-elements port)))
              ((eqv? c2 #\\)   (cons #\\ (read-symbol-elements port)))
              ((eqv? c2 #\a)   (cons (integer->char #x0007)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\b)   (cons (integer->char #x0008)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\t)   (cons (integer->char #x0009)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\n)   (cons (integer->char #x000a)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\r)   (cons (integer->char #x000d)
                                     (read-symbol-elements port)))
              ((eqv? c2 #\f)   (cons (integer->char #x000c) ; extension
                                     (read-symbol-elements port)))
              ((eqv? c2 #\v)   (cons (integer->char #x000b) ; extension
                                     (read-symbol-elements port)))
              ((eqv? c2 #\x) ; inline hex escape =  \x hex-scalar-value ;
                (cons 
                      (read-inline-hex-escape port)
                      (read-symbol-elements port))))))
        (else (cons c (read-symbol-elements port))))))

  ; Extension: When reading |...|, *include* the bars in the symbol, so that
  ; when we print it out later we know that there were bars there originally.
  (: read-literal-symbol (input-port -> (list-of char)))
  (define (read-literal-symbol port)
    (let ((c (my-read-char port)))
      (cond
        ((eof-object? c) (read-error "EOF inside literal symbol"))
        ((eqv? c #\|)    '(#\|)) ; Expected end of symbol elements
        ((eqv? c #\\)
          (let ((c2 (my-read-char port)))
            (if (eof-object? c2)
              (read-error "EOF after \\ in literal symbol")
              (cons c (cons c2 (read-literal-symbol port))))))
        (else (cons c (read-literal-symbol port))))))

  ; Read |...| symbol (like Common Lisp)
  ; This is present in R7RS draft 9.
  (: get-barred-symbol (input-port -> symbol))
  (define (get-barred-symbol port)
    (my-read-char port) ; Consume the initial vertical bar.
    (string->symbol (list->string
      (if literal-barred-symbol
        (cons #\| (read-literal-symbol port))
        (read-symbol-elements port)))))

  ; This implements a simple Scheme "read" implementation from "port",
  ; but if it must recurse to read, it will invoke "no-indent-read"
  ; (a reader that is NOT indentation-sensitive).
  ; This additional parameter lets us easily implement additional semantics,
  ; and then call down to this underlying-read procedure when basic reader
  ; procedureality (implemented here) is needed.
  ; This lets us implement both a curly-infix-ONLY-read
  ; as well as a neoteric-read, without duplicating code.
  (: underlying-read (:reader-proc: input-port -> *))
  (define (underlying-read no-indent-read port)
    (consume-whitespace port)
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char=? c #\")
          ; old readers tend to read strings okay, call it.
          ; (guile 1.8 and gauche/gosh 1.8.11 are fine)
          (invoke-read default-scheme-read port))
        (else
          ; attach the source information to the item read-in
          (attach-sourceinfo pos
            (cond
              ((char=? c #\#)
                (my-read-char port)
                (let ((rv (process-sharp no-indent-read port)))
                  (cond
                   ((eq? (car rv) 'scomment) (no-indent-read port))
                   ((eq? (car rv) 'datum-commentw)
                    (no-indent-read port) ; Consume following datum.
                    (no-indent-read port))
                   ((eq? (car rv) 'normal) (cadr rv))
                   ((eq? (car rv) 'abbrev)
                    (list (cadr rv) (no-indent-read port)))
                   (else   (read-error "Unknown # sequence")))))
	      
              ((char=? c #\.) (process-period port)) ; note: could lead to a number too

	      ;;;;;;;;;;;;;;;;;;;;;;;;;
              ((or (memv c digits)
		   (char=? c #\+)
		   (char=? c #\-))
                (let*
                  ((maybe-number (list->string
				  (read-until-delim port neoteric-delimiters)))
                   (as-number (string->number maybe-number)))
                  (if as-number
                      as-number
                      (string->symbol (fold-case-maybe port maybe-number)))))

	      ;; here we should 'push' as it is quoted or backquoted (pseudoquote,quasiquote)
              ((char=? c #\') ; quote
                (my-read-char port)
		(find*quote)
		(let ((mrp (no-indent-read port)))
		  (end-region) ; pop !
                  (list (attach-sourceinfo pos 'quote)
			mrp)))
	      
              ((char=? c #\`) ; quasiquote
                (my-read-char port)
		(find*quote)
		(let ((mrp (no-indent-read port)))
		  (end-region) ; pop !
                  (list (attach-sourceinfo pos (translate-cl 'quasiquote))
			mrp)))
	      
	      ;; here we should 'pop' as we have to eval with unquote or unquote-splicing
              ((char=? c #\,) ; unquote
                (my-read-char port)
		(find-unquote*)
		(cond
                    ((char=? #\@ (my-peek-char port))
                      (my-read-char port)
		      (let ((mrp (no-indent-read port)))
			(end-region) ; pop !
			(list (attach-sourceinfo pos
						 (translate-cl 'unquote-splicing))
			      mrp)))
                    (else
		     (let ((mrp (no-indent-read port)))
		       (end-region) ; pop !
		       (list (attach-sourceinfo pos (translate-cl 'unquote))
			     mrp)))))
	      
              ((char=? c #\( )
                  (my-read-char port)
                  (my-read-delimited-list no-indent-read #\) port))
              ((char=? c #\) )
                (my-read-char port)
                (read-error "Closing parenthesis without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\[ )   ;;(default-scheme-read port)) ;; this convert [ ... ] in ($bracket-list$ ...) in Kawa at least allowing Kawa special expressions such as: [1 <: 7] , with SRFI 105, to be checked with SRFI 110

                  (my-read-char port)
                  (my-read-delimited-list no-indent-read #\] port))
              ((char=? c #\] )
                (my-read-char port)
                (read-error "Closing bracket without opening")
                (underlying-read no-indent-read port))
              ((char=? c #\} )
                (my-read-char port)
                (read-error "Closing brace without opening")
                (underlying-read no-indent-read port))
	      
              ((char=? c #\| )
                ; Read |...| symbol (like Common Lisp and R7RS draft 9)
               (get-barred-symbol port))
	      
              (else ; Nothing else.  Must be a symbol or keyword start.
                (let ((s (fold-case-maybe port
                          (list->string
                            (read-until-delim port neoteric-delimiters)))))
                  (cond
                    ((and keyword-suffix
                        (> (string-length s) 1)
                        (eq? (string-ref s (- (string-length s) 1)) #\:))
                     (string->keyword (substring s 0 (- (string-length s) 1))))
                    ((and keyword-prefix
                          (> (string-length s) 1)
                          (eq? (string-ref s 0) #\:))
                     (string->keyword (substring s 1 (string-length s))))
                    (else (string->symbol s)))))))))))

; -----------------------------------------------------------------------------
; Curly Infix
; -----------------------------------------------------------------------------

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  (: even-and-op-prefix? (* list --> boolean))
  (define (even-and-op-prefix? op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f)
      ((not (equal? op (car lyst))) #f) ; fail - operators not the same
      ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
      (else   (even-and-op-prefix? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  (: simple-infix-list? (list --> boolean))
  (define (simple-infix-list? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix? (cadr lyst) (cdr lyst)))) ; true if rest is simple

  ; Return alternating parameters in a list (1st, 3rd, 5th, etc.)
  (: alternating-parameters (list --> list))
  #;(define (alternating-parameters lyst)
    (if (or (null? lyst) (null? (cdr lyst)))
        lyst
        (cons (car lyst) (alternating-parameters (cddr lyst)))))

  ; Not a simple infix list - transform it.  Written as a separate procedure
  ; so that future experiments or SRFIs can easily replace just this piece.
  (: transform-mixed-infix (list --> :reader-token:))
  (define (transform-mixed-infix lyst)
     (cons '$nfx$ lyst))


  
  ; Given curly-infix lyst, map it to its final internal format.
  (: process-curly (list --> list))
  #;(define (process-curly lyst)
    (cond
     ((not (pair? lyst)) lyst) ; E.G., map {} to ().
     ((null? (cdr lyst)) ; Map {a} to a.
       (car lyst))
     ((and (pair? (cdr lyst)) (null? (cddr lyst))) ; Map {a b} to (a b).
       lyst)
     ((simple-infix-list? lyst) ; Map {a OP b [OP c...]} to (OP a b [c...])
       (cons (cadr lyst) (alternating-parameters lyst)))
     (else  (transform-mixed-infix lyst))))

  
  ; Given curly-infix lyst, map it to its final internal format.
  (define (process-curly lyst)

  ;;(display "SRFI-105 : process-curly lyst=") (display lyst) (newline)

  (if use-only-syntax-transformers

      (cond 

       ;; E.G., map {} to ().
       
       ((not (pair? lyst)) lyst) ; E.G., map {} to ().


       
       ;; Map {a} to a.
       
       ((null? (cdr lyst)) ; Map {a} to a.

	(if srfi-strict
	    (car lyst)  ; original version
	    
	    ;; {(3.7 + 1)}
	    ;; ($nfx$ (3.7 + 1))
	    ;; 4.7
	    
	    ;;{3.7}
	    ;;($nfx$ 3.7)
	    ;;3.7
	    (list '$nfx$ (car lyst)))) ; ($nfx$ a)

       

       ;; Map {a b} to (a b).
       
       ((and (pair? (cdr lyst))
	     (null? (cddr lyst))) ; Map {a b} to (a b).

	(if srfi-strict
	    lyst

	    ;; '{abs (3.7 + 1)}
	    ;; '($nfx$ abs (3.7 + 1))

	    ;; {abs (3.7 + 1)}
	    ;; ($nfx$ abs (3.7 + 1))
	    ;; 4.7

	    ;; (define (h x y) {abs ((cos (x + y)) * (sin (x - y))) } )
	    ;; (define (h x y) ($nfx$ abs ((cos (x + y)) * (sin (x - y)))))
	    ;; #<eof>
	    ;; (h  .2 .3)
	    ;; (h 0.2 0.3)
	    ;; 0.08761206554319241
	    (cons '$nfx$ lyst))) ; ($nfx$ a b)



       ;; Map {a OP b [OP c...]} to (OP a b [c...])
       
       ;; deal quoted and quasi-quoted the old way
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '(- (2 + 3) (5 - 7) 2)
       
       ;; '{(2 + 3) - (5 - 7)}
       ;; '(- (2 + 3) (5 - 7))
       ((and (simple-infix-list? lyst)
	     (or (and care-of-quote
		      region-quote)
		 srfi-strict)) ; Map {a OP b [OP c...]} to (OP a b [c...])
	
	(cons (cadr lyst)
	      (alternating-parameters lyst)))
       
       ;; comment above force this (which is not what i want):
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '($nfx$ (2 + 3) - (5 - 7) - 2)

       ;; '{(2 + 3) - (5 - 7)}
       ;; '($nfx$ (2 + 3) - (5 - 7))

       ;; `{{2 + 3} - ,{2 + 1}}
       ;; `($nfx$ ($nfx$ 2 + 3) - ,($nfx$ 2 + 1))
       ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 + 1)>
       ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
       ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
       ;; '($nfx$ ($nfx$ 2 + 3) - 3)


       ;; general case
       
       (else ; will insert $nfx$ in front of list
	(transform-mixed-infix lyst))) 


      
      ;; `{{2 + 3} - ,{2 + 1}}
      ;; `(- (+ 2 3) ,($nfx$ 2 + 1))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (2 + 1)>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
      ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
      ;; '(- (+ 2 3) 3)

      ;; {x <- (1 + 2 + 3) - (4 + 5)}
      ;; ($nfx$ x <- (1 + 2 + 3) - (4 + 5))
      ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (x <- (1 + 2 + 3) - (4 + 5))>
      ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax x> .#<syntax <-> .#<syntax (1 + 2 + 3)> .#<syntax -> .#<syntax (4 + 5)>)
      ;; $nfx$ : parsed-args=.#<syntax (<- x (- (+ 1 2 3) (+ 4 5)))>
      ;; #<eof>
      ;; x
      ;; x
      ;; -3

     
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; else : limited use of  syntax transformers
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; {3 * 5 + 2}
      ;; (+ (* 3 5) 2)
      ;; 17
      
      (condx

       ;; E.G., map {} to ().
       
       ((not (pair? lyst)) lyst) ; E.G., map {} to ().

       ;; {a . z} : an improper list , as in SRFI 105 (without this : cddr contract violation)
       ((not (list? lyst)) (transform-mixed-infix lyst)) ; ($nfx$ a . z)


       ;; Map {a} to a.
       
       ((null? (cdr lyst)) ; Map {a} to a.

	(if #;srfi-strict (or srfi-strict region-quote) ; in a quote {a} is a , as in SRFI 105 : the quotes are respected (Scheme+/care-of-quotes)
	    (car lyst)  ; original version
	    
	    ;; {(3.7 + 1)}
	    ;; ($nfx$ (3.7 + 1))
	    ;; 4.7
	    
	    ;;{3.7}
	    ;;($nfx$ 3.7)
	    ;;3.7
	    (nfx (car lyst)))) ; ($nfx$ a)
	    ;;(read-error "SRFI-105-curly-infix : process-curly : DEBUG lyst =" lyst)))

       ;; Map {a b} to (a b).
       
       ((and (pair? (cdr lyst))
	     (null? (cddr lyst))) ; Map {a b} to (a b).

	(if #;srfi-strict (or srfi-strict region-quote) ; in a quote {a b} is (a b) , as in SRFI 105
	    lyst

	    ;; '{abs (3.7 + 1)}
	    ;; '($nfx$ abs (3.7 + 1))

	    ;; {abs (3.7 + 1)}
	    ;; ($nfx$ abs (3.7 + 1))
	    ;; 4.7

	    ;; (define (h x y) {abs ((cos (x + y)) * (sin (x - y))) } )
	    ;; (define (h x y) ($nfx$ abs ((cos (x + y)) * (sin (x - y)))))
	    ;; #<eof>
	    ;; (h  .2 .3)
	    ;; (h 0.2 0.3)
	    ;; 0.08761206554319241
	    (apply nfx lyst))) ; ($nfx$ a b)

       
       ;; list of operators
       (exec
	(define operands (alternating-parameters lyst))
	#;(define mbr+- (or (member '+ operands) ; there could be + - + + , superscripts ,so operators could be wrong
			  (member '- operands)))
	(define oper (cadr lyst)) ; first operator of list
	(define mbr+- (and (or (eq? oper '+) (eq? oper '-)) ; the +/- chained-sign ambiguity only makes sense when the repeating operator itself is + or - ,otherwise a bare + or - operand (e.g. {x := +}) is a legitimate value, not a disguised sign
			   (or (member '+ operands) ; there could be + - + + , superscripts ,so operators could be wrong
			       (member '- operands))))
					; {x := +} -> #f
					; {+ - 2} -> #t
					; {5 - 2} -> #f
	(define sil (simple-infix-list? lyst))
	;;(define oper (cadr lyst)) ; first operator of list (now defined before mbr+-)
	(define infx (not (eq? oper 'if))) ; true infix, not Python 'statement if test else statement2'
	;;(read-error "SRFI-105-curly-infix : infx =" infx) 
	); when all operators are the same

       
       ;; Map {a OP b [OP c...]} to (OP a b [c...])
       
       ;; deal quoted and quasi-quoted the old way
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '(- (2 + 3) (5 - 7) 2)
       
       ;; '{(2 + 3) - (5 - 7)}
       ;; '(- (2 + 3) (5 - 7))
       ; old condition :
       ; ((and infx ; true infix
       ;       sil ; simple infix list , when all operators are the same
       ;       (or (and care-of-quote
       ;                region-quote)
       ;           srfi-strict)) ; Map {a OP b [OP c...]} to (OP a b [c...])
       ((and sil ; simple infix list , when all operators are the same
	     (or srfi-strict ; SRFI 105 : (OP a b [c...]) , even with if
		 (and infx ; true infix , not the Python if of Scheme+
		      region-quote))) ; in a quote the operands are data , their ( ) are not converted (as in SRFI 105)
	
	(cons oper ; first operator of list
	      operands))  ; Map {a OP b [OP c...]} to (OP a b [c...])

       
       ;; comment above force this (which is not what i want):
       ;; '{(2 + 3) - (5 - 7) - 2}
       ;; '($nfx$ (2 + 3) - (5 - 7) - 2)

       ;; '{(2 + 3) - (5 - 7)}
       ;; '($nfx$ (2 + 3) - (5 - 7))

       ;; `{{2 + 3} - ,{2 + 1}}
       ;; `($nfx$ ($nfx$ 2 + 3) - ,($nfx$ 2 + 1))
       ;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 + 1)>
       ;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax +> .#<syntax 1>)
       ;; $nfx$ : parsed-args=.#<syntax (+ 2 1)>
       ;; '($nfx$ ($nfx$ 2 + 3) - 3)

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


       
       ;; this allows some infix macros
       ;; (define-syntax +=
       ;;     (syntax-rules ()
       ;;       ({var1 _ var2} {var1 := var1 + var2})))

       ;; (define-syntax += (syntax-rules () ((_ var1 var2) (:= var1 (+ var1 var2))))) ; parsed result

       ;; {x := 3}
       ;; {x += 7}
       ;; x
       ;; 10

       ;; (define-syntax plus
       ;;     (syntax-rules ()
       ;;       ({var1 _ ...} {var1 + ...})))

       ;; (define-syntax plus (syntax-rules () ((_ var1 ...) (+ var1 ...)))) ; parsed result

       ;; {2 plus 3}
       ;; (plus 2 3) ; parsed result
       ;; 5

       ;; {2 plus 3 plus 4 plus 5 plus 6}
       ;; (plus 2 3 4 5 6)  ; parsed result
       ;; 20
       ((and infx ; true infix
	     sil ; simple infix list , when all operators are the same
	     (not mbr+-) ; could be there + - + + , superscripts ,so operators could be wrong
	     (not (ormap superscript-only? lyst)) ; a superscript (exponent) needs the general parser (superscript-only? : not * and · , that are also normal operators : {x := *} is (:= x *)) : {2 ⁴ ²} is (** 2 4 2) , not (⁴ 2 ²) , {- 2 ³} is (- (** 2 3))
	     (not (or (eq? oper 'not) (member-in-prefix-unary-lst oper))) ; {not not a} is (not (not a)) , {~ ~ a} is (~ (~ a)) : prefix operators (also those declared in the file) , grouped by the Scheme+ parser (group-prefix-operators)
	     (not (right-associative? oper))) ; {2 expt 3 expt 2} is not (expt 2 3 2) , expt has 2 arguments : the right associative operators are parsed by the precedence engine
					; {2 - + - 3 - 4} will not be parsed here but later !

	(define deep-terms (map (lambda (x) ; deep terms should be parsed by Scheme+
				  (!*prec-generic-infix-parser-rec-prepare x
									   (lambda (op a b)
									     (list op a b)))) ; creator
				operands))
	(cons oper
	      deep-terms))

       
       ;; general case
       
       (else ; apply nfx to the list
	#;(apply nfx lyst)
	(cond (srfi-strict (transform-mixed-infix lyst)) ; SRFI 105 : ($nfx$ a b c ...)
	      (region-quote (infix-top-level-only lyst)) ; in a quote only the top level is infix , the ( ) are data (Scheme+/care-of-quotes)
	      (else (apply nfx lyst))))))) ; execute (nfx a b c ...)
  


  
  (: curly-infix-read-real (:reader-proc: input-port -> *))
  (define (curly-infix-read-real no-indent-read port) ; TODO should we set! global-port here too ?
    (let* ((pos (get-sourceinfo port))
           (c   (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((eqv? c #\;)
          (consume-to-eol port)
          (curly-infix-read-real no-indent-read port))
        ((my-char-whitespace? c)
          (my-read-char port)
          (curly-infix-read-real no-indent-read port))
        ((eqv? c #\{)
          (my-read-char port)
          ; read in as infix
          #;(attach-sourceinfo pos
            (process-curly
              (my-read-delimited-list neoteric-read-real #\} port)))
	  (let ((lyst (my-read-delimited-list neoteric-read-real #\} port)))
	    (if (and (null? lyst)
		     strict-pragma-in-curly) ; {BEGIN-STRICT-SRFI-105-REGION} : the pragma is given back to the enclosing list , which drops it
		(let ((pragma strict-pragma-in-curly))
		  (set! strict-pragma-in-curly #f)
		  pragma)
		(begin
		  (set! strict-pragma-in-curly #f)
		  (attach-sourceinfo pos
		    (process-curly lyst))))))
        (else
          (underlying-read no-indent-read port)))))

  ; Read using curly-infix-read-real
  (define (curly-infix-read-nocomment port)
    (set! comment #f) ; for expr not in module (in module the module is the only expression ,so no need to reset the comment flag)
    ;; TODO postfix-flag ?
    (curly-infix-read-real curly-infix-read-nocomment port))

  
; -----------------------------------------------------------------------------
; Neoteric Expressions
; -----------------------------------------------------------------------------

  ; Implement neoteric-expression's prefixed (), [], and {}.
  ; At this point, we have just finished reading some expression, which
  ; MIGHT be a prefix of some longer expression.  Examine the next
  ; character to be consumed; if it's an opening paren, bracket, or brace,
  ; then the expression "prefix" is actually a prefix.
  ; Otherwise, just return the prefix and do not consume that next char.
  ; This recurses, to handle formats like f(x)(y).
  (: neoteric-process-tail (input-port * -> *))
  (define (neoteric-process-tail port prefix)
      (let* (#;(pos (get-sourceinfo port))
	     (pos #f) ; here the port is after the prefix : neoteric-read-real gives the location of the whole form
             (c   (my-peek-char port)))
        (cond
          ((eof-object? c) prefix)
	  ;;   f = prefix
          ((char=? c #\( ) ; Implement f(x)
            (my-read-char port)
            (neoteric-process-tail port (attach-sourceinfo pos (cons prefix
                  (my-read-delimited-list neoteric-read-nocomment #\) port)))))
          ((char=? c #\[ )  ; Implement f[x]
            (my-read-char port)
            (neoteric-process-tail port (attach-sourceinfo pos
              (cons (attach-sourceinfo pos '$bracket-apply$)
                (cons prefix
                  (my-read-delimited-list neoteric-read-nocomment #\] port))))))
          ((char=? c #\{ )  ; Implement f{x}
            (my-read-char port)
            (neoteric-process-tail port (attach-sourceinfo pos
							   (let
							       ((tail (process-curly
								       (my-read-delimited-list neoteric-read-nocomment #\} port))))
							     (if (eqv? tail '())
								 (list prefix) ; Map f{} to (f), not (f ()).
								 (list prefix tail))))))
          (else prefix))))


  ; This is the "real" implementation of neoteric-read.
  ; It directly implements unprefixed (), [], and {} so we retain control;
  ; it calls neoteric-process-tail so f(), f[], and f{} are implemented.
  (: neoteric-read-real (input-port -> *))
  (define (neoteric-read-real port)
    (let*
      ((pos (get-sourceinfo port))
       (c   (my-peek-char port))
       (result
         (cond
           ((eof-object? c) c)
           ((char=? c #\( )
             (my-read-char port)
             (attach-sourceinfo pos
               (my-read-delimited-list neoteric-read-nocomment #\) port)))
           ((char=? c #\[ )
             (my-read-char port)
             (attach-sourceinfo pos
               (my-read-delimited-list neoteric-read-nocomment #\] port)))
           ((char=? c #\{ )
             (my-read-char port)
             #;(attach-sourceinfo pos
               (process-curly
                 (my-read-delimited-list neoteric-read-nocomment #\} port)))
	     (let ((lyst (my-read-delimited-list neoteric-read-nocomment #\} port)))
	       (if (and (null? lyst)
			strict-pragma-in-curly) ; {BEGIN-STRICT-SRFI-105-REGION} : the pragma is given back to the enclosing list , which drops it
		   (let ((pragma strict-pragma-in-curly))
		     (set! strict-pragma-in-curly #f)
		     pragma)
		   (begin
		     (set! strict-pragma-in-curly #f)
		     (attach-sourceinfo pos
		       (process-curly lyst))))))
           ((my-char-whitespace? c)
             (my-read-char port)
             (neoteric-read-real port))
           ((eqv? c #\;)
             (consume-to-eol port)
             (neoteric-read-real port))
           (else (underlying-read neoteric-read-nocomment port)))))
      (if (eof-object? result)
          result
          #;(neoteric-process-tail port result)
	  (attach-sourceinfo pos (neoteric-process-tail port result)))))

  (: neoteric-read-nocomment (input-port -> *))
  (define (neoteric-read-nocomment port)
    ; should we put this line here too (already in curly-infix-read):
    ;(set! comment #f) ; for expr not in module (in module the module is the only expression ,so no need to reset the comment flag)
    (neoteric-read-real port))

; -----------------------------------------------------------------------------
; Sweet Expressions (this implementation maps to the BNF)
; -----------------------------------------------------------------------------

  ; Note: If your Lisp has macros, but doesn't support hygenic macros,
  ; it's probably trivial to reimplement this.  E.G., in Common Lisp:
  ; (defmacro let-splitter ((full first-value second-value) expr &rest body)
  ;   `(let* ((,full ,expr)
  ;           (,first-value (car ,full))
  ;           (,second-value (cadr ,full)))
  ;          ,@body))

  (define group-split (string->symbol "\\\\"))
  (define group-split-char #\\ ) ; First character of split symbol.
  (define non-whitespace-indent #\!) ; Non-whitespace-indent char.
  (define sublist (string->symbol "$"))
  (define sublist-char #\$) ; First character of sublist symbol.

  (: indentation>? (string string --> boolean))
  (define (indentation>? indentation1 indentation2)
    (let ((len1 (string-length indentation1))
          (len2 (string-length indentation2)))
      (and (> len1 len2)
             (string=? indentation2 (substring indentation1 0 len2)))))

  (: indentation>=? (string string --> boolean))
  (define (indentation>=? indentation1 indentation2)
    (or
      (string=? indentation1 indentation2)
      (indentation>? indentation1 indentation2)))

  ; Does character "c" begin a line comment (;) or end-of-line?
  (define initial-comment-eol (list #\; #\newline carriage-return))
  (define (lcomment-eol? c)
    (or
       (eof-object? c)
       (memv c initial-comment-eol)))

  ; Return #t if char is space or tab.
  (: char-hspace? (char --> boolean))
  (define (char-hspace? char)
    (or (eqv? char #\space)
        (eqv? char tab)))

  ; Consume 0+ spaces or tabs
  (: hspaces (input-port -> . *)) ;; FIXME returns undefined value.
  (define (hspaces port)
    (cond ; Use "cond" as "when" for portability.
      ((char-hspace? (my-peek-char port))
        (my-read-char port)
        (hspaces port))))

  ; Return #t if char is space, tab, or !
  (: char-hspace? (char --> boolean))
  (define (char-ichar? char)
    (or (eqv? char #\space)
        (eqv? char tab)
        (eqv? char non-whitespace-indent)))

  (: accumulate-ichar (input-port -> (list-of char)))
  (define (accumulate-ichar port)
    (if (char-ichar? (my-peek-char port))
        (cons (my-read-char port) (accumulate-ichar port))
        '()))

  (define (consume-ff-vt port)
    (let ((c (my-peek-char port)))
      (cond
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (my-read-char port)
          (consume-ff-vt port)))))

  ; Do 2-item append, but report read-error if the LHS is not a proper list.
  ; Don't use this if the lhs *must* be a list (e.g., if we have (list x)).
  ;; FIXME do we absolutely NEED this?  Looks rather expensive.  `list?`
  ;; FIXME should IMHO be avoided if all possible.
  (define (my-append lhs rhs)
    (cond
      ((eq? lhs empty-value) rhs)
      ((eq? rhs empty-value) lhs)
      ((list? lhs) (append lhs rhs))
      (else
        (read-error "Must have proper list on left-hand-side to append data"))))

  ; Read an n-expression.  Returns ('scomment '()) if it's an scomment,
  ; else returns ('normal n-expr).
  ; Note: If a *value* begins with #, process any potential neoteric tail,
  ; so weird constructs beginning with "#" like #f() will still work.
  (: n-expr-or-scomment (input-port -> :reader-token:))
  (define (n-expr-or-scomment port)
    (if (eqv? (my-peek-char port) #\#)
        (let* ((consumed-sharp (my-read-char port))
               (result (process-sharp neoteric-read-nocomment port)))
          (cond
	    ((not (pair? result)) (read-error "Unsupported hash"))
            ((eq? (car result) 'normal)
              (list 'normal (neoteric-process-tail port (cadr result))))
            ((eq? (car result) 'abbrev)
              (list 'normal
                (list (cadr result) (neoteric-read-nocomment port))))
            (else result)))
        (list 'normal (neoteric-read-nocomment port))))

  ; Read an n-expression.  Returns ('normal n-expr) in most cases;
  ; if it's a special marker, the car is the marker name instead of 'normal.
  ; Markers only have special meaning if their first character is
  ; the "normal" character, e.g., {$} is not a sublist.
  ; Call "process-sharp" if first char is "#".
  (: n-expr (input-port -> :reader-token:))
  (define (n-expr port)
    (let ((c (my-peek-char port)))
      (let-splitter (results type expr)
                    (n-expr-or-scomment port)
        (if (eq? (car results) 'scomment)
            results
            (cond
              ((and (eq? expr sublist) (eqv? c sublist-char))
                (list 'sublist-marker '()))
              ((and (eq? expr group-split) (eqv? c group-split-char))
                (list 'group-split-marker '()))
              ((and (eq? expr '<*) (eqv? c #\<))
                (list 'collecting '()))
              ((and (eq? expr '*>) (eqv? c #\*))
                (list 'collecting-end '()))
              ((and (eq? expr '$$$) (eqv? c #\$))
                (read-error "$$$ is reserved"))
              ((and (eq? expr period-symbol) (eqv? c #\.))
                (list 'period-marker '()))
              (else
                results))))))

  ; Check if we have abbrev+whitespace.  If the current peeked character
  ; is one of certain whitespace chars,
  ; return 'abbrevw as the marker and abbrev-procedure
  ; as the value (the cadr). Otherwise, return ('normal n-expr).
  ; We do NOT consume the peeked char (so EOL can be examined later).
  ; Note that this calls the neoteric-read procedure directly, because
  ; quoted markers are no longer markers. E.G., '$ is just (quote $).
  (: maybe-initial-abbrev (input-port * -> :reader-token:))
  (define (maybe-initial-abbrev port abbrev-procedure)
    (let ((c (my-peek-char port)))
      (if (or (char-hspace? c) (eqv? c carriage-return) (eqv? c linefeed))
          (list 'abbrevw abbrev-procedure)
          (list 'normal
            (list abbrev-procedure (neoteric-read-nocomment port))))))

  ; Read the first n-expr on a line; handle abbrev+whitespace specially.
  ; Returns ('normal VALUE) in most cases.
  (: n-expr-first (input-port -> :reader-token:))
  (define (n-expr-first port)
    (case (my-peek-char port)
      ((#\') 
        (my-read-char port)
        (maybe-initial-abbrev port 'quote))
      ((#\`) 
        (my-read-char port)
        (maybe-initial-abbrev port (translate-cl 'quasiquote)))
      ((#\,) 
        (my-read-char port)
        (if (eqv? (my-peek-char port) #\@)
            (begin
              (my-read-char port)
              (maybe-initial-abbrev port (translate-cl 'unquote-splicing)))
            (maybe-initial-abbrev port (translate-cl 'unquote))))
      ((#\#) 
        (let* ((consumed-sharp (my-read-char port))
               (result (process-sharp neoteric-read-nocomment port)))
          (cond
            ((eq? (car result) 'normal)
              (list 'normal (neoteric-process-tail port (cadr result))))
            ((eq? (car result) 'abbrev)
              (maybe-initial-abbrev port (cadr result)))
            (else result))))
      (else ; not cond expression
        (n-expr port))))

  ; Consume ;-comment (if there), consume EOL, and return new indent.
  ; Skip ;-comment-only lines; a following indent-only line is empty.
  (: get-next-indent (input-port -> string))
  (define (get-next-indent port)
    (consume-to-eol port)
    (consume-end-of-line port)
    (let* ((indentation-as-list (cons #\^ (accumulate-ichar port)))
           (c (my-peek-char port)))
      (cond
        ((eqv? c #\;)  ; A ;-only line, consume and try again.
          (get-next-indent port))
        ((lcomment-eol? c) ; Indent-only line
          (if (memv #\! indentation-as-list)
              (get-next-indent port)
              "^"))
        (else (list->string indentation-as-list)))))

  ; Read and throw away "skippable" content (text that is semantically the
  ; same as whitespace but is complex).  This implements the sequence:
  ;   (scomment hs | datum-commentw hs n-expr hs)
  (: skippable (symbol input-port -> . *)) ; FIXME: incomplete result type
  (define (skippable stopper port)
    (cond
    ((eq? stopper 'scomment)
      (hspaces port))
    ((eq? stopper 'datum-commentw)
      (hspaces port)
      (if (not (lcomment-eol? (my-peek-char port)))
        (begin
          (n-expr port)
          (hspaces port))
        (read-error "Datum comment start not followed a datum (EOL instead)")))
    (else (read-error "skippable: Impossible case"))))

  ; Utility declarations and functions

  (define empty-value (string-copy "empty-value")) ; Represent no value at all

  (define (conse x y) ; cons, but handle "empty" values
    (cond
      ((eq? y empty-value) x)
      ((eq? x empty-value) y)
      (else (cons x y))))

  (define (appende x y) ; append, but handle "empty" values
    (cond
      ((eq? y empty-value) x)
      ((eq? x empty-value) y)
      (else (append x y))))

  (define (list1e x) ; list, but handle "empty" values
    (if (eq? x empty-value)
        '()
        (list x)))

  (define (list2e x y) ; list, but handle "empty" values
    (if (eq? x empty-value)
        y
        (if (eq? y empty-value)
           x
           (list x y))))

  ; If x is a 1-element list, return (car x), else return x
  (define (monify x)
    (cond
      ((not (pair? x)) x)
      ((null? (cdr x)) (car x))
      (else x)))

  ; Read the contents of a collecting list and return it.
  ; Precondition: Have already read collecting start and horizontal spaces.
  ; Postcondition: Consumed the matching COLLECTING_END.
  ; Return contents (value) of collecting-content.  It does *not* report a
  ; stopper or ending indent, because it is *ONLY* stopped by collecting-end
  (: collecting-content (input-port -> *))
  (define (collecting-content port)
    (let* ((c (my-peek-char port)))
      (cond
        ((eof-object? c)
         (read-error "Collecting tail: EOF before collecting list ended"))
        ((lcomment-eol? c)
          (consume-to-eol port)
          (consume-end-of-line port)
          (collecting-content port))
        ((char-ichar? c)
          (let* ((indentation (accumulate-ichar port))
                 (c (my-peek-char port)))
            (if (lcomment-eol? c)
                (collecting-content port)
                (read-error "Collecting tail: Only ; after indent"))))
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (consume-ff-vt port)
          (if (lcomment-eol? (my-peek-char port))
              (collecting-content port)
              (read-error "Collecting tail: FF and VT must be alone on line")))
        (else
          (let-splitter (it-full-results it-new-indent it-value)
                        (it-expr port "^")
            (cond
              ((string=? it-new-indent "")
                ; Specially compensate for "*>" at the end of a line if it's
                ; after something else.  This must be interpreted as EOL *>,
                ; which would cons a () after the result.
                ; Directly calling list for a non-null it-value has
                ; the same effect, but is a lot quicker and simpler.
                (cond
                  ((null? it-value) it-value)
                  ((eq? it-value empty-value) '())
                  (else (list it-value))))
              (else (conse it-value (collecting-content port)))))))))

  ; Skip scomments and error out if we have a normal n-expr, implementing:
  ;    skippable* (n-expr error)?
  (: n-expr-error (input-port pair -> :reader-token:))
  (define (n-expr-error port full)
    (if (not (eq? (car full) 'normal))
        (read-error "BUG! n-expr-error called but stopper not normal"))
    (if (lcomment-eol? (my-peek-char port))
        full ; All done!
        (let-splitter (n-full-results n-stopper n-value)
                      (n-expr port)
          (cond
            ((or (eq? n-stopper 'scomment) (eq? n-stopper 'datum-commentw))
              (skippable n-stopper port)
              (n-expr-error port full))
            ((eq? n-stopper 'normal)
              (read-error "Illegal second value after ."))
            (else ; We found a stopper, return it with the value from "full"
              (list n-stopper (cadr full)))))))

  ; Read input after a lone ".", normally exactly one datum.
  ; Returns (stopper value-after-period)
  (: post-period (input-port -> :reader-token:))
  (define (post-period port)
    (if (not (lcomment-eol? (my-peek-char port)))
        (let-splitter (pn-full-results pn-stopper pn-value)
                      (n-expr port)
          (cond
            ((or (eq? pn-stopper 'scomment) (eq? pn-stopper 'datum-commentw))
              (skippable pn-stopper port)
              (post-period port))
            ((eq? pn-stopper 'normal)
              (hspaces port)
              (n-expr-error port pn-full-results))
            ((eq? pn-stopper 'collecting)
              (hspaces port)
              (let ((cl (collecting-content port)))
                (hspaces port)
                (n-expr-error port (list 'normal cl))))
            ((eq? pn-stopper 'period-marker)
              (list 'normal period-symbol))
            (else ; Different stopper; respond as empty branch with that stopper
              (list pn-stopper (list period-symbol)))))
        (list 'normal period-symbol))) ; Empty branch.


  ; Read the 1+ n-expressions on one line, and return them as a list.
  ; If there is exactly one n-expression on the line,
  ; it returns a list of exactly one item.
  ; Precondition: At beginning of line after indent
  ; Postcondition: At unconsumed EOL
  ; Returns (stopper computed-value).
  ; The stopper may be 'normal, 'scomment (special comment),
  ; 'abbrevw (initial abbreviation), 'sublist-marker, or 'group-split-marker
  (: line-exprs (input-port string -> :reader-token:))
  (define (line-exprs port indent)
    (let-splitter (basic-full-results basic-special basic-value)
                  (n-expr-first port)
      (cond
        ((eq? basic-special 'collecting)
          (hspaces port)
          (let* ((cl-results (collecting-content port)))
            (hspaces port)
            (if (not (lcomment-eol? (my-peek-char port)))
                (let-splitter (rr-full-results rr-stopper rr-value)
                              (rest-of-line port indent)
                  (list rr-stopper (cons cl-results rr-value)))
                (list 'normal (list cl-results)))))
        ((eq? basic-special 'period-marker)
          (if (char-hspace? (my-peek-char port))
              (begin
                (hspaces port)
                (let-splitter (ct-full-results ct-stopper ct-value)
                              (post-period port)
                  (list ct-stopper (list ct-value))))
              (list 'normal (list period-symbol))))
        ((not (eq? basic-special 'normal)) basic-full-results)
        ((char-hspace? (my-peek-char port))
          (hspaces port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (let-splitter (br-full-results br-stopper br-value)
                            (rest-of-line port indent)
                (list br-stopper (cons basic-value br-value)))
              (list 'normal (list basic-value))))
        (else
          (list 'normal (list basic-value))))))

  ; Read the rest of the expressions on a line,
  ; after the first expression of the line.  This supports line-exprs.
  ; Precondition: At beginning of non-first expression on line (past hspace)
  ; Postcondition: At unconsumed EOL
  ; Returns (stopper computed-value); stopper may be 'normal, etc.
  ; Read in one n-expr, then process based on whether or not it's special.
  (: rest-of-line (input-port string -> :reader-token:))
  (define (rest-of-line port indent)
    (let-splitter (basic-full-results basic-special basic-value)
                  (n-expr port)
      (cond
        ((or (eq? basic-special 'scomment) (eq? basic-special 'datum-commentw))
          (skippable basic-special port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (rest-of-line port indent)
              (list 'normal '())))
        ((eq? basic-special 'collecting)
          (hspaces port)
          (let* ((cl-results (collecting-content port)))
            (hspaces port)
            (if (not (lcomment-eol? (my-peek-char port)))
                (let-splitter (rr-full-results rr-stopper rr-value)
                              (rest-of-line port indent)
                  (list rr-stopper (cons cl-results rr-value)))
                (list 'normal (list cl-results)))))
        ((eq? basic-special 'period-marker)
          (if (char-hspace? (my-peek-char port))
              (begin
                (hspaces port)
                (post-period port))
              ; (list 'normal (list period-symbol)) ; To interpret as |.|
              (read-error "Cannot end line with '.'")))
        ((eq? basic-special 'group-split-marker)
          ; Local extension - allow \\ as line-continuation, a
          ; capability useful in Common Lisp.
          ; Add this here to be consistent with Common Lisp implementation.
          ; This is *NOT* a SRFI-110 requirement!!
          (hspaces port)
          (if (lcomment-eol? (my-peek-char port))
            (let ((new-indent (get-next-indent port)))
              (if (indentation>=? new-indent indent)
                (rest-of-line port indent)
                (read-error "Line continuation indentation is inconsistent.")))
            (list basic-special '())))
        ((not (eq? basic-special 'normal)) (list basic-special '()))
        ((char-hspace? (my-peek-char port))
          (hspaces port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (let-splitter (br-full-results br-stopper br-value)
                            (rest-of-line port indent)
                (list br-stopper (cons basic-value br-value)))
              (list 'normal (list basic-value))))
        (else (list 'normal (list basic-value))))))

  ; Read the sequence of 1+ child lines in an it_expr
  ; (e.g., after "line_expr"), each of which is itself an it_expr.
  ; It returns the list of expressions in the body and the new indent as
  ; (new-indent computed-value).
  ; We name this "read-body", not "body", to avoid an error in rscheme.
  (: read-body (input-port string -> :reader-token:))
  ; an element of an indented body , treated as an element of a list ( ) in my-read-delimited-list : the pragmas
  ; ANNOTATION-ON / ANNOTATION-OFF (kept as markers) , the pragmas of the strict SRFI 105 mode (removed) , the
  ; declarations for the infix parser of Scheme+ ; returns the datum , or empty-value to remove it
  (define (process-indented-datum datum)
    (cond ((eq? datum 'BEGIN-STRICT-SRFI-105-REGION)
	   (set! srfi-strict #t)
	   empty-value)
	  ((eq? datum 'END-STRICT-SRFI-105-REGION)
	   (set! srfi-strict #f)
	   empty-value)
	  (else
	   (process-annotation-pragma datum)
	   (verify-if-declaration-for-parser-then-store-it datum)
	   datum)))

  (define (read-body port starting-indent)
    (let-splitter (i-full-results i-new-indent i-value-read)
                  (it-expr port starting-indent)
     (let ((i-value (process-indented-datum i-value-read))) ; the element is processed before the next ones are read
      (if (string=? starting-indent i-new-indent)
          (if (eq? i-value period-symbol)
              (let-splitter (f-full-results f-new-indent f-value)
                            (it-expr port i-new-indent)
                (if (not (indentation>? starting-indent f-new-indent))
                    (read-error "Dedent required after lone . and value line"))
                (list f-new-indent f-value)) ; final value of improper list
              (if (eq? i-value empty-value)
                (read-body port i-new-indent)
                (let-splitter (nxt-full-results nxt-new-indent nxt-value)
                              (read-body port i-new-indent)
                  (list nxt-new-indent (cons i-value nxt-value)))))
          (list i-new-indent (list1e i-value)))))) ; dedent - end list.


  ; Read a sweet-expression that doesn't have a special prefix.
  ; Returns (new-indent computed-value)
  (: it-expr-real (input-port string -> :reader-indent-token:))
  (define (it-expr-real port starting-indent)
    (let-splitter (line-full-results line-stopper line-value)
                  (line-exprs port starting-indent)
      (if (and (not (null? line-value)) (not (eq? line-stopper 'abbrevw)))
          ; Production line-exprs produced at least one n-expression:
          (cond
            ((eq? line-stopper 'group-split-marker)
              (hspaces port)
              ; This error can't happen due to \\ line continuation extension,
	      ; but we will test it just in case:
              (if (lcomment-eol? (my-peek-char port))
                  (read-error "Cannot follow split with end of line")
                  (list starting-indent (monify line-value))))
            ((eq? line-stopper 'sublist-marker)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (read-error "EOL illegal immediately after sublist"))
              (let-splitter (sub-i-full-results sub-i-new-indent sub-i-value)
                            (it-expr port starting-indent)
                (list sub-i-new-indent
                  (my-append line-value (list sub-i-value)))))
            ((eq? line-stopper 'collecting-end)
              ; Note that indent is "", forcing dedent all the way out.
              (list ""
                (if (eq? line-value empty-value)
                  '()
                  (monify line-value))))
            ((lcomment-eol? (my-peek-char port))
              (let ((new-indent (get-next-indent port)))
                (if (indentation>? new-indent starting-indent)
                    (let-splitter (body-full body-new-indent body-value)
                                  (read-body port new-indent)
                      (list body-new-indent (my-append line-value body-value)))
                    (list new-indent (monify line-value)))))
            (else
              (read-error
                (string-append
                   "Unexpected text after n-expression around character "
                   (let ((c (my-peek-char port)))
                     (if (< (char->integer c) 32)
                       (string-append "#\\x"
                         (number->string (char->integer c) 16))
                       (string c)))))))
          ; line-exprs begins with something special like GROUP-SPLIT:
          (cond
            ((eq? line-stopper 'datum-commentw)
              (hspaces port)
              (cond
                ((not (lcomment-eol? (my-peek-char port)))
                  (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                                (it-expr port starting-indent)
                    (list is-i-new-indent empty-value)))
                (else
                  (let ((new-indent (get-next-indent port)))
                    (if (indentation>? new-indent starting-indent)
                      (let-splitter
                           (body-full-results body-new-indent body-value)
                           (read-body port new-indent)
                        (list body-new-indent empty-value))
                      (read-error "#;+EOL must be followed by indent"))))))
            ((or (eq? line-stopper 'group-split-marker)
                 (eq? line-stopper 'scomment))
              (hspaces port)
              (if (not (lcomment-eol? (my-peek-char port)))
                  (it-expr port starting-indent) ; Skip and try again.
                  (let ((new-indent (get-next-indent port)))
                    (cond
                      ((indentation>? new-indent starting-indent)
                        (read-body port new-indent))
                      (else
                        (list new-indent empty-value))))))
            ((eq? line-stopper 'sublist-marker)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (read-error "EOL illegal immediately after solo sublist"))
              (let-splitter (is-i-full-results is-i-new-indent is-i-value)
                            (it-expr port starting-indent)
                (list is-i-new-indent (list1e is-i-value))))
            ((eq? line-stopper 'abbrevw)
              (hspaces port)
              (if (lcomment-eol? (my-peek-char port))
                  (let ((new-indent (get-next-indent port)))
                    (if (not (indentation>? new-indent starting-indent))
                        (read-error "Indent required after abbreviation"))
                    (let-splitter (ab-full-results ab-new-indent ab-value)
                                  (read-body port new-indent)
                      (list ab-new-indent
                        (append (list line-value) ab-value))))
                  (let-splitter (ai-full-results ai-new-indent ai-value)
                                (it-expr port starting-indent)
                    (list ai-new-indent
                      (list2e line-value ai-value)))))
            ((eq? line-stopper 'collecting-end)
              (list "" line-value))
            (else
              (read-error "Initial line-expression error"))))))

  ; Read it-expr.  This is a wrapper that attaches source info
  ; and checks for consistent indentation results, then calls it-expr-real.
  (: it-expr (input-port string -> :reader-indent-token:))
  (define (it-expr port starting-indent)
    (let ((pos (get-sourceinfo port)))
      (let-splitter (results results-indent results-value)
                    (it-expr-real port starting-indent)
        (if (indentation>? results-indent starting-indent)
            (read-error "Inconsistent indentation"))
        (list results-indent (attach-sourceinfo pos results-value)))))

  ; Read the rest of an initial-indent-expr (a sweet-expression with
  ; a special initial value).
  (: initial-indent-expr-tail :reader-proc:)
  (define (initial-indent-expr-tail port)
    (if (not (memv (my-peek-char port) initial-comment-eol))
        (let-splitter (results results-stopper results-value)
                      (n-expr-or-scomment port)
          (cond
            ((memq results-stopper '(scomment datum-commentw))
              (skippable results-stopper port)
              (initial-indent-expr-tail port))
            (else results-value))) ; Normal n-expr, return one value.
        (begin
          (consume-to-eol port)
          (consume-end-of-line port)
          empty-value))) ; (t-expr-real port)

  ; Read a sweet-expression (t-expression).  Handle special
  ; cases, such as initial indent; call it-expr for normal case.
  (: t-expr-real :reader-proc:)
  (define (t-expr-real port)
    (let* ((c (my-peek-char port)))
      (cond
        ; Check EOF early (a bug in guile before 2.0.8 consumes EOF on peek)
        ((eof-object? c) c)
        ((lcomment-eol? c)
          (consume-to-eol port)
          (consume-end-of-line port)
          (t-expr-real port))
        ((or (eqv? c form-feed) (eqv? c vertical-tab))
          (consume-ff-vt port)
          (if (not (lcomment-eol? (my-peek-char port)))
              (read-error "FF and VT must be alone on line in a sweet-expr"))
          (t-expr-real port))
        ((char-ichar? c) ; initial-indent-expr
          (accumulate-ichar port) ; consume and throw away ichars
          (initial-indent-expr-tail port))
        (else
          (let-splitter (results results-indent results-value)
                        (it-expr port "^")
            (if (string=? results-indent "")
                (read-error "Closing *> without preceding matching <*")
                results-value))))))

  ; Top level - read a sweet-expression (t-expression).  Handle special values
  (: t-expr :reader-proc:)
  (define (t-expr port)
    (let* ((te (t-expr-real port)))
      (if (eq? te empty-value)
          (t-expr port)
          te)))

  ; Skip until we find a line with 0 indent characters.
  ; We use this after read error to resync to good input.
  (: read-to-unindented-line :reader-proc:)
  (define (read-to-unindented-line port)
    (let* ((c (my-peek-char port)))
      (cond
        ((eof-object? c) c)
        ((char-line-ending? c)
          (consume-end-of-line port)
          (if (char-ichar? (my-peek-char port))
            (read-to-unindented-line port)))
        (else
          (consume-to-eol port)
          (consume-end-of-line port)
          (read-to-unindented-line port)))))

  ; Call on sweet-expression reader - use exceptions
  ; so that errors will force a restart.
  (: t-expr-catch :reader-proc:)
  (define (t-expr-catch port)

    (init-sweet)
    (set! comment #f) ; as in curly-infix-read-nocomment
    (set! in-list #f)

    (guard
      (exception
        ((eq? exception 'readable)
         (read-to-unindented-line port) (t-expr-catch port)))
      (t-expr port)))

; -----------------------------------------------------------------------------
; Write routines
; -----------------------------------------------------------------------------

  ; A list with more than this length and no pairs is considered "boring",
  ; and thus is presumed to NOT be a procedure call or execution sequence.
  (define boring-length 16)

  (define special-infix-operators
    '(and or xor))

  (define punct-chars
    (list #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\-
          #\.  #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\\ #\] #\^
          #\_ #\` #\{ #\| #\} #\~))

  ; Returns #t if x is a list with exactly 1 element.  Improper lists are #f.
  (define (list1? x)
    (and (pair? x) (null? (cdr x))))

  ; Returns #t if x is a list with exactly 2 elements.  Improper lists are #f.
  (define (list2? x)
    (and (pair? x) (pair? (cdr x)) (null? (cddr x))))

  ; Does x contain a list of ONLY punctuation characters?
  ; An empty list is considered true.
  (define (contains-only-punctuation? x)
    (cond ((null? x) #t)
          ((not (pair? x)) #f)
          ((memq (car x) punct-chars)
           (contains-only-punctuation? (cdr x)))
          (else #f)))

  ; Returns #t if x is a symbol that would typically be used in infix position.
  (define (is-infix-operator? x)
    (cond ((not (symbol? x)) #f)
          ((memq x special-infix-operators) #t)
          (else
           (contains-only-punctuation?
             (string->list (symbol->string x))))))

  ; A possibly-improper list is long and boring if its length is at least
  ; num-to-go long and it's boring (it contains no pairs up to that length).
  ; A long-and-boring list is almost certainly NOT a function call or a
  ; body of some executable sequence - it's almost certainly a long
  ; boring list of data instead. If it is, we want to display it differently.
  ; This doesn't get stuck on circular lists; it always terminates after
  ; num-to-go iterations.
  (define (long-and-boring? x num-to-go)
    (cond
      ((pair? (car x)) #f)
      ((not (pair? (cdr x))) #f)
      ((<= num-to-go 1) #t)
      (else (long-and-boring? (cdr x) (- num-to-go 1)))))

  (define (list-no-longer-than? x num-to-go)
    (cond
      ((not (pair? x)) #f)
      ((null? (cdr x)) #t) ; This is the last one!
      ((not (pair? (cdr x))) #f)
      ((<= num-to-go 0) #f)
      (else (list-no-longer-than? (cdr x) (- num-to-go 1)))))

  ; Return #t if x should be represented using curly-infix notation {...}.
  (define (represent-as-infix? x)
    (and (pair? x)
         (pair? (cdr x))                ; At least 2 elements.
         (is-infix-operator? (car x))
         (list-no-longer-than? x 6)))

  (define (represent-as-inline-infix? x)
    (and (represent-as-infix? x) (not (list2? x)))) ; Must be 3+ elements

  ; Return #t if x should be represented as a brace suffix
  (define (represent-as-brace-suffix? x)
    (represent-as-infix? x))

  ; Define an association list mapping the Scheme procedure names which have
  ; abbreviations ==> the list of characters in their abbreviation
  (define abbreviations
    '((quote (#\'))
      (quasiquote (#\`))
      (unquote (#\,))
      (unquote-splicing (#\, #\@))
      ; Scheme syntax-rules. Note that this will abbreviate any 2-element
      ; list whose car is "syntax", whether you want that or not!
      (syntax  (#\# #\'))
      (quasisyntax (#\# #\`))
      (unsyntax-splicing (#\# #\, #\@)
      (unsyntax (#\# #\,)))))

  ; return #t if we should as a traditional abbreviation, e.g., '
  (define (represent-as-abbreviation? x)
    (and (list2? x)
         (assq (car x) abbreviations)))

  ; The car(x) is the symbol for an abbreviation; write the abbreviation.
  (define (write-abbreviation x port)
    (for-each (lambda (c) (display c port))
      (cadr (assq (car x) abbreviations))))


  ; Return list x's *contents* represented as a list of characters.
  ; Each one must use neoteric-expressions, space-separated;
  ; it will be surrounded by (...) so no indentation processing is relevant.
  (define (n-write-list-contents x port)
    (cond
      ((null? x) (no-values))
      ((pair? x)
        (n-write-simple (car x) port)
        (cond ((not (null? (cdr x)))
          (display " " port)
          (n-write-list-contents (cdr x) port))))
      (else
        (display ". " port)
        (n-write-simple x port))))

  (define (c-write-list-contents x port)
    (cond
      ((null? x) (no-values))
      ((pair? x)
        (c-write-simple (car x) port)
        (cond ((not (null? (cdr x)))
          (display " " port)
          (c-write-list-contents (cdr x) port))))
      (else
        (display ". " port)
        (c-write-simple x port))))

  ; Return tail of an infix expression, as list of chars
  ; The "op" is the infix operator represented as a list of chars.
  (define (infix-tail op x port)
    (cond
      ((null? x) (display "}" port))
      ((pair? x)
        (display " " port)
        (n-write-simple op port)
        (display " " port)
        (n-write-simple (car x) port)
        (infix-tail op (cdr x) port))
      (else
        (display " " port)
        (n-write-simple x port)
        (display "}" port))))

  ; Return "x" as a list of characters, surrounded by {...}, for use as f{...}.
  (define (as-brace-suffix x port)
    (display "{" port)
    (if (list2? x)
      (begin
        (n-write-list-contents x port)
        (display "}" port))
      (begin
        (n-write-simple (cadr x) port)
        (infix-tail (car x) (cddr x) port))))

  (define (n-write-simple x port)
    (cond
      ((pair? x)
        (cond
          ((represent-as-abbreviation? x)              ; Format 'x
            (write-abbreviation x port)
            (n-write-simple (cadr x) port))
          ((long-and-boring? x boring-length)          ; Format (a b c ...)
            (display "(" port)
            (n-write-list-contents x port)
            (display ")" port))
          ((symbol? (car x))
            (cond
              ((represent-as-inline-infix? x)          ; Format {a + b}
                (display "{" port)
                (n-write-simple (cadr x) port)
                (infix-tail (car x) (cddr x) port))
              ((and (list1? (cdr x))                   ; Format f{...}
                (pair? (cadr x))
                (represent-as-infix? (cadr x)))
                  (n-write-simple (car x) port)
                  (as-brace-suffix (cadr x) port))
              (else                                    ; Format f(...)
                (n-write-simple (car x) port)
                (display "(" port)
                (n-write-list-contents (cdr x) port)
                (display ")" port))))
          (else                                        ; Format (1 2 3 ...)
            (display "(" port)
            (n-write-list-contents x port)
            (display ")" port))))
      ((vector? x)
        (display "#( " port) ; Surround with spaces, easier to implement.
        (for-each (lambda (v) (n-write-simple v port) (display " " port))
          (vector->list x))
        (display ")" port))
      (else (write x port))))                          ; Default format.


  (define (c-write-simple x port)
    (cond
      ((pair? x)
        (cond
          ((represent-as-abbreviation? x)              ; Format 'x
            (write-abbreviation x port)
            (c-write-simple (cadr x) port))
          ((represent-as-inline-infix? x)              ; Format {a + b}
            (display "{" port)
            (n-write-simple (cadr x) port)
            (infix-tail (car x) (cddr x) port))
          (else                                        ; Format (1 2 3 ...)
            (display "(" port)
            (c-write-list-contents x port)
            (display ")" port))))
      ((vector? x)
        (display "#( " port) ; Surround with spaces, easier to implement.
        (for-each (lambda (v) (c-write-simple v port) (display " " port))
          (vector->list x))
        (display ")" port))
      (else (write x port))))                          ; Default format.


  ; Front entry - Use default port if none provided.
  (define (neoteric-write-simple x . rest)
    (if (pair? rest)
        (n-write-simple x (car rest))
        (n-write-simple x (current-output-port))))

  ; Front entry - Use default port if none provided.
  (define (curly-write-simple x . rest)
    (if (pair? rest)
        (c-write-simple x (car rest))
        (c-write-simple x (current-output-port))))


  ;; Write routines for cyclic and shared structures, based on srfi-38.
  ;; The original code was written by Alex Shinn in 2009 and placed in the
  ;; Public Domain.  All warranties are disclaimed.
  ;; Extracted from Chibi Scheme 0.6.1.


  ; We need hash tables for an efficient implementation.
  ; John Cowan on 10 April 2013 said:
  ; "... though not part of R7RS-small,
  ; SRFI 69 hash tables are fairly pervasive, because they are
  ; simple, have a reference implementation, and can be built on top of
  ; R6RS hashtables (which are standard). The only reason they aren't in
  ; more Schemes is that as SRFIs go, 69 is a fairly recent one. I wouldn't
  ; hesitate to use them."

  (define (extract-shared-objects x cyclic-only?)
    (let ((seen (srfi-69-make-hash-table eq?)))
      ;; find shared references
      (let find ((x x))
        (let ((type (type-of x)))
          (cond ;; only interested in pairs, vectors and records
           ((or (pair? x) (vector? x) (and type (type-printer type)))
            ;; increment the count
            (hash-table-update!/default seen x (lambda (n) (+ n 1)) 0)
            ;; walk if this is the first time
            (cond
             ((> (hash-table-ref seen x) 1))
             ((pair? x)
              (find (car x))
              (find (cdr x)))
             ((vector? x)
              (do ((i 0 (+ i 1)))
                  ((= i (vector-length x)))
                (find (vector-ref x i))))
             (else
              (let ((num-slots (type-num-slots type)))
                (let lp ((i 0))
                  (cond ((< i num-slots)
                         (find (slot-ref type x i))
                         (lp (+ i 1))))))))
            ;; delete if this shouldn't count as a shared reference
            (if (and cyclic-only?
                     (<= (hash-table-ref/default seen x 0) 1))
                (hash-table-delete! seen x))))))
      ;; extract shared references
      (let ((res (srfi-69-make-hash-table eq?)))
        (hash-table-walk
         seen
         (lambda (k v) (if (> v 1) (hash-table-set! res k #t))))
        res)))

  (define (advanced-write-with-shared-structure x port cyclic-only? neoteric?)
    (let ((shared (extract-shared-objects x cyclic-only?))
          (count 0))
      ; Returns #t if this part is shared.
      (define (shared-object? x)
        (let ((type (type-of x)))
          (if (or (pair? x) (vector? x) (and type (type-printer type)))
            (let ((index (hash-table-ref/default shared x #f)))
              (not (not index)))
            #f)))
      ; Check-shared prints #n# or #n= as appropriate.
      (define (check-shared x prefix cont)
        (let ((index (hash-table-ref/default shared x #f)))
          (cond ((integer? index)
                 (display prefix port)
                 (display "#" port)
                 (write index port)
                 (display "#" port))
                (else
                 (cond (index
                        (display prefix port)
                        (display "#" port)
                        (write count port)
                        (display "=" port)
                        (hash-table-set! shared x count)
                        (set! count (+ count 1))))
                 (cont x index)))))
      (let wr ((x x) (neoteric? neoteric?))
        (define (infix-tail/ss op x port)
          (cond
            ((null? x) (display "}" port))
            ((pair? x)
              (display " " port)
              (wr op #t)
              (display " " port)
              (wr (car x) #t) ; Always neoteric inside infix list.
              (infix-tail/ss op (cdr x) port))
            (else
              (display " .  " port)
              (n-write-simple x port)
              (display "}" port))))
        (check-shared
         x
         ""
         (lambda (x shared?)
           (cond
            ; Check for special formats first.
            ((and (represent-as-abbreviation? x) (not (shared-object? (cadr x))))
              ; Format 'x
              (write-abbreviation x port)
              (wr (cadr x) neoteric?))
            ((and (represent-as-inline-infix? x) (not (any shared-object? x)))
              ; Format {a + b}
              (display "{" port)
              (wr (cadr x) #t) ; Always neoteric inside {...}
              (infix-tail/ss (car x) (cddr x) port))
            ((and neoteric? (pair? x) (symbol? (car x)) (list1? (cdr x))
              (represent-as-infix? (cadr x))
              (not (shared-object? (cdr x)))
              (not (shared-object? (cadr x)))
              (not (any shared-object? (cadr x))))
              ; Format f{...}
              (wr (car x) neoteric?)
              (let ((expr (cadr x)))
                (if (list2? expr)
                  (begin
                    (display "{" port)
                    (wr (car expr) neoteric?)
                    (display " " port)
                    (wr (cadr expr) neoteric?)
                    (display "}" port))
                  (begin
                    (wr expr neoteric?)))))
            ((pair? x)
             ; Some format ending with closing paren - which one is it?
             (cond
               ((and neoteric? (symbol? (car x))
                 (not (long-and-boring? x boring-length))
                 (not (shared-object? (cdr x)))) ; I don't like the way it looks
                 ; Neoteric format a(b c ...)
                 (wr (car x) neoteric?)
                 (display "(" port)) ; )
               (else
                 ; Default format, (a b c ...)
                 (display "(" port) ; )
                 (wr (car x) neoteric?)
                 (if (not (null? (cdr x))) (display " " port))))
             (let lp ((ls (cdr x)))
               (check-shared
                ls
                ". "
                (lambda (ls shared?)
                  (cond ((null? ls))
                        ((pair? ls)
                         (cond
                          (shared?
                           (display "(" port)
                           (wr (car ls) neoteric?)
                           (check-shared
                            (cdr ls)
                            ". "
                            (lambda (ls shared?)
                              (if (not (null? ls)) (display " " port))
                              (lp ls)))
                           (display ")" port))
                          (else
                           (wr (car ls) neoteric?)
                           (if (not (null? (cdr ls))) (display " " port))
                           (lp (cdr ls)))))
                        (else
                         (display ". " port)
                         (wr ls neoteric?))))))
             (display ")" port))
            ((vector? x)
             (display "#(" port)
             (let ((len (vector-length x)))
               (cond ((> len 0)
                      (wr (vector-ref x 0) neoteric?)
                      (do ((i 1 (+ i 1)))
                          ((= i len))
                        (display " " port)
                        (wr (vector-ref x i) neoteric?)))))
             (display ")" port))
            ((let ((type (type-of x)))
               (and (type? type) (type-printer type)))
             => (lambda (printer) (printer x wr port)))
            (else
             (write x port))))))))


  (define (curly-write-shared x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #f #f))

  (define (curly-write-cyclic x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #t #f))

  (define (neoteric-write-shared x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #f #t))

  (define (neoteric-write-cyclic x . o)
    (advanced-write-with-shared-structure x
      (if (pair? o) (car o) (current-output-port))
      #t #t))

  ; Since we have "cyclic" versions, the -write forms use them:
  (define neoteric-write neoteric-write-cyclic)
  (define curly-write curly-write-cyclic)


; -----------------------------------------------------------------------------
; Exported Interface
; -----------------------------------------------------------------------------

  (define curly-infix-read (make-read curly-infix-read-nocomment))
  (define neoteric-read (make-read neoteric-read-nocomment))
  ; the pragmas of the strict SRFI 105 mode written alone at the level of the file (outside a module form) are removed
  #;(define (t-expr-catch-pragmas port)
    (let ((datum (t-expr-catch port)))
      (cond ((eq? datum 'BEGIN-STRICT-SRFI-105-REGION)
	     (set! srfi-strict #t)
	     (t-expr-catch-pragmas port))
	    ((eq? datum 'END-STRICT-SRFI-105-REGION)
	     (set! srfi-strict #f)
	     (t-expr-catch-pragmas port))
	    (else datum))))

  ; a pragma of the strict SRFI 105 mode written alone at the level of the file (outside a module form) changes the
  ; mode and is returned : the reader of the file (main.rkt , curly-infix2prefix4racket.rkt) removes it with
  ; process-strict-pragma. The next datum is not read here : it is read by read-with-directives , that processes a
  ; parsing directive written just after the pragma (#!curly-infix , #!sweet , #!no-sweet) ; before , the directive was
  ; read here as a comment and ignored.
  (define (t-expr-catch-pragmas port)
    (let ((datum (t-expr-catch port)))
      (cond ((eq? datum 'BEGIN-STRICT-SRFI-105-REGION)
	     (set! srfi-strict #t))
	    ((eq? datum 'END-STRICT-SRFI-105-REGION)
	     (set! srfi-strict #f)))
      datum))

  #;(define sweet-read (make-read t-expr-catch))
  (define sweet-read (make-read t-expr-catch-pragmas))

  (define mutable-read curly-infix-read) ; this mutable reader will be the current-one changing from curly infix to sweet and so on...

  ;; the first word of a line : the name of a parsing directive , "curly-infix" in "curly-infix ; a comment"
  (define (directive-word line)
    (let loop ((l (string->list line)))
      (if (or (null? l)
	      (char-whitespace? (car l)))
	  '()
	  (cons (car l) (loop (cdr l))))))

  ;; read the next datum of a file with the current reader of the port (mutable-read) , after having processed the
  ;; parsing directives of SRFI 110 written alone at the beginning of a line (#!sweet , #!curly-infix , #!no-sweet , ...) ;
  ;; a directive acts from the next datum ("A #!curly-infix directive SHOULD cause the current port to switch to SRFI-105
  ;; semantics") : the readers process a directive as a comment and go on reading in the same mode , so the directive is
  ;; processed here , before the reader is called ; the empty lines and the lines of comment before it are skipped
  ;; Guile : an empty line or a line with only a comment , possibly after spaces , is read (as the regular expression
  ;; ^[ \t]*(;[^\r\n]*)?(\r\n|\n|\r) of the Racket version) ; otherwise nothing is read and #f is returned
  (define (blank-or-comment-line! port)
    (let loop ((read '()) (comment? #f))
      (let ((c (peek-char port)))
	(cond ((and (not (eof-object? c)) (or (char=? c #\newline) (char=? c #\return)))
	       (read-char port)
	       (when (and (char=? c #\return) (eqv? (peek-char port) #\newline))
		 (read-char port))
	       #t)
	      ((and (not (eof-object? c))
		    (or comment? (char=? c #\;) (char=? c #\space) (char=? c #\tab)))
	       (loop (cons (read-char port) read) (or comment? (char=? c #\;))))
	      (else
	       (unread-string (list->string (reverse read)) port)
	       #f)))))

  ;; Guile : the next n characters of the port , not read (as (peek-string n 0 port) of Racket)
  (define (peek-string-n n port)
    (let ((s (get-string-n port n)))
      (unless (eof-object? s) (unread-string s port))
      s))

  (define (read-with-directives port . src)
    (let loop ()
      (let ((c (peek-char port)))
	(cond ((eof-object? c) c)
	      ;; an empty line or a line with only a comment , possibly after spaces (example : the end of the line
	      ;; #reader SRFI-110 ; a comment)
	      ((blank-or-comment-line! port)
	       (loop))
	      ((let ((s (peek-string-n 3 port))) ; #!directive
		 (and (string? s)
		      (= (string-length s) 3)
		      (char=? (string-ref s 0) #\#)
		      (char=? (string-ref s 1) #\!)
		      (char-alphabetic? (string-ref s 2))))
	       (get-string-n port 2) ; #!
	       (process-directive (list->string (directive-word (read-line port))))
	       (loop))
	      ((eq? mutable-read default-scheme-read) ; #!no-sweet : the read of Racket takes only the port
	       (mutable-read port))
	      (else
	       (apply mutable-read port src))))))
  
  ; added 2025
  ; NOT USED
  ;; (cond-expand
  ;;  (racket
  ;;   (define (curly-infix-read-init . port-src)
  ;;     (when (and (not (null? port-src))
  ;;                (not (null? (cdr port-src)))
  ;;                (path? (cadr port-src)))
  ;;       (set! source (some-system-path->string (cadr port-src)))) ; source filename
  ;;     (define curly-infix-read (make-read curly-infix-read-nocomment)) ; re-make the reader
  ;;     (if (null? port-src)
  ;;         (begin
  ;;            (set! global-port (current-input-port)) ; fall back
  ;;            (curly-infix-read))
  ;;         (begin
  ;;            (set! global-port (car port-src))
  ;;            (curly-infix-read (car port-src))))))
  ;;  (else ))

  ) ; end readable-kernel-module-contents



(export
	care-of-quote
	srfi-strict
	use-only-syntax-transformers
	get-annot-flag
	set-annot-flag!
	get-annot-on-read
	set-annot-on-read!
	process-annotation-pragma
	process-strict-pragma
	reset-srfi-strict!
	mutable-read
	curly-infix-read
	neoteric-read
	sweet-read
	set-read-mode
	replace-read
	restore-traditional-read
	enable-curly-infix
	enable-neoteric
	enable-sweet
	curly-write-simple
	neoteric-write-simple
	curly-write
	curly-write-cyclic
	curly-write-shared
	neoteric-write
	neoteric-write-cyclic
	neoteric-write-shared
	read-with-directives)
 ; end of Racket module


; vim: set expandtab shiftwidth=2 :
