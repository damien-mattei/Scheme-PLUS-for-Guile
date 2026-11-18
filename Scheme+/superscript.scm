;; Translated from Scheme+ for Racket (superscript.rkt) for Guile.


;; superscript definitions

;; port from scheme+ to scheme

;; Damien MATTEI

;; this code parse only the superscript expressions

(define-module (Scheme+ superscript)
  #:use-module (Scheme+ racket-compat))
	



(use-modules
	((srfi srfi-13) #:hide (string-hash))
	((srfi srfi-13) #:select ((string-hash . string-hash-srfi13)))) ;; strings

(use-modules
	(Scheme+ def-function)
	(Scheme+ operators-list))

;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign

;; under Aquamacs and in general use fonts of fixed size Menlo (not Monaco because the superscript chars are at various height!)



(define superscript-string "⁻⁺⁰¹²³⁴⁵⁶⁷⁸⁹") ; super script numbers and signs


;; (superscript? '²)
;; '("²")
(def (superscript? ss-stx) ; test if a syntax (or not) object is a superscript expression of integers numbers positives or not ,alphabetic char existing in superscript
  ;;(display "superscript? : ss-stx=") (display ss-stx)(newline)
  (define sd ss-stx)
  (when (syntax? ss-stx) ; could also be something not syntax,example:  '¹²  
    (set! sd (syntax->datum ss-stx)))
  ;;(display "superscript? : sd=") (display sd) (newline)
  (when (not (symbol? sd)) ; can not remember an example!
    (return #f))
  (define str (symbol->string sd))
  ;;(display "superscript? : str=") (display str) (newline)
  ;;(regexp-match (regexp "^[⁻⁺]*[⁰¹²³⁴⁵⁶⁷⁸⁹]+$") str))
  (define str-rgex (string-append "^[" full-superscript-string "]+$"))
  (regexp-match (regexp str-rgex) str))


;; NOT USED FOR NOW AND ERROR
(define (superscript-but-not-operator? ss-stx)
  (define sd ss-stx)
  (when (syntax? ss-stx) ; could also be something not syntax,example:  '¹²  
    (set! sd (syntax->datum ss-stx)))
  (and (superscript? sd)
       (not (member-in-postfix-lst sd)))) ; TODO ERROR: postfix? but operator? above


(def (superscript-only? ss-stx) ; test if a syntax (or not) object is a superscript expression, testing only alphabetic existing superscript
  ;;(display "superscript? : ss-stx=") (display ss-stx)(newline)
  (define sd ss-stx)
  (when (syntax? ss-stx) ; could also be something not syntax,example:  '¹²  
    (set! sd (syntax->datum ss-stx)))
  ;;(display "superscript? : sd=") (display sd) (newline)
  (when (not (symbol? sd)) ; can not remember an example!
    (return #f))
  (define str (symbol->string sd))
  ;;(display "superscript? : str=") (display str) (newline)
  ;;(regexp-match (regexp "^[⁻⁺]*[⁰¹²³⁴⁵⁶⁷⁸⁹]+$") str))
  (define str-rgex (string-append "^[" superscript-only-string "]+$"))
  (regexp-match (regexp str-rgex) str))

;; like this with superscript
;; scheme@(guile-user)> (string-subscript-number->string-number "₁₂₃")
;; "123"
(define (string-superscript-number->string-number str)
  (string-replace-chars str superscript-string "-+0123456789"))

(define (syntax-superscript-number->number ss-stx) ; take a syntax as argument
  (string->number (string-superscript-number->string-number (symbol->string (syntax->datum ss-stx)))))

(define (generic-superscript-number->number ss-stx) ; take any argument (syntax or symbol)
  (define sd ss-stx)
  (when (syntax? ss-stx) ; could also be something not syntax,example:  ¹²  
    (set! sd (syntax->datum ss-stx))) ; convert it in datum (number,symbol,....)
  (string->number (string-superscript-number->string-number (symbol->string sd))))

(define (string-superscript-number->number str)
  (string->number (string-superscript-number->string-number str)))


;; scheme@(guile-user)> (string-replace-chars "₁₂₃" "₋₊₀₁₂₃₄₅₆₇₈₉" "-+0123456789")
;; "123"
(define (string-replace-chars str str-before str-after)
  (string-map (lambda (c)
		(translate-char c str-before str-after))
	      str))


;; scheme@(guile-user)> (translate-char #\₁ "₋₊₀₁₂₃₄₅₆₇₈₉" "-+0123456789")
;; #\1
(def (translate-char c str-before str-after)
  (define i (string-index str-before c))
  (unless i (return c)) ;; if no match return unchanged the character c
  (string-ref str-after i))


;; scheme@(guile-user)> (string-for-each (lambda (c) (display c) (display " <-> ") (display (char->integer c)) (newline)) "₋₊₀₁₂₃₄₅₆₇₈₉")
;; ₋ <-> 8331
;; ₊ <-> 8330
;; ₀ <-> 8320
;; ₁ <-> 8321
;; ₂ <-> 8322
;; ₃ <-> 8323
;; ₄ <-> 8324
;; ₅ <-> 8325
;; ₆ <-> 8326
;; ₇ <-> 8327
;; ₈ <-> 8328
;; ₉ <-> 8329



;; (superscript->normal "³")
;; "3"

;; (superscript->normal "3")
;; "3"

(define (superscript->normal str)
     (string-replace-chars str full-superscript-string full-normal-string))

(define full-superscript-string (string-append superscript-string ; numbers and signs
                                               ; the commented string below is without ᵀ (once reserved for the transpose of a matrix) , ᵀ is now a superscript letter
					  #;"ᴬᵃᴮᵇᶜᴰᵈᴱᵉᶠᴳᵍᴴʰᴵᶦᴶʲᴷᵏᴸˡᴹᵐᴺⁿᴼᵒᴾᵖᴿʳˢᵗᵁᵘⱽᵛᵂʷˣʸᶻ" "ᴬᵃᴮᵇᶜᴰᵈᴱᵉᶠᴳᵍᴴʰᴵᶦᴶʲᴷᵏᴸˡᴹᵐᴺⁿᴼᵒᴾᵖᴿʳˢᵀᵗᵁᵘⱽᵛᵂʷˣʸᶻ" ; note all letters are not available (ex:C,Q,q...)
					  "⁽⁾"
					  "⸱" ; will be used as decimal separator
					  "*" ; multiplication
					  "·" ; another multiplication symbol
					  "⸍"; division
					  ))


(define full-normal-string (string-append "-+0123456789" ; numbers and signs
                                          ; the commented string below is without T (once reserved for the transpose of a matrix) , T is now used for ᵀ
					  #;"AaBbcDdEefGgHhIiJjKkLlMmNnOoPpRrstUuVvWwxyz" "AaBbcDdEefGgHhIiJjKkLlMmNnOoPpRrsTtUuVvWwxyz" ; note all letters are not available (ex:C,Q,q...)
					  "()"
					  "." ; will be used as decimal separator
					  "*" ; multiplication
					  "·" ; another multiplication symbol
					  "/"; division
					  ))

(define superscript-only-string (string-append superscript-string ; numbers and signs
                                               ; ᵀ is a superscript letter (it was once reserved for the transpose of a matrix)
					  "ᴬᵃᴮᵇᶜᴰᵈᴱᵉᶠᴳᵍᴴʰᴵᶦᴶʲᴷᵏᴸˡᴹᵐᴺⁿᴼᵒᴾᵖᴿʳˢᵀᵗᵁᵘⱽᵛᵂʷˣʸᶻ" ; note all letters are not available (ex:C,Q,q...)
					  "⁽⁾"
					  "⸱" ; will be used as decimal separator
					  ;; "*" ; multiplication is ambiguous (both super and normal script)
					  ;; "·" ; another multiplication symbol (but ambiguous too)
					  "⸍"; division
					  ))





(export
	string-superscript-number->string-number
	string-replace-chars
	translate-char
	superscript->normal
	string-superscript-number->number
	superscript?
	syntax-superscript-number->number
	generic-superscript-number->number
	superscript-only?
	full-superscript-string)
 ; end of module

