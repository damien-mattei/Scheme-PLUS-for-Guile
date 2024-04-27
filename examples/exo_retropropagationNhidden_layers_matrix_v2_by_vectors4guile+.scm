
; Deep Learning : back propagation, gradient descent, neural network with N hidden layers

; L'algorithme de rétro-propagation du gradient dans un
; réseau de neurones avec N couches cachées.

;  D. Mattei	


; use MacVim to show ALL the characters of this file (not Emacs, not Aquamacs)
; jeu de couleurs: Torte ou Koehler

;./curly-infix2prefix4guile.scm    ../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile+.scm > ../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile.scm

; or: make -f Makefile.Guile

; use: (load "exo_retropropagationNhidden_layers_matrix_v2_by_vectors4guile.scm")

; in case of problem: rm -rf .cache/guile/

#|  note: this file can run in two ways:
          - directly loaded in Guile
	  - parsed and loaded
|#


(use-modules (Scheme+)
	     (matrix+)
	     ;(srfi srfi-42) ; import all definitions
             ((srfi srfi-42) 
		#:renamer (lambda (sym) ; this avoid conflict with scheme+ slicing symbol
                              (if (eq? sym ':)
                                  's42-:
                                  sym)))
		;#:select ((: . s42-:))) ; rename only one
	     ;;((srfi srfi-42) ; Eager Comprehensions
	;;	#:prefix s42-)
	     (oop goops)
	     (srfi srfi-43)) ; vectors


;scheme@(guile-user)> {#(1 2) + #(3 4 5)}
;$1 = #(1 2 3 4 5)
;scheme@(guile-user)> {#(1 2) + #(3 4 5) + #(6 7)}
;$2 = #(1 2 3 4 5 6 7)
(define-method (+ (x <vector>) (y <vector>)) (vector-append x y))

;; ; first stage overloading
;; (define-overload-existing-operator +)

;; ; second stage overloading
;; (overload-existing-operator + vector-append (vector? vector?))



;; overload [ ] 
(overload-square-brackets matrix-ref matrix-set!  (matrix? number? number?))
(overload-square-brackets matrix-line-ref matrix-line-set! (matrix? number?))

(define (uniform-dummy dummy1 dummy2) {-1 + (random 2.0)}) 

; return a random number between [inf, sup]
(define (uniform-interval inf sup)
  {gap <+ {sup - inf}}
  {inf + (random {gap * 1.0})})

; sigmoïde
(define (σ z̃) 
  {1 / {1 + (exp (- z̃))}})

; some derivatives
(define (der_tanh z z̃)
  {1 - z ** 2})	

(define (der_σ z z̃)
    {z * {1 - z}})

(define (der_atan z z̃)
  {1 / {1 + z̃ ** 2}})



#| this is a Scheme multi line comment,
but will it works with all Scheme+ parser?
|#

; modify coefficients layer
(define (modification_des_poids M_i_o η z_input z_output z̃_output ᐁ_i_o მzⳆმz̃) ; derivative of activation function of the layer
	 
	  ; the length of output and input layer with coeff. used for bias update
	  {(len_layer_output len_layer_input_plus1forBias) <+ (dim-matrix M_i_o)} ; use values and define-values to create bindings
        
	  {len_layer_input <+ {len_layer_input_plus1forBias - 1}}

	  (for-each-in (j (in-range len_layer_output)) ; line
		(for-each-in (i (in-range len_layer_input)) ; column , parcours les colonnes de la ligne sauf le bias
		    {M_i_o[j {i + 1}]  <-  M_i_o[j {i + 1}] + η * z_input[i] * მzⳆმz̃(z_output[j] z̃_output[j]) * ᐁ_i_o[j]})

		; and update the bias
            	{M_i_o[j 0]  <-  M_i_o[j 0] + η * 1.0 * მzⳆმz̃(z_output[j] z̃_output[j]) * ᐁ_i_o[j]}))
	




(define-class ReseauRetroPropagation () ; network back propagation
  
  (nbiter #:init-value 3 
	  #:init-keyword #:nbiter 
	  #:getter nbp-get-nbiter 
	  #:setter nbp-set-nbiter!)

  (activation_function_hidden_layer #:init-keyword #:activation_function_hidden_layer
				    #:getter nbp-get-activation_function_hidden_layer)

  (activation_function_output_layer #:init-keyword #:activation_function_output_layer
				    #:getter nbp-get-activation_function_output_layer)

  (activation_function_hidden_layer_derivative #:init-keyword #:activation_function_hidden_layer_derivative
					       #:getter nbp-get-activation_function_hidden_layer_derivative)

  (activation_function_output_layer_derivative #:init-keyword #:activation_function_output_layer_derivative
					       #:getter nbp-get-activation_function_output_layer_derivative)


  (ηₛ #:init-value 1.0 
      #:init-keyword #:ηₛ
      #:getter nbp-get-ηₛ)

  (z #:getter nbp-get-z 
     #:setter nbp-set-z!)
  
  (z̃ #:getter nbp-get-z̃ 
     #:setter nbp-set-z̃!)

  (M #:getter nbp-get-M 
     #:setter nbp-set-M!)

  (ᐁ #:getter nbp-get-ᐁ  
     #:setter nbp-set-ᐁ!)

  (eror #:init-value 0)) ; end class


(define (*init* nc nbp)
		 (display "*init* : nc=") (display nc) (newline)
		 
		 {lnc <+ (vector-length nc)}

		 (define (make-vector-zero i lg) (make-vector lg 0))

		 (declare z z̃ M ᐁ)
		 
		 {nbiter <+ (nbp-get-nbiter nbp)}

		 {z <- (vector-map make-vector-zero nc)}
   		 (display "z=") (display z) (newline)
	 	 
		 ; z̃[0] is not used as z[0] is x, the initial data
		 {z̃ <- (vector-map make-vector-zero nc)}
                 (display "z̃=") (display z̃) (newline)

		 {M <- (vector-ec (s42-: n {lnc - 1}) ; vectors by eager comprehension (SRFI 42)
			  create-matrix-by-function(uniform-dummy nc[n + 1] {nc[n] + 1}))} ;; Matrix-vect

		 (display "M=") (display M) (newline)

   		 {ᐁ <- (vector-map make-vector-zero nc)}
		 (display "ᐁ=") (display ᐁ) (newline)

   		 (display "nbiter=") (display nbiter) (newline)

		 (nbp-set-z! nbp z)
		 (nbp-set-z̃! nbp z̃)
		 (nbp-set-M! nbp M)
		 (nbp-set-ᐁ! nbp ᐁ)

) ; end method *init*

  
(define (accepte_et_propage x nbp) ; on entre des entrées et on les propage
  
  {z <+ (nbp-get-z nbp)}

  (when {vector-length(x) ≠ vector-length(z[0])} 
	(display "Mauvais nombre d'entrées !") (newline)
	(exit #f))

  {z[0] <- x} ; on ne touche pas au biais

  ;; propagation des entrées vers la sortie

  {n <+ vector-length(z)}
  ;;(display "n=") (display n) (newline)

  ;; hidden layers
  (declare z_1)

  {z̃ <+ (nbp-get-z̃ nbp)}

  {M <+ (nbp-get-M nbp)}

  {activation_function_hidden_layer <+ (nbp-get-activation_function_hidden_layer nbp)}

  (define (activation_function_hidden_layer_indexed i z̃)
	(activation_function_hidden_layer z̃))

  {activation_function_output_layer <+ (nbp-get-activation_function_output_layer nbp)}

  (define (activation_function_output_layer_indexed i z̃)
	(activation_function_output_layer z̃))

  (declare i) ; because the variable will be used outside the 'for' loop too

  (for ({i <- 0} {i < n - 2} {i <- i + 1}) ; private 'for' definition as in Javascript,C,C++,Java

       ;; calcul des stimuli reçus par la couche cachée d'indice i+1 à-partir de la précedente

       ;; create an array with 1 in front for the bias coefficient
       
       {z_1 <- #(1) + z[i]} ; + operator has been overloaded to append scheme vectors

       ;(display "z_1 = ") (display z_1) (newline)

       ;(display "M=") (display M) (newline)

       {z̃[i + 1] <- M[i] * z_1} ; z̃ = matrix * vector , return a vector

       ;(display "z̃[i + 1] = ") (display {z̃[i + 1]}) (newline)

       #| calcul des réponses des neurones cachés
       
       i also use Neoteric Expression :https://sourceforge.net/p/readable/wiki/Rationale-neoteric/
       example: {map(sin '(0.2 0.7 0.3))}
       '(0.19866933079506122 0.644217687237691 0.29552020666133955)
       
       i also use Neoteric Expression to easily port Python code to Scheme+
       
       the original Python code was:
       z[i+1] = list(map(self.activation_function_hidden_layer,z̃[i+1]))
       the Scheme+ port is below: |#
       
       {z[i + 1] <- vector-map(activation_function_hidden_layer_indexed z̃[i + 1])}

       ;;(display "z[i + 1] = ") (display {z[i + 1]}) (newline)

       ) ; end for

  ;; output layer
  
  ;; calcul des stimuli reçus par la couche cachée d'indice i+1 à-partir de la précedente

  ;; create a list with 1 in front for the bias coefficient
  {z_1 <- #(1) + z[i]}

  {z̃[i + 1] <- M[i] * z_1} ; z̃ = matrix * vector , return a vector

  ;; calcul des réponses des neurones de la couche de sortie
  {z[i + 1] <- vector-map(activation_function_output_layer_indexed z̃[i + 1])}
  ;;(display "z[i + 1] = ") (display {z[i + 1]}) (newline)

  ; update the data
  (nbp-set-z! nbp z)
  (nbp-set-z̃! nbp z̃)
  
) ; end define


(define (apprentissage Lexemples nbp) ; apprentissage des poids par une liste d'exemples
  
  {ip <+ 0} ; numéro de l'exemple courant

  {z <+ (nbp-get-z nbp)}
  {z̃ <+ (nbp-get-z̃ nbp)}
  {M <+ (nbp-get-M nbp)}
  {ᐁ <+ (nbp-get-ᐁ nbp)}
  {activation_function_output_layer_derivative <+ (nbp-get-activation_function_output_layer_derivative nbp)}
  {activation_function_hidden_layer_derivative <+ (nbp-get-activation_function_hidden_layer_derivative nbp)}
  {ηₛ <+ (nbp-get-ηₛ nbp)}
  {nbiter <+ (nbp-get-nbiter nbp)}


  (declare x y)
  (for-each-in (it (in-range nbiter)) ; le nombre d'itérations est fixé !
		 (when {it % 1000 = 0}
		       (display it)(newline))

		 ;;(display it)(newline)
		 
		 ;{err <+ 0.0} ; l'erreur totale pour cet exemple

		 {x <- (car Lexemples[ip])}         ; un nouvel exemple à apprendre
		 {y <- (cdr Lexemples[ip])} 


		 ;; PROPAGATION VERS L'AVANT
		 (accepte_et_propage x nbp)       ; sorties obtenues sur l'exemple courant, self.z_k et z_j sont mis à jour

		 ;; RETRO_PROPAGATION VERS L'ARRIERE, EN DEUX TEMPS

		 {i <+ i_output_layer <+ vector-length(z) - 1} ; start at index i of the ouput layer

		 {ns <+ vector-length(z[i])}
		 

		 ;; TEMPS 1. calcul des gradients locaux sur la couche k de sortie (les erreurs commises)
		 (for-each-in (k (in-range ns))
				{ᐁ[i][k] <- y[k] - z[i][k]}  )   ; gradient sur un neurone de sortie (erreur locale)
			;	{err <- err + ᐁ[i][k] ** 2})    ; l'erreur quadratique totale

		 ;{err <- err * 0.5}

		 ;(when {it = nbiter - 1}
		  ;     {eror <- err})               ; mémorisation de l'erreur totale à la dernière itération


		 ;; modification des poids de la matrice de transition de la derniére couche de neurones cachés à la couche de sortie

		 {მzⳆმz̃ <+ activation_function_output_layer_derivative}

		 {modification_des_poids(M[i - 1] ηₛ z[i - 1] z[i] z̃[i] ᐁ[i] მzⳆმz̃)}

		 ;; TEMPS 2. calcul des gradients locaux sur les couches cachées (rétro-propagation), sauf pour le bias constant

		 {მzⳆმz̃ <- activation_function_hidden_layer_derivative}

		 (for-each-in (i (reversed (in-range 1 i_output_layer)))
				{nc <+ vector-length(z[i])}
				{ns <+ vector-length(z[i + 1])}
				(for-each-in (j (in-range nc))
					       {k <+ 0} ; should be commented ?
					       {ᐁ[i][j] <- ($+>
							    {sum <+ 0}  
							    (for-each-in (k (in-range ns))
								{sum <- sum + მzⳆმz̃(z[i + 1][k] z̃[i + 1][k]) * M[i][k {j + 1}] * ᐁ[i + 1][k]})
							    sum)})
				;; modification des poids de la matrice de transition de la couche i-1 à i
				{modification_des_poids(M[i - 1] ηₛ  z[i - 1] z[i] z̃[i] ᐁ[i] მzⳆმz̃)})

		 ;; et l'on passe à l'exemple suivant
		 
		 {ip <- random(vector-length(Lexemples))}

	       ) ; end for it

  ; uodate the data
  (nbp-set-z! nbp z)
  (nbp-set-z̃! nbp z̃)
  (nbp-set-M! nbp M)
  (nbp-set-ᐁ! nbp ᐁ)

) ; end define


(define (test Lexemples nbp)

	  {z <+ (nbp-get-z nbp)}

          (display "Test des exemples :") (newline)
          {err <+ 0}

	  (declare entree sortie_attendue ᐁ)
	  (for-each-in (entree-sortie_attendue (vector->list Lexemples))
		       
		{entree <- (car entree-sortie_attendue)} 
		{sortie_attendue <- (cdr entree-sortie_attendue)} 
		(accepte_et_propage entree nbp)

		(format #t ; current output port
			"~a --> ~a : on attendait ~a~%"
			entree 
			{z[vector-length(z) - 1]} 
			sortie_attendue)  ; ~% is(newline)

		{ᐁ <- sortie_attendue[0] - z[vector-length(z) - 1][0]} ; erreur sur un element
		{err <- err + ᐁ ** 2}) ; l'erreur quadratique totale
		
	  {err <- err * 0.5}
	  (display "Error on examples=") (display err) (newline))



(display "################## NOT ##################")
(newline)

{r1 <+ (make ReseauRetroPropagation  
	     #:nbiter 5000
	     #:ηₛ 10
	     #:activation_function_hidden_layer σ
	     #:activation_function_output_layer σ
	     #:activation_function_hidden_layer_derivative der_σ
	     #:activation_function_output_layer_derivative der_σ)}

{Lexemples1 <+ #((#(1) . #(0)) (#(0) . #(1)))}  ; use pairs in Scheme instead of vectors in Python

(*init* #(1 2 1) r1)

(apprentissage Lexemples1 r1)

(test Lexemples1 r1)

(newline)


(display "################## XOR ##################")
(newline)

;(for ({i <+ 0} {i < 100} {i <- i + 1})

{r2 <+ (make ReseauRetroPropagation  
	     #:nbiter 250000
	     #:ηₛ 0.1
	     #:activation_function_hidden_layer σ
	     #:activation_function_output_layer σ
	     #:activation_function_hidden_layer_derivative der_σ
	     #:activation_function_output_layer_derivative der_σ)}


{Lexemples2 <+ #( (#(1 0) . #(1))  (#(0 0) . #(0))  (#(0 1) . #(1))  (#(1 1) . #(0)))}  ; use pairs in Scheme instead of vectors in Python

(*init* #(2 8 1) r2) ; 1' 40" - 2' 25"

(apprentissage Lexemples2 r2)

(test Lexemples2 r2)

(newline)
;)



(display "################## SINE ##################")
(newline)

{r3 <+ (make ReseauRetroPropagation  
	     #:nbiter 50000
	     #:ηₛ 0.01
	     #:activation_function_hidden_layer atan
	     #:activation_function_output_layer tanh
	     #:activation_function_hidden_layer_derivative der_atan
	     #:activation_function_output_layer_derivative der_tanh)}




(declare pi)
{pi <- 4 * atan(1)}


{Llearning <+ (vector-map (lambda (i x) (cons (vector x) (vector (sin x))))  ; use pairs in Scheme instead of vectors in Python
			  (list->vector (map (lambda (n) (uniform-interval (- pi) pi))
					(in-range 10000))))}

;(display "Llearning=")  (display Llearning) (newline)

(*init* #(1 70 70 1) r3)


{Ltest <+ (vector-map (lambda (i x) (cons (vector x) (vector (sin x))))  ; use pairs in Scheme instead of vectors in Python
		      (list->vector (map (lambda (n) (uniform-interval {(- pi) / 2} {pi / 2}))
					 (in-range 10000))))}


(apprentissage Llearning r3)

(test Ltest r3)

(newline)



