; A small program written with Scheme+ for Guile and the sweet-expressions of SRFI 110
; (sweet-example+.rkt of SRFI 110 for Racket , written for Guile).
;
; The indentation gives the parenthesis , the curly brackets { } are infix (with the precedence of the operators) ,
; f(x) is (f x) , v[i] is the element i of the vector v , M[i][j] the element j of the line i of a matrix.
;
; Four rules of the sweet-expressions to remember :
;  - an empty line ends an expression : inside a block the lines are separated by comment lines ;
;  - the then and else blocks of if can be indented under their keyword , but a keyword of Scheme+ that separates
;    expressions at the end of a loop (while of do , until of repeat) is written alone on its line , at the same level
;    as the expressions ;
;  - ! at the beginning of a line is an indentation character : the factorial is written {n !} or (! n) ;
;  - in Guile <- and := only assign : a variable is defined with <+ (or define).
;
; how to run it (Scheme+ for Guile 20.2) , in the directory examples/ of Scheme+ for Guile :
;   make
;     parses the Scheme+ files of the directory : the files *-sweet+.scm with sweet-infix2prefix4guile.scm , the other
;     ones with curly-infix2prefix4guile.scm , in parsed_files_directory/
;   guile -L .. parsed_files_directory/example-sweet+.scm
;     -L .. : the Scheme+ of this repository , before an older version installed in the site directory of Guile
; after an update of Scheme+ : make clean , and rm -rf ~/.cache/guile/

use-modules (Scheme+)

ANNOTATION-ON

; ------------------------------------------------------------------ the prime numbers : sieve of Eratosthenes
define (primes-below n)
  type ℕ n i j              ; the indexes : vector-ref with no test
  {sieve <+ (make-vector n #t)}
  {primes <+ '()}
  for ({i <+ 2} {i < n} {i <- i + 1})
    when sieve[i]
      {primes <- (cons i primes)}
      for ({j <+ i * i} {j < n} {j <- j + i})
        {sieve[j] <- #f}
  reverse primes

; ------------------------------------------------------------------ Fibonacci : def allows return
def (fibonacci n)
  when {n < 2}
    return n
  {a <+ 0}
  {b <+ 1}
  for ({k <+ 2} {k <= n} {k <- k + 1})
    {(a b) <- (values b {a + b})} ; assignment of multiple values : the right hand side uses the old values
  b

; ------------------------------------------------------------------ Fibonacci , the recursive version
; f{x} is (f x) : fib{n - 1} is (fib (- n 1)) , while fib(n - 1) would be (fib n - 1)
define (fib n)
  if {n < 2}
    n
    {fib{n - 1} + fib{n - 2}}

; ------------------------------------------------------------------ product of two matrices (vectors of lines)
define (matrix-product A B)
  type #(#(ℝ)) A B C        ; vectors of vectors of flonums
  type ℕ n m p i j k        ; the indexes : vector-ref with no test
  {n <+ (vector-length A)}
  {m <+ (vector-length B)}
  {p <+ (vector-length B[0])}
  {C <+ (make-vector n)}
  for ({i <+ 0} {i < n} {i <- i + 1})
    {C[i] <- (make-vector p 0.0)}
    for ({j <+ 0} {j < p} {j <- j + 1})
      for ({k <+ 0} {k < m} {k <- k + 1})
        {C[i][j] <- C[i][j] + A[i][k] * B[k][j]}
  C

; ------------------------------------------------------------------ square root by the method of Newton
; with the type of x the annotation pass uses the operations of the flonums
define (newton-sqrt x)
  type flonum? x
  {r <+ x / 2.0}
  while {abs{r * r - x} > 1e-12}
    {r <- (r + x / r) / 2.0}
  r

; ------------------------------------------------------------------ if then else : the blocks indented under then and else
define (sign x)
  if {x < 0}
    then
      'negative
    else
      if {x = 0} 'zero 'positive

; ------------------------------------------------------------------ binomial coefficient : the postfix factorial
define (binomial n k)
  {(n !) / ((k !) * ((n - k) !))}

; ------------------------------------------------------------------ the length of a sequence of Collatz
define (collatz-length n)
  {steps <+ 0}
  while {n ≠ 1}
    {n <- (if (even? n) {n / 2} {3 * n + 1})}
    {steps <- steps + 1}
  steps

; ------------------------------------------------------------------ let with the group marker \\
define (hypotenuse a b)
  let
    \\
      a2 {a * a}
      b2 {b * b}
    sqrt{a2 + b2}

; ------------------------------------------------------------------ symbolic computation : a quoted formula
; in the region CARE-OF-QUOTES-OFF the quoted lists are infix expressions (with the precedence of the operators)
CARE-OF-QUOTES-OFF

{carry <+ '(Bₖ · Bₖ₋₁ ⊕ Cₖ · (Bₖ ⊕ Bₖ₋₁))}

CARE-OF-QUOTES-ON

; ------------------------------------------------------------------ the results ($ : the rest of the line is a list)
display "primes below 50 : "
display $ primes-below 50
newline()

display "fibonacci(30) = "
display fibonacci(30)
newline()

display "fib(25) = "
display fib(25)
newline()

{M <+ #(#(1.0 2.0) #(3.0 4.0))}

display "M * M = "
display $ matrix-product M M
newline()

display "newton-sqrt(2.0) = "
display newton-sqrt(2.0)
newline()

display $ list sign(-5) sign(0) sign(7)
newline()

display "binomial(10 3) = "
display binomial(10 3)
newline()

display "collatz-length(27) = "
display collatz-length(27)
newline()

display "hypotenuse(3 4) = "
display hypotenuse(3 4)
newline()

display "carry = "
display carry
newline()
