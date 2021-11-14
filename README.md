<body>
    <p><br>
    </p>

<div align="center"><br>
    </div>
    <div align="center"><img moz-do-not-send="true" src="Scheme+.png" title="by Damien MATTEI"
        alt="Scheme+" width="290" height="65"></div>
    <h2 style="text-align: center;"><span style="color: #00cccc;"><i>Enhancing
          Scheme (and Lisp-like) language. </i></span></h2>
    <h2 style=" text-align: center;">Damien Mattei<br>
    </h2>
    <p style="text-align: center;"><i><br>
      </i></p>
    <p style="text-align: center;"><i>"Doubts are such tiny things. A mind with
        no room for doubts must have no room for thoughts either."</i> -<a href="https://www.ics.uci.edu/%7Epattis/"
        target="_blank">R. Patti</a></p>
    <p style="text-align: center;"><br>
    </p>
    <h1 style="text-align: center;"><b><span style="color: #000099;">Scheme+</span></b><b><span
          style="color: #999999;"> <font size="+2">version 1.0 for Guile Scheme<br>
          </font></span></b></h1>
    <p style="text-align: center;"> </p>
    <p style="text-align: center;"><br>
    </p>
    <p style="text-align: center;"><a href="https://damien-mattei.github.io/Scheme-PLUS-for-Guile/Scheme+.html" target="_blank">(click
        here for a better view of the programs of this page)</a><br>
    </p>
    <p><br>
    </p>
    <h2><u>1. Scheme+ at a glance:&nbsp;</u></h2>
    <p><b>Scheme+ is an extension of the Scheme syntax language.</b><br>
    </p>
    <p>The <b>idea</b> of <b>Scheme+</b> is came from this : <b><i>"Why is it
          so boring to define variables in Scheme with the traditionnal set of
          LET,LET*,LETREC,LET-VALUES special forms?"</i></b> and <b><i>"Is it
          possible to use a single mechanism for assignment in Scheme?"</i></b></p>
    <p>It comes also from the ascertainment that <i><b>"A computer language is
          created by ONE man, later, a community only bring a library to the
          language without self-questioning any more on the fundations of
          language itself."</b></i></p>
    <p><i><b><br>
        </b></i></p>
    <p><i>Scheme+</i> is a Scheme module.</p>
    <p>Once loaded by inserting this in the header of the source file : <b>(use-modules
        (Scheme+))</b><br>
    </p>
    <p><i>Scheme+</i> brings syntax to classic <i>Scheme</i> language like
      those:</p>
    <p><br>
    </p>
    <p style="margin-left: 40px;"><b>{x &lt;- y} </b>infix assignment of the
      value of y to the variable x.</p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"><b>{T[k]} </b>return value of vector or array
      T indexed by k.</p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"><b>{T</b><b>[k] &lt;- x} </b>infix assignment
      of <b>T</b><b>[k] </b>with x.</p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"><b>{T</b><b>[k] &lt;- </b><b><b>T</b><b>[{k +
          1}]</b>} </b>in infix or the same with a little part of classic
      prefix Scheme:&nbsp; <b>{T</b><b>[k] &lt;- </b><b><b>T</b><b>[(+ k 1)]</b>}
        <br>
      </b></p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;">the same with multi dimensional arrays or
      matrix: <br>
      <b><b>{</b>T</b><b>[m n]} </b>return the value of the multi dimension
      array T indexed by m and n</p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"><b><b>{</b>T</b><b>[m n] &lt;- </b><b><b>T</b><b>[m
          {n + 1}]</b>}</b></p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"> no more need of <strike>LET,LET*,LETREC,LET-VALUES</strike>,...
      </p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"><br>
      defining new functions with <b>def </b>instead of the classic define we
      can now use <b>return</b> to escape immediately :<br>
    </p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;"><b>(return)</b><br>
    </p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;">or returning the value v: </p>
    <div style="margin-left: 40px;"> <b>(return </b>v<b>)</b></div>
    <p style="text-align: left;"><b> <br>
      </b></p>
    <p>Here is an example of the previous features:</p>

    ```scheme
    (use-modules (Scheme+))

    (def (subset-sum-dynamic L t)

    (declare ls dyn c R s) ;; declare multiple variables

    {ls <- (length L)}
    {dyn <- dyna[ls t]} ;; dyna is a toplevel defined array

    ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

    (if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)
        (return (one? dyn)))

    (when (null? L)
      {dyna[ls t] <- 2}
      (return #f))

    {c <- (first L)}

    (when {c = t}  ;; c is the solution
      {dyna[ls t] <- 1}
      (return #t))

    {R <- (rest L)} ;; continue searching a solution in the rest

    (if {c > t}  ;; c is to big to be a solution
      {s <- (subset-sum-dynamic R t)}
      ;; c is part of the solution or c is not part of solution
      {s <- {(subset-sum-dynamic R {t - c}) or (subset-sum-dynamic R t)}})

    {dyna[ls t] <- (one-two s)}
    s) ;; return boolean value
    ```

    <p>Another example with Guile Scheme in the REPL (Read Eval Print Loop):</p>
    <p> </p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">mattei@MBP-Touch-Bar ~ % cd
        Dropbox/git/Scheme-PLUS-for-Guile</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">mattei@MBP-Touch-Bar
        Scheme-PLUS-for-Guile % guile</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">GNU Guile 3.0.7</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">Copyright (C) 1995-2021 Free
        Software Foundation, Inc.</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt;
        (use-modules (Scheme+))</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; note: source file
        ./Scheme+.scm</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; <span class="Apple-converted-space">&nbsp;
          &nbsp; &nbsp; </span>newer than compiled<span class="Apple-converted-space">
          <br>
        </span></font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; compiling ./Scheme+.scm</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; compiled<span class="Apple-converted-space">&nbsp;</span></font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt;
        (declare L)</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt; {L
        &lt;- '(1 2 3 4)}</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">$1 = (1 2 3 4)</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt; L</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">$2 = (1 2 3 4)</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt; {L
        &lt;- (list 1 2 3 4 5)}</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #fef48b"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">$3 = (1 2 3 4 5)</font></p>
    <br>
    <p>for the rest read below and the documentation.</p>
    <br>
    <h2>2.<u>Download Scheme+:</u></h2>
    <p>The source code of the <b>Scheme+ module</b>, currently only for <a href="https://www.gnu.org/software/guile/"
        target="_blank">Guile implementation of Scheme</a> can be retrieved
      here: </p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h3 style="    text-align: center;">&gt;&gt;&gt;&nbsp;&nbsp; <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile"
        target="_blank">https://github.com/damien-mattei/Scheme-PLUS-for-Guile
        --&gt; Code --&gt; Download ZIP</a>&nbsp;&nbsp; &lt;&lt;&lt;<br>
    </h3>
    <p><br>
    </p>
    <p><br>
    </p>
    <p>Including : (but not mandatory to use Scheme+)</p>
    <p style="margin-left: 40px;">The growable vector module class in the file
      growable-vector.scm.</p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;">The enhanced set of LET special forms in the
      file let.scm.</p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;">Many of the examples of this web page are in
      the source file SssDyna.scm. </p>
    <p style="margin-left: 40px;">The .guile configuration file i use with Scheme+.</p>
    <h2><br>
    </h2>
    <h2>3<u>.Requirements:</u><br>
    </h2>
    <p><br>
    </p>
    <p>Scheme+ needs a Scheme interpreter and will be adapted and released for
      many Scheme (Guile,Racket,...), download Scheme Guile:<br>
    </p>
    <a href="https://www.gnu.org/software/guile/"><img moz-do-not-send="true" src="guile-logo.png"
        title="Guile Scheme" alt="Guile Scheme web site" width="156" height="76"
        border="0"></a><br>
    <br>
    <p>This version needs Guile Scheme version 3. It has been developed with
      Guile 3.0.7 on <b>Mac</b><font color="#9999ff"><b> </b><b>OS</b></font>
      and Guile 3.0.1 under <b>Linux</b>.<br>
      <br>
      <br>
    </p>
    <p>Guile offers infix notation that can be activated this way: for curly
      infix notation put in your <b>.guile </b>file :<b> (read-enable
        'curly-infix)</b><br>
    </p>
    Also you need to activate in Guile the <b>OOP option (Object Oriented
      Programming)</b><br>
    <br>
    Also Guile can be more easy to use with history on command line, completion,
    as example here is my <b>.guile </b>init file:<br>
    <br>
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #008800; font-style: italic; background-color: #0f140f">;; Guile config file</span>

<span style="color: #008800; font-style: italic; background-color: #0f140f">;; history</span>
<span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">use-modules</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">ice-9</span> <span
style="color: #fb660a">readline</span><span style="color: #ffffff">)</span>
         <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">ice-9</span> <span
style="color: #fb660a">history</span><span style="color: #ffffff">)</span>
         <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">srfi</span> <span
style="color: #fb660a">srfi-43</span><span style="color: #ffffff">)</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; vector</span>
         <span style="color: #008800; font-style: italic; background-color: #0f140f">;; guile object oriented programming system</span>
         <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">oop</span> <span
style="color: #fb660a">goops</span><span style="color: #ffffff">)</span>
         <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">oop</span> <span
style="color: #fb660a">goops</span> <span style="color: #fb660a">describe</span><span
style="color: #ffffff">))</span>
        
<span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">activate-readline</span><span
style="color: #ffffff">)</span>
<span style="color: #008800; font-style: italic; background-color: #0f140f">;;(disable-value-history!)</span>

<span style="color: #008800; font-style: italic; background-color: #0f140f">;; curly infix as in srfi-105</span>
<span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">read-enable</span> <span
style="color: #0086d2">'curly-infix</span><span style="color: #ffffff">)</span>

<span style="color: #008800; font-style: italic; background-color: #0f140f">;; set current path in load path</span>
<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">set! </span><span
style="color: #fb660a">%load-path</span> <span style="color: #ffffff">(reverse (cons </span><span
style="color: #0086d2">"."</span> <span style="color: #ffffff">(reverse </span><span
style="color: #fb660a">%load-path</span><span style="color: #ffffff">))))</span>
<span style="color: #008800; font-style: italic; background-color: #0f140f">;; other solution is to put this in shell:</span>
<span style="color: #008800; font-style: italic; background-color: #0f140f">;; export GUILE_LOAD_PATH="...:."</span>
</pre> </div>
    <p> </p>
    <br>
    <p>This .guile file is included in the Scheme+ repository.</p>
    <p><br>
    </p>
    <h2><br>
    </h2>
    <h2>4.<u>Author and license:</u></h2>
    <p><b>Damien Mattei </b>; <b>Scheme+</b> : Copyright 2021 <b>Damien
        Mattei </b>under <a href="https://www.gnu.org/licenses/gpl-3.0.txt" target="_blank">GNU GENERAL PUBLIC LICENSE Version 3</a></p>
    <p>Nice - France (commune de Carros) </p>
    <p> contact : <a href="https://annuaire.univ-cotedazur.fr/" target="_blank">directory
        e-mail<br>
      </a></p>
    <p>Guagno - Island of Corsica (Région des Dui Sorru) où m'est venue l'idée
      de Scheme+ ; contact : il y a la 4G,la 5G parfois,pour le routage il faut
      suivre un <i>Protocole développé localement</i> : les <b>"<i><a href="https://www.corsematin.com/articles/vandalisme-pitie-pour-les-panneaux-routiers-des-deux-sorru-527"
            target="_blank">Panneaux en Braille</a></i>" </b>ou du moins ce
      qu'il en reste<b>:</b> <a href="intersection-Guagno-Soccia.JPG" target="_blank"><img
          src="intersection-Guagno-Soccia.JPG" alt="intersection-Guagno-Soccia."
          title="intersection-Guagno-Soccia." height="100"></a> Photo “<em>Tous
        droits réservés</em>” Damien Mattei</p>
    <p>mais ne nous égarons pas :-) et revenons au Scheme+ <br>
    </p>
    <p> </p>
    <p> </p>
    <br>
    <p>To subit an issue, a bug or something else related with the Scheme+ code
      use the <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile/issues"
        target="_blank">Github issues system</a>.<br>
    </p>
    <p><br>
    </p>
    <h2><u>5. Idea and rationale:</u></h2>
    <p>The core language of <a href="https://en.wikipedia.org/wiki/Scheme_%28programming_language%29"
        target="_blank">Scheme</a> and <a href="https://en.wikipedia.org/wiki/Lisp_%28programming_language%29"
        target="_blank">Lisp</a> are based on syntaxic form that date back from
      '50 for Lisp and '70 for Scheme. Syntactic forms such as the set of
      LET,LET*,LETREC,... have since long ago been replaced in many other
      languages (C/C++,Java,Fortran,Pascal,Javascript,BASIC (the LET keyword can
      be ommited in Applesoft Basic),by simple assignment operator that frees
      the syntax and allow saving indentation space, number of parenthesis,
      readability of code.</p>
    <p>The main idea of this project is to improve Scheme and Lisp like
      languages with syntaxic features,such as easy assignment for many object
      types (numbers,strings,vectors,arrays,etc),and also allowing an immediate
      'return' mechanism to escape from procedure when result is known and
      others features that exist in others languages
      (Python,Java,C/C++,Pascal,BASIC,Javascript). Some new features can be and
      are better used with infix notations (<a href="https://srfi.schemers.org/srfi-105/srfi-105.html"
        target="_blank">SRFI 105</a> known as "Curly Infix"),so for some
      syntactic expressions infix will be promoted (but not mandatory). </p>
    <p>A few new feature (see below) allow a great change in syntax and
      readability of Scheme programs. Less parenthesis are used,less indentation
      and it allows an enhanced programming style that modernize Scheme but also
      keep it 100% compatible with the original language.</p>
    <p>Also vectors and arrays in Scheme are painfully accessed and modified by
      vector-ref and vector-set! , Scheme+ use a simplier syntax again with the
      &lt;- operator and the [] syntax.</p>
    <p>Scheme+ will remove the need for the classic LET set of special form, no
      more need of <strike>LET,LET*,LETREC,LET-VALUES</strike>,... and will use
      an unique infix (or not) assignment operator : &lt;-. (also LET are
      enhanced - the project historically by improving and simplifying LET, i
      simplified it so much that i remove it now! - in other versions such as
      let&lt;-arrow requiring less brackets, even if their use is useless and no
      more promoted, see extra features part)</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2><u>6. Features:</u></h2>
    <p>The list of new features is subject to change and will grow by time, this
      version allows:</p>
    <ul>
      <li>use of infix SRFI 105 "Curly Infix" for some syntax forms</li>
      <li>Assignment operator : <b>&lt;-</b> (can be used in infix syntax and
        from left to right and right to left)</li>
      <li>Square Bracket operator<b> []</b> working with Scheme <b>Vectors,
          Arrays,Multidimentional Arrays</b>,Dynamic Arrays (my growable vector
        class)...</li>
      <li>combination of Assignment operator &lt;- with [] operator for
        Vectors,Arrays,....</li>
      <li>RETURN : <b>return</b> keyword used in any procedure to return from
        procedure and return a value.</li>
      <li>declaration of variables</li>
      <li>declaration and assignment of variables with an infix operator.</li>
      <li>conditionals and execution of code in a new form : condx</li>
      <li>extra feature: new set of LET special forms,even if their use is
        completely discouraged in favor of assignment operator combined with
        declarative form (see declare).</li>
    </ul>
    <h2><br>
    </h2>
    <p><br>
    </p>
    <h2><u>7. First Examples:</u></h2>
    <h3 style=" margin-left: 40px;">The <a href="https://en.wikipedia.org/wiki/Fibonacci_number"
        target="_blank">Fibonacci sequence</a>:</h3>
    <p style="margin-left: 40px;">This example is written in Scheme with and
      without infix notation just to allow the reader to be familiar with "curly
      infix" notation which is not part of Scheme+ but used by it:</p>
    <p style="margin-left: 40px;">Here is the definition in classic prefix
      Scheme:</p>

```scheme
(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
```

<p style="margin-left: 40px;">And here is a definition using "Curly Infix"
      SRFI 105:</p>

```scheme
(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))
```

<p> </p>
    <br>
    <p>note that the last expression of fib: <b>{(fib {n - 1}) + (fib {n - 2})}
      </b>could also be expressed in curly infix Scheme like that :<b> {fib({n -
        1}) + fib({n - 2})}</b> or even like that: <b>{fib{n - 1} + fib{n - 2}}
      </b>but i will not use them, preferring to use infix only where
      mathematical calculus are coded and for comparaison tests in conditionals.</p>
    <p>Fibonacci is time consuming,even fib(47) can takes minutes to compute. We
      will write a faster dynamic version that memorize the results computed
      more than one time.</p>
    <p>Here is a solution in classic Scheme:</p>



```scheme
(define size0 10000)
(define memo0 (make-vector size0 0))return

(define (fibdyna0 n)
  (cond ((< n 2) n)
	((not (zero? (vector-ref memo0 n))) (vector-ref memo0 n))
	(else (let ((fibn (+ (fibdyna0 (- n 1)) (fibdyna0 (- n 2)))))
		(vector-set! memo0 n fibn)
		fibn))))
```


<p> </p>
    <br>
    <p>In the example above we can notice that the array access and modification
      is not easy to read and in the <b>else</b> block i have to use a <b>let</b>
      special form to create a local variable to return the value computed and
      already stored in array,even if i have not used a local variable i then
      must have to access the result in array to return it...</p>
    <p>Scheme+ allow to write code more readable and simpler than normal Scheme.</p>
    <p>Prior to use <b>Scheme+</b> for the implementation of Scheme named
      Guile, the <b>module </b>must be loaded,this is done by inserting the
      statement <b>(use-modules (Scheme+))</b> at the beginning of the Scheme
      source file.</p>
    <p>Below is the version of the above code written in Scheme+ :</p>
    <p><!-- HTML generated using hilite.me --></p>
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;"><span
        style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">use-modules</span>
      <span style="color: #ffffff">(</span><span style="color: #ff0086;
        font-weight: bold">Scheme+</span><span style="color: #ffffff">))</span>
    </div>
    <div style="background: #111111; overflow:auto;width:auto;border:
      gray;border-width:.1em .1em .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">{</span><span
style="color: #fb660a">size</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #0086f7; font-weight: bold">10000</span><span style="color: #ffffff">}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">memo</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #ffffff">(make-vector </span><span
style="color: #fb660a">size</span> <span style="color: #0086f7; font-weight: bold">0</span><span
style="color: #ffffff">)}</span> 
  
<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">define </span><span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">fibdyna</span> <span
style="color: #fb660a">n</span><span style="color: #ffffff">)</span>
  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">cond </span><span
style="color: #ffffff">({</span><span style="color: #fb660a">n</span> <span style="color: #fb660a">&lt;</span> <span
style="color: #0086f7; font-weight: bold">2</span><span style="color: #ffffff">}</span> <span
style="color: #fb660a">n</span><span style="color: #ffffff">)</span>
	<span style="color: #ffffff">({</span><span style="color: #fb660a">memo</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">n</span><span style="color: #ffffff">]</span> <span
style="color: #fb660a">&lt;&gt;</span> <span style="color: #0086f7; font-weight: bold">0</span><span
style="color: #ffffff">}</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">memo</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">n</span><span style="color: #ffffff">]})</span>                
	<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">else </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">memo</span><span style="color: #ffffff">[</span><span
style="color: #fb660a">n</span><span style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">{(</span><span style="color: #ff0086; font-weight: bold">fibdyna</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">n</span> <span style="color: #fb660a">-</span> <span
style="color: #0086f7; font-weight: bold">1</span><span style="color: #ffffff">})</span> <span
style="color: #fb660a">+</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">fibdyna</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">n</span> <span style="color: #fb660a">-</span> <span
style="color: #0086f7; font-weight: bold">2</span><span style="color: #ffffff">})}})</span> <span
style="color: #ffffff">))</span>
</pre> </div>
    <p> </p>
    <br>
    <p>The reader can notice the introduction of one new assignment operator <b>&lt;-</b>
      and also <b>&lt;+</b>, the latter being simply an infix <b>define</b> of
      Scheme. The important point of the new assignment operator <b>&lt;-</b>
      of <b>Scheme+</b> is that it will work like the classic <b>set! </b>of
      Scheme on variables but also will work on more <b>complex object such as
        element of vectors,multidimensional arrays (as defined in
        SRFIs),growable vectors (my class),... and much more in the next
        versions of Scheme+</b>. (we will see another more complex example with
      multidimentional array later)</p>
    <p>So <b>{x &lt;- 7} </b>simply assign 7 to the variable x but <b>{m[3]
        &lt;- 7}</b> will store 7 in the array <b>m </b>at the position
      indexed by 3. Access an element of array is also simpler in Scheme+ than
      in Scheme: <b>{m[3]}</b>. And this syntax is 100% compatible with Scheme,
      you can mix both the syntaxes if you want. This syntax is also more
      natural and readable and much like the mathematical notation, it is also a
      syntax used for arrays in other languages such as
      C/C++,Python,Java,Pascal,Javascript...</p>
    <p>Also you will notice&nbsp; that the main computation in the <b>else</b>
      block is now shorter and it is partly due to the fact that the assignment
      operator <b>&lt;- </b>return not NIL but the affected <b>value</b>
      making it available for other calculus or as a final result value.</p>
    <p>Here is now the same solution in a declarative form, in the part&nbsp;
      called "History of project" i explain why there exist two solutions. </p>
    <p>Instead of declare and assign the variables with the <b>define</b>
      nested in the <b>macro &lt;+ </b>we also can <b>declare</b> them and
      assign the variables later with the universal <b>&lt;-</b> operator:</p>
    <h2><br>
    </h2>
    <h2><br>
    </h2>
    <h2><u>8.Short documentation</u></h2>
    <p>Square bracket operator <b>[] </b>is used for vectors,arrays,growable
      vectors,etc.</p>
    <p>example: <b>{T[k]} </b>return value of vector or array T indexed by k.</p>
    <p><br>
    </p>
    <p>Assignment operator <b>&lt;- </b>can be used only on existing single
      variables or vector,arrays,etc . Note that vector and arrays must be
      defined the usual way they are in Scheme. The operator &lt;- works also
      with multidimensional arrays.</p>
    <p>examples : </p>
    <ol>
      <li><b>{x &lt;- 7} </b></li>
      <li><b>{x &lt;- y} </b></li>
      <li><b>{m[3] &lt;- 7}</b></li>
      <li><b>{T</b><b>[k] &lt;- x}</b></li>
      <li><b>{T</b><b>[k] &lt;- </b><b><b>T</b><b>[{k + 1}]</b>}</b></li>
      <li><b><b>{</b>T</b><b>[m n] &lt;- </b><b><b>T</b><b>[m {n + 1}]</b>}</b></li>
    </ol>
    <p><br>
    </p>
    <p>Definition and assignment of variables is made with the <b>&lt;+</b>
      operator but can also be done with the classic Scheme <b>define</b>.</p>
    <p>example: {size &lt;+ 1000}</p>
    <p><br>
    </p>
    <p>Declaration of variable can also be used to <b>declare</b> one or many
      variables. (for explanation why there exist still <b>&lt;+</b> and <b>declare</b>
      and not only <b>&lt;-</b> read the part history of project)</p>
    <p>example: <b>(declare x y z t)</b></p>
    <p> </p>
    <br>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2>9<u>.Advanced examples:</u><br>
    </h2>
    <p>Here is another example, from the <a href="https://en.wikipedia.org/wiki/Subset_sum_problem#Pseudo-polynomial_time_dynamic_programming_solution"
        target="_blank">Subset Sum Problem</a> that show the use of <b>&lt;+</b>
      and <b>&lt;-</b> (due to the impossibility to have easily a single
      operator :-( ) :</p>
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;"><span
        style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">use-modules</span>
      <span style="color: #ffffff">(</span><span style="color: #ff0086;
        font-weight: bold">Scheme+</span><span style="color: #ffffff">))</span>
    </div>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">{</span><span
style="color: #fb660a">L-init</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #ffffff">'(</span><span style="color: #0086f7; font-weight: bold">1</span> <span
style="color: #0086f7; font-weight: bold">3</span> <span style="color: #0086f7; font-weight: bold">4</span> <span
style="color: #0086f7; font-weight: bold">16</span> <span style="color: #0086f7; font-weight: bold">17</span> <span
style="color: #0086f7; font-weight: bold">24</span> <span style="color: #0086f7; font-weight: bold">45</span> <span
style="color: #0086f7; font-weight: bold">64</span> <span style="color: #0086f7; font-weight: bold">197</span> <span
style="color: #0086f7; font-weight: bold">256</span> <span style="color: #0086f7; font-weight: bold">275</span> <span
style="color: #0086f7; font-weight: bold">323</span> <span style="color: #0086f7; font-weight: bold">540</span> <span
style="color: #0086f7; font-weight: bold">723</span> <span style="color: #0086f7; font-weight: bold">889</span> <span
style="color: #0086f7; font-weight: bold">915</span> <span style="color: #0086f7; font-weight: bold">1040</span> <span
style="color: #0086f7; font-weight: bold">1041</span> <span style="color: #0086f7; font-weight: bold">1093</span> <span
style="color: #0086f7; font-weight: bold">1099</span> <span style="color: #0086f7; font-weight: bold">1111</span> <span
style="color: #0086f7; font-weight: bold">1284</span> <span style="color: #0086f7; font-weight: bold">1344</span> <span
style="color: #0086f7; font-weight: bold">1520</span> <span style="color: #0086f7; font-weight: bold">2027</span> <span
style="color: #0086f7; font-weight: bold">2500</span> <span style="color: #0086f7; font-weight: bold">2734</span> <span
style="color: #0086f7; font-weight: bold">3000</span> <span style="color: #0086f7; font-weight: bold">3267</span> <span
style="color: #0086f7; font-weight: bold">3610</span> <span style="color: #0086f7; font-weight: bold">4285</span> <span
style="color: #0086f7; font-weight: bold">5027</span><span style="color: #ffffff">)}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">t-init</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #0086f7; font-weight: bold">35267</span><span
style="color: #ffffff">}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">ls</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #ffffff">(length </span><span
style="color: #fb660a">L-init</span><span style="color: #ffffff">)}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">make-array</span> <span
style="color: #0086f7; font-weight: bold">0</span> <span style="color: #ffffff">{</span><span
style="color: #fb660a">ls</span> <span style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">t-init</span> <span
style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">})}</span>
</pre> </div>
    <div style="background: #111111; overflow:auto;width:auto;border:
      gray;border-width:.1em .1em .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">{</span><span
style="color: #fb660a">cpt</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #0086f7; font-weight: bold">0</span><span style="color: #ffffff">}</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; define cpt to 0</span>

<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">define </span><span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">subset-sum-guile</span> <span
style="color: #fb660a">L</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">ls</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #ffffff">(length </span><span
style="color: #fb660a">L</span><span style="color: #ffffff">)}</span>
  <span style="color: #ffffff">{</span><span style="color: #fb660a">dyn</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]}</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">cpt</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">cpt</span> <span
style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}}</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; cpt has been already defined at toplevel</span>
  
  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution</span>
  
  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">condx</span> <span
style="color: #ffffff">[{</span><span style="color: #fb660a">dyn</span> <span style="color: #fb660a">&lt;&gt;</span> <span
style="color: #0086f7; font-weight: bold">0</span><span style="color: #ffffff">}</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one?</span> <span
style="color: #fb660a">dyn</span><span style="color: #ffffff">)]</span>
	 <span style="color: #ffffff">[(null? </span><span style="color: #fb660a">L</span><span
style="color: #ffffff">)</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #0086f7; font-weight: bold">2</span><span style="color: #ffffff">}</span>  <span
style="color: #0086d2">#f</span><span style="color: #ffffff">]</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; return #f</span>
	 <span style="color: #ffffff">[</span><span style="color: #fb660a">exec</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">first</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">)}]</span>	 
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is the solution</span>
	 <span style="color: #ffffff">[{</span><span style="color: #fb660a">c</span> <span
style="color: #fb660a">=</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">}</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span style="color: #ffffff">[</span><span
style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">]</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}</span>  <span style="color: #0086d2">#t</span><span style="color: #ffffff">]</span>  <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return #t</span>
	 <span style="color: #ffffff">[</span><span style="color: #fb660a">exec</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">R</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">rest</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">)}]</span>	 
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; continue searching a solution in the rest</span>
	 <span style="color: #ffffff">[{</span><span style="color: #fb660a">c</span> <span
style="color: #fb660a">&gt;</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">}</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">s</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">subset-sum-guile</span> <span
style="color: #fb660a">R</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)}</span>
	  <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)}</span>
	<span style="color: #fb660a">s</span><span style="color: #ffffff">]</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return boolean value</span>
	<span style="color: #008800; font-style: italic; background-color: #0f140f">;; else : c &lt; t at this point</span>
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is part of a solution OR not part of a solution</span>
	 <span style="color: #ffffff">[</span><span style="color: #fb660a; font-weight: bold">else </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">s</span> <span style="color: #fb660a">&lt;+</span> <span
style="color: #ffffff">{(</span><span style="color: #ff0086; font-weight: bold">subset-sum-guile</span> <span
style="color: #fb660a">R</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">t</span> <span
style="color: #fb660a">-</span> <span style="color: #fb660a">c</span><span style="color: #ffffff">})</span> <span
style="color: #fb660a; font-weight: bold">or </span><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">subset-sum-guile</span> <span style="color: #fb660a">R</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">)}}</span>
	  <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)}</span>
	   <span style="color: #fb660a">s</span><span style="color: #ffffff">]))</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return boolean value</span>
</pre> </div>
    <p> </p>
    <br>
    <p><b>scheme@(guile-user)&gt; (subset-sum-guile&nbsp; L-init t-init)</b></p>
    <p><b>#t</b><br>
      <br>
    </p>
    <p style="text-align: center;">note: in the example above condx is a macro
      that allow execution of arbitrary code between conditionals
      clauses,general syntax is:</p>
    <p style="text-align: left; margin-left: 40px;">(<b>condx </b>[<i>condition
        statements </i>...]</p>
    <div style="margin-left: 40px;"> </div>
    <p style="text-align: left; margin-left: 40px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;












      [<b>exec</b><i> statements ... </i>]</p>
    <div style="margin-left: 40px;"> </div>
    <p style="text-align: left; margin-left: 40px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;












      ...</p>
    <div style="margin-left: 40px;"> </div>
    <p style="text-align: left; margin-left: 40px;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;












      [<b>else </b><i>statements</i> ...])</p>
    <div style="margin-left: 40px;"> </div>
    <p style="text-align: center;"><br>
    </p>
    <p style="text-align: left;">In classic Scheme the code would be like that
      which is longer than in Scheme+:</p>
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #fb660a; font-weight: bold">define </span><span style="color: #fb660a">cpt</span> <span
style="color: #0086f7; font-weight: bold">0</span><span style="color: #ffffff">)</span>

<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">define </span><span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">ssigma-proto</span> <span
style="color: #fb660a">L</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)</span>

  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">set! </span><span
style="color: #fb660a">cpt</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">cpt</span> <span
style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">})</span>
 
  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">define </span><span
style="color: #fb660a">ls</span> <span style="color: #ffffff">(length </span><span
style="color: #fb660a">L</span><span style="color: #ffffff">))</span>
  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">define </span><span
style="color: #fb660a">dyn</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">array-ref</span> <span
style="color: #fb660a">dyna</span> <span style="color: #fb660a">ls</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">))</span>
    
  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; dyna[ls][t] means 0: unknown solution, 1: solution found, 2: no solution</span>

  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">cond </span><span
style="color: #ffffff">[(not (zero? </span><span style="color: #fb660a">dyn</span><span
style="color: #ffffff">))</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one?</span> <span
style="color: #fb660a">dyn</span><span style="color: #ffffff">)]</span>
	<span style="color: #ffffff">[(null? </span><span style="color: #fb660a">L</span><span
style="color: #ffffff">)</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">array-set!</span> <span
style="color: #fb660a">dyna</span> <span style="color: #0086f7; font-weight: bold">2</span> <span
style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)</span> <span
style="color: #0086d2">#f</span><span style="color: #ffffff">]</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; return #f</span>
	
	<span style="color: #ffffff">[</span><span style="color: #fb660a; font-weight: bold">else </span><span
style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">let </span><span
style="color: #ffffff">[(</span><span style="color: #ff0086; font-weight: bold">c</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">first</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">))]</span>
		
		<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">if </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span style="color: #fb660a">=</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">}</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is the solution</span>
		      
		    <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">begin</span>
		      <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">array-set!</span> <span
style="color: #fb660a">dyna</span> <span style="color: #0086f7; font-weight: bold">1</span> <span
style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)</span>
		      <span style="color: #0086d2">#t</span><span style="color: #ffffff">)</span>  <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return #t</span>

		    <span style="color: #008800; font-style: italic; background-color: #0f140f">;; else</span>
		    <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">let </span><span
style="color: #ffffff">[(</span><span style="color: #ff0086; font-weight: bold">R</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">rest</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">))]</span>
		      
		      <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">if </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span style="color: #fb660a">&gt;</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">}</span>   <span style="color: #008800; font-style: italic; background-color: #0f140f">;; continue searching a solution in the rest</span>
			    
			  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">let </span><span
style="color: #ffffff">[(</span><span style="color: #ff0086; font-weight: bold">s</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">ssigma-proto</span> <span
style="color: #fb660a">R</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">))]</span>
			    <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">array-set!</span> <span
style="color: #fb660a">dyna</span>
					<span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)</span>
					<span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">)</span>
			      
			    <span style="color: #fb660a">s</span><span style="color: #ffffff">)</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return s</span>
			
			  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; else</span>
			  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c &lt; t at this point</span>
			  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is part of the solution or his approximation</span>
			  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; or c is not part of solution</span>
			  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">let </span><span
style="color: #ffffff">[(</span><span style="color: #ff0086; font-weight: bold">s</span> <span
style="color: #ffffff">{(</span><span style="color: #ff0086; font-weight: bold">ssigma-proto</span> <span
style="color: #fb660a">R</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">t</span> <span
style="color: #fb660a">-</span> <span style="color: #fb660a">c</span><span style="color: #ffffff">})</span> <span
style="color: #fb660a; font-weight: bold">or </span><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">ssigma-proto</span> <span style="color: #fb660a">R</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">)})]</span>
			    <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">array-set!</span> <span
style="color: #fb660a">dyna</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)</span>
					<span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">)</span>
			    <span style="color: #fb660a">s</span><span style="color: #ffffff">)))))</span>
	      <span style="color: #ffffff">]</span> <span style="color: #ffffff">))</span>
</pre> </div>
    <p style="text-align: left;"> <br>
    </p>
    <p style="text-align: left;"><br>
    </p>
    <p style="text-align: left;">If you want to use a single assignment operator
      <b>&lt;-</b> it is possible using a declarative programming style with the
      <b>declare </b>macro:</p>
    <p> </p>
    <p><!-- HTML generated using hilite.me --> </p>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">use-modules</span> <span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">Scheme+</span><span style="color: #ffffff">))</span>
</pre> </div>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">declare</span> <span style="color: #fb660a">L-init</span> <span
style="color: #fb660a">t-init</span> <span style="color: #fb660a">ls</span> <span
style="color: #fb660a">dyna</span> <span style="color: #fb660a">cpt</span><span
style="color: #ffffff">)</span>

<span style="color: #ffffff">{</span><span style="color: #fb660a">L-init</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">'(</span><span
style="color: #0086f7; font-weight: bold">1</span> <span style="color: #0086f7; font-weight: bold">3</span> <span
style="color: #0086f7; font-weight: bold">4</span> <span style="color: #0086f7; font-weight: bold">16</span> <span
style="color: #0086f7; font-weight: bold">17</span> <span style="color: #0086f7; font-weight: bold">24</span> <span
style="color: #0086f7; font-weight: bold">45</span> <span style="color: #0086f7; font-weight: bold">64</span> <span
style="color: #0086f7; font-weight: bold">197</span> <span style="color: #0086f7; font-weight: bold">256</span> <span
style="color: #0086f7; font-weight: bold">275</span> <span style="color: #0086f7; font-weight: bold">323</span> <span
style="color: #0086f7; font-weight: bold">540</span> <span style="color: #0086f7; font-weight: bold">723</span> <span
style="color: #0086f7; font-weight: bold">889</span> <span style="color: #0086f7; font-weight: bold">915</span> <span
style="color: #0086f7; font-weight: bold">1040</span> <span style="color: #0086f7; font-weight: bold">1041</span> <span
style="color: #0086f7; font-weight: bold">1093</span> <span style="color: #0086f7; font-weight: bold">1099</span> <span
style="color: #0086f7; font-weight: bold">1111</span> <span style="color: #0086f7; font-weight: bold">1284</span> <span
style="color: #0086f7; font-weight: bold">1344</span> <span style="color: #0086f7; font-weight: bold">1520</span> <span
style="color: #0086f7; font-weight: bold">2027</span> <span style="color: #0086f7; font-weight: bold">2500</span> <span
style="color: #0086f7; font-weight: bold">2734</span> <span style="color: #0086f7; font-weight: bold">3000</span> <span
style="color: #0086f7; font-weight: bold">3267</span> <span style="color: #0086f7; font-weight: bold">3610</span> <span
style="color: #0086f7; font-weight: bold">4285</span> <span style="color: #0086f7; font-weight: bold">5027</span><span
style="color: #ffffff">)}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">t-init</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #0086f7; font-weight: bold">35267</span><span
style="color: #ffffff">}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">ls</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">(length </span><span
style="color: #fb660a">L-init</span><span style="color: #ffffff">)}</span>
<span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">make-array</span> <span
style="color: #0086f7; font-weight: bold">0</span> <span style="color: #ffffff">{</span><span
style="color: #fb660a">ls</span> <span style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">t-init</span> <span
style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">})}</span>

<span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">define </span><span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">b</span><span style="color: #ffffff">)</span>
  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">if </span><span
style="color: #fb660a">b</span> <span style="color: #0086f7; font-weight: bold">1</span> <span
style="color: #0086f7; font-weight: bold">2</span><span style="color: #ffffff">))</span>

<span style="color: #ffffff">{</span><span style="color: #fb660a">cpt</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #0086f7; font-weight: bold">0</span><span
style="color: #ffffff">}</span>
</pre> </div>
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #fb660a; font-weight: bold">define </span><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">subset-sum-guile-dec</span> <span style="color: #fb660a">L</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">)</span>

  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">declare</span> <span
style="color: #fb660a">ls</span> <span style="color: #fb660a">dyn</span> <span style="color: #fb660a">c</span> <span
style="color: #fb660a">R</span> <span style="color: #fb660a">s</span><span style="color: #ffffff">)</span>
  
  <span style="color: #ffffff">{</span><span style="color: #fb660a">ls</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">(length </span><span
style="color: #fb660a">L</span><span style="color: #ffffff">)}</span>
  <span style="color: #ffffff">{</span><span style="color: #fb660a">dyn</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]}</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">cpt</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">cpt</span> <span
style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}}</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; cpt has been already defined at toplevel</span>
  
  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution</span>
  
  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">condx</span> <span
style="color: #ffffff">[{</span><span style="color: #fb660a">dyn</span> <span style="color: #fb660a">&lt;&gt;</span> <span
style="color: #0086f7; font-weight: bold">0</span><span style="color: #ffffff">}</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one?</span> <span
style="color: #fb660a">dyn</span><span style="color: #ffffff">)]</span>
	 <span style="color: #ffffff">[(null? </span><span style="color: #fb660a">L</span><span
style="color: #ffffff">)</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #0086f7; font-weight: bold">2</span><span style="color: #ffffff">}</span>  <span
style="color: #0086d2">#f</span><span style="color: #ffffff">]</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; return #f</span>
	 
	 <span style="color: #ffffff">[</span><span style="color: #fb660a">exec</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">first</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">)}]</span>	 
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is the solution</span>
	 <span style="color: #ffffff">[{</span><span style="color: #fb660a">c</span> <span
style="color: #fb660a">=</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">}</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span style="color: #ffffff">[</span><span
style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">]</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}</span>  <span style="color: #0086d2">#t</span><span style="color: #ffffff">]</span>  <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return #t</span>
	 
	 <span style="color: #ffffff">[</span><span style="color: #fb660a">exec</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">R</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">rest</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">)}]</span>	 
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; continue searching a solution in the rest</span>
	 <span style="color: #ffffff">[{</span><span style="color: #fb660a">c</span> <span
style="color: #fb660a">&gt;</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">}</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">s</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">subset-sum-guile-dec</span> <span
style="color: #fb660a">R</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)}</span>
	          <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)}</span>
		  <span style="color: #fb660a">s</span><span style="color: #ffffff">]</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return boolean value</span>
			
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; else : c &lt; t at this point</span>
	 <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is part of a solution OR not part of a solution</span>
	 <span style="color: #ffffff">[</span><span style="color: #fb660a; font-weight: bold">else </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">s</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">{(</span><span style="color: #ff0086; font-weight: bold">subset-sum-guile-dec</span> <span
style="color: #fb660a">R</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">t</span> <span
style="color: #fb660a">-</span> <span style="color: #fb660a">c</span><span style="color: #ffffff">})</span> <span
style="color: #fb660a; font-weight: bold">or </span><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">subset-sum-guile-dec</span> <span style="color: #fb660a">R</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">)}}</span>
	       <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)}</span>
	       <span style="color: #fb660a">s</span><span style="color: #ffffff">]))</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return boolean value</span>
</pre> </div>
    <p style="text-align: left;">It is also possible to use <b>return</b>
      keyword in function definition by <b>def </b>macro and have again
      another programming style with <b>if then else</b> that looks like
      traditionals language such as Python,Javascript,C/C++,etc... :</p>
    <p style="text-align: left;">This example mix many Scheme+ Style and
      illustrate the use of <b>return </b>in a Python/C++ style:</p>
    <p style="text-align: left;"><br>
    </p>
    <!-- HTML generated using hilite.me -->
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">use-modules</span> <span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">Scheme+</span><span style="color: #ffffff">))</span>
</pre> </div>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #fb660a">def</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">subset-sum-dyna</span> <span
style="color: #fb660a">L</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)</span>

  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">declare</span> <span
style="color: #fb660a">ls</span> <span style="color: #fb660a">dyn</span><span style="color: #ffffff">)</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; declare multiple variables</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">ls</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">(length </span><span
style="color: #fb660a">L</span><span style="color: #ffffff">)}</span>
  <span style="color: #ffffff">{</span><span style="color: #fb660a">dyn</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]}</span>

  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution</span>

  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">if </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">dyn</span> <span style="color: #fb660a">&lt;&gt;</span> <span
style="color: #0086f7; font-weight: bold">0</span><span style="color: #ffffff">}</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; IF or WHEN : it is the same thing here (only one statement)</span>
      <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">return</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one?</span> <span
style="color: #fb660a">dyn</span><span style="color: #ffffff">)))</span>

  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">when</span> <span
style="color: #ffffff">(null? </span><span style="color: #fb660a">L</span><span
style="color: #ffffff">)</span>
    <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #0086f7; font-weight: bold">2</span><span style="color: #ffffff">}</span>
    <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">return</span> <span
style="color: #0086d2">#f</span><span style="color: #ffffff">))</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">first</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">)}</span>

  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">when</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span style="color: #fb660a">=</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">}</span>  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is the solution</span>
    <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #0086f7; font-weight: bold">1</span><span style="color: #ffffff">}</span>
    <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">return</span> <span
style="color: #0086d2">#t</span><span style="color: #ffffff">))</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">R</span> <span
style="color: #fb660a">&lt;+</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">rest</span> <span
style="color: #fb660a">L</span><span style="color: #ffffff">)}</span> <span style="color: #008800; font-style: italic; background-color: #0f140f">;; continue searching a solution in the rest</span>

  <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">declare</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)</span>
  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">if </span><span
style="color: #ffffff">{</span><span style="color: #fb660a">c</span> <span style="color: #fb660a">&gt;</span> <span
style="color: #fb660a">t</span><span style="color: #ffffff">}</span>  <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is to big to be a solution</span>
    <span style="color: #ffffff">{</span><span style="color: #fb660a">s</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">subset-sum-dyna</span> <span
style="color: #fb660a">R</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)}</span>
    <span style="color: #008800; font-style: italic; background-color: #0f140f">;; c is part of the solution or c is not part of solution</span>
    <span style="color: #ffffff">{</span><span style="color: #fb660a">s</span> <span
style="color: #fb660a">&lt;-</span> <span style="color: #ffffff">{(</span><span
style="color: #ff0086; font-weight: bold">subset-sum-dyna</span> <span style="color: #fb660a">R</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">t</span> <span style="color: #fb660a">-</span> <span
style="color: #fb660a">c</span><span style="color: #ffffff">})</span> <span style="color: #fb660a; font-weight: bold">or </span><span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">subset-sum-dyna</span> <span
style="color: #fb660a">R</span> <span style="color: #fb660a">t</span><span style="color: #ffffff">)}})</span>

  <span style="color: #ffffff">{</span><span style="color: #fb660a">dyna</span><span
style="color: #ffffff">[</span><span style="color: #fb660a">ls</span> <span style="color: #fb660a">t</span><span
style="color: #ffffff">]</span> <span style="color: #fb660a">&lt;-</span> <span
style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">one-two</span> <span
style="color: #fb660a">s</span><span style="color: #ffffff">)}</span>
  <span style="color: #fb660a">s</span><span style="color: #ffffff">)</span> <span
style="color: #008800; font-style: italic; background-color: #0f140f">;; return boolean value</span>
</pre> </div>
    <h2><br>
    </h2>
    <h2><br>
    </h2>
    <h2>10<u>. History of project:</u></h2>
    <p>First i developped a new set of LET special forms with less bracket use
      and after i decided to use another assignment scheme based on infix
      operator for a little part,at some point the new assignment scheme was
      became so perfect that we no more need the set of LET special forms.But i
      also released the new set of LET special forms even if i consider those
      useless.Unfornunately Scheme do not allow to use only a single assignment
      operator because&nbsp; of declaration (define) of variables and set! are
      different and it does not exist a define-or-set! function. So i only had
      the choice to use 2 assignment operators, one acting as <b>define</b> (<b>&lt;+</b>)
      and the other as <b>set!</b> (<b>&lt;-</b>) or a single assignment
      operator <b>&lt;- </b>and a declarative macro called <b>declare</b>
      which should be interesting for typed Scheme implementations too.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2>11.<u>Additional documentation:</u></h2>
    <p><b>condx</b> is a macro that allow execution of arbitrary code between
      conditionals clauses,syntax is :</p>
    <p>(<b>condx </b>[<i>condition statements </i>...] </p>
    <p style="text-align: left;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;












      [<b>exec</b><i> statements ... </i>]</p>
    <p style="text-align: left;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;












      ...</p>
    <p style="text-align: left;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;












      [<b>else </b><i>statements</i> ...])</p>
    <p style="text-align: left;"><b>condx</b> is not a major feature of <b>Scheme+</b>
      but it can replace a lot of 'if then <b>elif'</b>, i dislike and still
      misunderstand sometimes, '<b>else if</b>' since BASIC !<br>
    </p>
    <p style="text-align: left;">&nbsp;<br>
    </p>
    <p style="text-align: left;">defining new functions with <b>def </b>instead












      of the classic define we can now use <b>return</b> to escape immediately
      returning the value v:</p>
    <p style="text-align: left;"><b>(return </b>v<b>) </b><b> </b></p>
    <p style="text-align: left;"><br>
    </p>
    <p style="text-align: left;"><b>$ </b>is a macro, it is a shortcut for <b>begin.<br>
      </b></p>
    <p style="text-align: left;">But <b>begin</b> is dual in Scheme, it expands
      differently in <i>expression </i>context and <i>definition </i>context.
      See this for more information:</p>
    <p style="text-align: left;"><a href="https://www.gnu.org/software/guile/manual/html_node/begin.html"
        moz-do-not-send="true">https://www.gnu.org/software/guile/manual/html_node/begin.html</a><br>
    </p>
    <p style="text-align: left;">For this reason it is sometimes necessary to
      use a classic <b>let </b>to create a block,for example when defining a <i>clozure.<br>
      </i></p>
    <p style="text-align: left;">As example consider those two definitions where
      the first - foo - is erroneous:<br>
    </p>
    <!-- HTML generated using hilite.me -->
    <div style="background: #f8f8f8;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%">(<span style="color: #AA22FF; font-weight: bold">define </span><span
style="color: #B8860B">foo</span> (<span style="color: #AA22FF; font-weight: bold">begin </span>(<span
style="color: #AA22FF; font-weight: bold">define </span><span style="color: #B8860B">x</span> <span
style="color: #666666">1</span>)
                   (<span style="color: #AA22FF; font-weight: bold">define </span><span
style="color: #B8860B">y</span> <span style="color: #666666">2</span>)
                   (<span style="color: #AA22FF; font-weight: bold">lambda </span>(<span
style="color: #00A000">t</span>) (<span style="color: #AA22FF">+ </span><span style="color: #B8860B">x</span> <span
style="color: #B8860B">y</span> <span style="color: #B8860B">t</span>))))

(<span style="color: #AA22FF; font-weight: bold">define </span><span style="color: #B8860B">bar</span> (<span
style="color: #AA22FF; font-weight: bold">let </span>()
              (<span style="color: #AA22FF; font-weight: bold">define </span><span
style="color: #B8860B">x</span> <span style="color: #666666">1</span>)
              (<span style="color: #AA22FF; font-weight: bold">define </span><span
style="color: #B8860B">y</span> <span style="color: #666666">2</span>)
              (<span style="color: #AA22FF; font-weight: bold">lambda </span>(<span
style="color: #00A000">t</span>) (<span style="color: #AA22FF">+ </span><span style="color: #B8860B">x</span> <span
style="color: #B8860B">y</span> <span style="color: #B8860B">t</span>))))
</pre> </div>
    <p style="text-align: left;">error: define: not allowed in an expression
      context in: (define x 1)<br>
    </p>
    <p style="text-align: left;">for this reason i provide a <b>&amp; </b>macro
      that is defined with a <b>let </b>as this:<br>
    </p>
    <p style="text-align: left;"> </p>
    <pre style="margin: 0; line-height: 125%">(<span style="color: #AA22FF; font-weight: bold">define-syntax </span><span
style="color: #B8860B">&amp;</span>
  (<span style="color: #AA22FF; font-weight: bold">syntax-rules </span>()
    ((<span style="color: #00A000">_</span> <span style="color: #B8860B">ev</span>)  <span
style="color: #B8860B">ev</span>)
    ((<span style="color: #00A000">_</span> <span style="color: #B8860B">ev</span> <span
style="color: #666666">...</span>) (<span style="color: #AA22FF; font-weight: bold">let </span>() <span
style="color: #B8860B">ev</span> <span style="color: #666666">...</span>))))
</pre>
    <p>We can now define the previous bar function in Scheme+ like this:<br>
    </p>
    <p> </p>
    <div id="highlight-preview">
      <pre class="scheme" style="font-family:monospace;"><span style="color: #006600;">(</span>def bar <span
style="color: #006600;">(</span><span style="color: #006600;">&amp;</span> <span
style="color: #006600;">(</span>declare x y<span style="color: #006600;">)</span>
            <span style="color: #006600;">{</span>x <span style="color: #006600;">&lt;-</span> <span
style="color: #cc66cc;">1</span><span style="color: #006600;">}</span> 
            <span style="color: #006600;">{</span>y <span style="color: #006600;">&lt;-</span> <span
style="color: #cc66cc;">2</span><span style="color: #006600;">}</span>
            <span style="color: #006600;">(</span><span style="color: #b1b100;">lambda</span> <span
style="color: #006600;">(</span><span style="color: #b1b100;">t</span><span style="color: #006600;">)</span> <span
style="color: #006600;">(</span><span style="color: #006600;">+</span> x y <span
style="color: #b1b100;">t</span><span style="color: #006600;">)</span><span style="color: #006600;">)</span><span
style="color: #006600;">)</span><span style="color: #006600;">)<br><br></span><span
style="color: #006600;"></span></pre>
      <pre>(bar 7)<br>10</pre>
    </div>
    <p style="text-align: left;"> </p>
    <h2 style=" text-align: left;">12.<u>Caveats:</u></h2>
    <p>Inherent to Scheme language it as not been possible to simplify more
      assignment operator in a single one case. This is due to the fact that it
      can not be written a macro that do define-or-set! and that define can not
      be placed any where. See discussion in <a href="https://lists.gnu.org/archive/html/guile-devel/2021-09/msg00031.html"
        target="_blank">Guile devel mailing list archive in September 2021</a>
      For the place of define, by chance,some Scheme implementation allow define
      to be placed almost anywhere.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2>13.<u>Implementation:</u></h2>
    <p>Mainly with <b>Scheme macros</b> which are not recursive (except in some
      obsolete features),so expansion is fast and code also. It also allows a
      great portabilty from one Scheme implementation to another one.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2>14.<u>Future:</u></h2>
    <p>Scheme+ will be implemented for other Scheme. Racket (former DrScheme)
      first,....</p>
    <p>Next version will integrate more features, concerning <b>multiple values</b>
      return for example.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2><u>15.Extra features:</u> </h2>
    <p>The <b>growable vector class</b> which is not specific to Scheme+ is
      included in Scheme+ because it is intrinsic with <b>&lt;-</b> operator of
      Scheme+.</p>
    <p>"Growable vectors" are vectors with size growing dynamically when
      necessary. The documentation and examples are in the source code header.</p>
    <p>Usage: <b>(use-modules (growable-vector))</b></p>
    <p>Not using the module will just show a warning like "possibly undefined
      function growable-vector?" as it is used in assignment.scm.No problem till
      you do not create a growable vector, and when you will do it you will have
      loaded the growable module class before and the warning message will
      vanish.This is the force of Scheme on other language:you can have an
      undefined function&nbsp; in your source code and it can run without
      problem (till the function is not called of course! which is the case in
      my code if no growable vector exist in your code too)</p>
    <p>Other features include a part of my personal functional library when
      functions are not existing already in some <a href="https://srfi.schemers.org/"
        target="_blank">SRFI (Scheme Request For Implementations)</a>.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2>16.<u>Obsolete features:</u></h2>
    <p>As mentioned early the project idea was initially to enhance the set of
      LET special form. The use of the LET set is no more need with the new
      assignment operator but i release anyway those set of enhanced and
      simplified LETs, i just give some examples as names are self-explanatory.</p>
    <p>The way to be able to use the new set of LET special form is to load the
      file let.scm which is available in the Scheme+ directory:</p>
    <p><b>(load "let.scm")</b></p>
    <p>Examples:</p>
    <p><!-- HTML generated using hilite.me --></p>
    <div style="background: #272822;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">let&lt;-rec*</span> <span style="color: #f8f8f2">[x</span> <span
style="color: #f8f8f2">&lt;-</span> <span style="color: #ae81ff">1</span>
            <span style="color: #f8f8f2">y</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">(+ x</span> <span style="color: #ae81ff">1</span><span style="color: #f8f8f2">)</span>
            <span style="color: #f8f8f2">z</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">(+ </span><span style="color: #ae81ff">2</span> <span style="color: #f8f8f2">y)]</span>
         <span style="color: #f8f8f2">z)</span>
<span style="color: #ae81ff">4</span>
</pre> </div>
    <p> </p>
    <p>and here is the source code of this <b>recursive macro</b> defined with
      an <b>accumulator</b>:</p>
    <p><!-- HTML generated using hilite.me --></p>
    <div style="background: #272822;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #f8f8f2">(</span><span
style="color: #66d9ef">define-syntax </span><span style="color: #f8f8f2">let&lt;-rec*</span>

  <span style="color: #f8f8f2">(</span><span style="color: #66d9ef">syntax-rules </span><span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">&lt;-</span><span style="color: #f8f8f2">)</span>

    <span style="color: #f8f8f2">((</span><span style="color: #a6e22e">_</span> <span
style="color: #f8f8f2">()</span> <span style="color: #f8f8f2">expr</span> <span
style="color: #f92672">...</span><span style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">(</span><span
style="color: #66d9ef">begin </span><span style="color: #f8f8f2">expr</span> <span
style="color: #f92672">...</span><span style="color: #f8f8f2">))</span> <span style="color: #75715e">;;  case empty let</span>
    <span style="color: #f8f8f2">((</span><span style="color: #a6e22e">_</span> <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val1)</span> <span style="color: #f8f8f2">expr</span>  <span
style="color: #f92672">...</span><span style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">letrec*</span> <span style="color: #f8f8f2">((</span><span
style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">val1))</span> <span
style="color: #f8f8f2">expr</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">))</span> <span style="color: #75715e">;; case single binding</span>
    <span style="color: #f8f8f2">((</span><span style="color: #a6e22e">_</span> <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val1</span> <span style="color: #75715e">;; multiple binding</span>
	 <span style="color: #f8f8f2">var2</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val2</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">)</span>
	<span style="color: #f8f8f2">expr</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">)</span>
     <span style="color: #f8f8f2">(</span><span style="color: #a6e22e">%parse-letrec-bindings-and-evaluate-expressions</span> <span
style="color: #f8f8f2">((</span><span style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">val1))</span>  <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">var2</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val2</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">expr</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">))))</span>

<span style="color: #75715e">;; recursive macro with accumulator</span>
<span style="color: #f8f8f2">(</span><span style="color: #66d9ef">define-syntax </span><span
style="color: #f8f8f2">%parse-letrec-bindings-and-evaluate-expressions</span>

  <span style="color: #f8f8f2">(</span><span style="color: #66d9ef">syntax-rules </span><span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">&lt;-</span><span style="color: #f8f8f2">)</span>

    <span style="color: #f8f8f2">((</span><span style="color: #a6e22e">_</span> <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">bindings</span> <span
style="color: #f92672">...</span><span style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val1)</span> <span style="color: #f8f8f2">expr</span> <span
style="color: #f92672">...</span><span style="color: #f8f8f2">)</span>  <span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">letrec*</span> <span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">bindings</span> <span style="color: #f92672">...</span> <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">val1))</span> <span
style="color: #75715e">;; last binding</span>
							  <span style="color: #f8f8f2">expr</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">))</span> <span style="color: #75715e">;; we evaluate expressions</span>
    <span style="color: #f8f8f2">((</span><span style="color: #a6e22e">_</span> <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">bindings</span> <span
style="color: #f92672">...</span><span style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">var1</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val1</span> <span style="color: #75715e">;; multiple binding</span>
			<span style="color: #f8f8f2">var2</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val2</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">expr</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">)</span>
     <span style="color: #75715e">;; store (var1 val1) binding in accumulator and continue parsing</span>
       <span style="color: #f8f8f2">(</span><span style="color: #a6e22e">%parse-letrec-bindings-and-evaluate-expressions</span> <span
style="color: #f8f8f2">(</span><span style="color: #a6e22e">bindings</span> <span
style="color: #f92672">...</span> <span style="color: #f8f8f2">(</span><span style="color: #a6e22e">var1</span> <span
style="color: #f8f8f2">val1))</span> <span style="color: #f8f8f2">(</span><span
style="color: #a6e22e">var2</span> <span style="color: #f8f8f2">&lt;-</span> <span
style="color: #f8f8f2">val2</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">)</span> <span style="color: #f8f8f2">expr</span> <span style="color: #f92672">...</span><span
style="color: #f8f8f2">))))</span> 
    
</pre> </div>
    <p> </p>
    <br>
    <p>a few others special forms simplifying the LETs (but still obsolete in my
      opinion):</p>
    <p>let-arrow* :</p>
    <p><!-- HTML generated using hilite.me --></p>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">let-arrow*</span> <span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">x</span> <span style="color: #ffffff">←</span> <span
style="color: #0086f7; font-weight: bold">1</span>
 	     <span style="color: #fb660a">y</span> <span style="color: #ffffff">←</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">x</span> <span style="color: #fb660a">+</span> <span
style="color: #0086f7; font-weight: bold">1</span><span style="color: #ffffff">})</span>
           <span style="color: #fb660a">y</span><span style="color: #ffffff">)</span>
<span style="color: #0086f7; font-weight: bold">2</span>
</pre> </div>
    <p> </p>
    <p><!-- HTML generated using hilite.me --></p>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">let-arrow*</span> <span style="color: #ffffff">({</span><span
style="color: #fb660a">x</span> <span style="color: #ffffff">←</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}</span>
             <span style="color: #ffffff">{</span><span style="color: #fb660a">y</span> <span
style="color: #ffffff">←</span> <span style="color: #ffffff">{</span><span style="color: #fb660a">x</span> <span
style="color: #fb660a">+</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">}})</span>
           <span style="color: #fb660a">x</span>
           <span style="color: #fb660a">y</span><span style="color: #ffffff">)</span>

<span style="color: #0086f7; font-weight: bold">2</span>
</pre> </div>
    <p> </p>
    <p><!-- HTML generated using hilite.me --></p>
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">let-arrow*</span> <span style="color: #ffffff">[</span> <span
style="color: #fb660a">x</span> <span style="color: #0086f7; font-weight: bold">1</span>
              <span style="color: #fb660a">y</span> <span style="color: #ffffff">(+ </span><span
style="color: #fb660a">x</span> <span style="color: #0086f7; font-weight: bold">1</span><span
style="color: #ffffff">)</span>
              <span style="color: #fb660a">z</span> <span style="color: #ffffff">(+ </span><span
style="color: #0086f7; font-weight: bold">2</span> <span style="color: #fb660a">y</span><span
style="color: #ffffff">)</span> <span style="color: #ffffff">]</span>
            <span style="color: #fb660a">z</span> <span style="color: #fb660a">y</span><span
style="color: #ffffff">)</span>

<span style="color: #0086f7; font-weight: bold">2</span>
</pre> </div>
    <p> </p>
    <p><br>
    </p>
    <p>letrec-arrow* , the same as let-arrow* but for recursive functions:</p>
    <!-- HTML generated using hilite.me -->
    <div style="background: #111111;
      overflow:auto;width:auto;border:solid gray;border-width:.1em .1em      .1em .8em;padding:.2em .6em;">
      <pre style="margin: 0; line-height: 125%"><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">letrec-arrow*</span> <span style="color: #ffffff">[</span> <span
style="color: #fb660a">fact</span> <span style="color: #ffffff">←</span> <span style="color: #ffffff">(</span><span
style="color: #fb660a; font-weight: bold">lambda </span><span style="color: #ffffff">(</span><span
style="color: #ff0086; font-weight: bold">n</span><span style="color: #ffffff">)</span>
 			  <span style="color: #ffffff">(</span><span style="color: #fb660a; font-weight: bold">if </span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">n</span> <span style="color: #fb660a">=</span> <span
style="color: #0086f7; font-weight: bold">1</span><span style="color: #ffffff">}</span>
 			       <span style="color: #0086f7; font-weight: bold">1</span>
                               <span style="color: #ffffff">{</span><span style="color: #fb660a">n</span> <span
style="color: #fb660a">*</span> <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">fact</span> <span
style="color: #ffffff">{</span><span style="color: #fb660a">n</span> <span style="color: #fb660a">-</span> <span
style="color: #0086f7; font-weight: bold">1</span><span style="color: #ffffff">})}</span> <span
style="color: #ffffff">))</span> <span style="color: #ffffff">]</span>
 	       <span style="color: #ffffff">(</span><span style="color: #ff0086; font-weight: bold">fact</span> <span
style="color: #0086f7; font-weight: bold">5</span><span style="color: #ffffff">))</span>

<span style="color: #0086f7; font-weight: bold">120</span>
</pre> </div>
    <br>
    <h2><br>
    </h2>
    <h2><u>17.Comments:</u></h2>
    <p>Github web site technology force me to convert this page in Github Markup
      Language, perheaps instead of HTML should i have make it directly in GML
      but i did not know it existed before doing it. I did not want to make this
      documentation so long and graphically complex to set up. Well it's done! <i><b>i
          hope you get interest in reading at least 10% :-)</b></i> . And get as
      pleasure to read and use than i take making and writing it.<br>
      <b>This documentation is at end and it is quite impressive how long it
        takes to document things! </b>Even if i suppose&nbsp; i could have
      forget&nbsp; things to talk about.<br>
    </p>
    <p><br>
    </p>
    <div style="text-align: right;">
      <h2><a href="https://youtu.be/FLUrPy_ah18" target="_blank"><i>That</i></a><a
href="https://youtu.be/ms4DpPfXPNY" target="_blank"><i>'s </i></a>
<a href="https://youtu.be/mMx228QVT3E" target="_blank"><i>all </i></a>
<a href="https://youtu.be/krRMKAErUNI" target="_blank"><i>folks</i></a>
<a href="https://youtu.be/_kQHOJhzoQw" target="_blank"><i> !</i></a></h2>
    </div>
    <p> </p>


</body>

