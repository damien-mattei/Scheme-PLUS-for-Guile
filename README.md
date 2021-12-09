<body>
  <p>
      <br>
  </p>
  <div align="center">
    <br>
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
    <p style="text-align: center;">
	<u>Display options:</u><br>
    <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile/blob/main/README.md">Click here for a larger page view that improve code
  display.</a>
  <br>
  <br>
    <a href="https://damien-mattei.github.io/Scheme-PLUS-for-Guile/Scheme+.html">Click here for the light HTML page version of this
  page.</a>
  <br>
  <br>
    <a
  href="https://damien-mattei.github.io/Scheme-PLUS-for-Guile/Scheme+io.html">Click
  here for the dark Hacker Theme version of this page.</a>
   </p>
    <p><br>
    </p>
    <h2><u>1. Scheme+ at a glance:&nbsp;</u></h2>
    <p><b>Scheme+ is an extension of the syntax of the <a href="https://en.wikipedia.org/wiki/Scheme_%28programming_language%29"
          target="_blank">Scheme</a> language.</b><br>
    </p>
	<p><b>The idea of Scheme+ first come to my mind when comparing the
  assignation syntax used in Clojure with Scheme and Lisp, and later
  the easyness of creating and assigning variables in Python
  completely convince me that the Scheme system have to be enhanced.</b><br>
    </p>
    <p>The <b>idea</b> of <b>Scheme+</b> is also came from this : <b>
	<i>"Why is it
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
    <p style="margin-left: 40px;">The same thing with a new symbol
      ←&nbsp; :&nbsp; (unicode 2190 in hexadecimal that can be enter under Linux
      with Ctrl-Shift u 2190 Enter or Space bar)</p>
    <p style="margin-left: 40px;"><b>{x ← y}</b></p>
    <div style="margin-left: 40px;"> </div>
    <p style="margin-left: 40px;">Assignment from right to left : <b>{y ← 7}</b></p>
    <p style="margin-left: 40px;">Assignment from left to right :
  <b>{7 → y}</b></p>
<p><font size="+3">⥆</font>&nbsp; or &lt;+ : Define and assign a
new variable in the current environment ,example : <font
size="+2">{v ⥆ 7}</font></p>
<p style="margin-left: 40px;">
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
</p>
<p style="margin-left: 40px;">(unicode 2946 , on <a
href="https://apple.stackexchange.com/questions/183045/how-can-i-type-unicode-characters-without-using-the-mouse"
moz-do-not-send="true">Mac OS use </a>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<a
href="https://apple.stackexchange.com/questions/183045/how-can-i-type-unicode-characters-without-using-the-mouse"
moz-do-not-send="true">CTRL+CMD+space to bring up the characters
popover, </a>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
<a
href="https://apple.stackexchange.com/questions/183045/how-can-i-type-unicode-characters-without-using-the-mouse"
moz-do-not-send="true">then type in u + unicode and hit Enter to
get it</a>)<br>
<br>
</p>
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
    <p style="margin-left: 40px;"><b><b>{</b>T</b><b>[m n] &lt;- </b><b><b>T</b><b>[m{n + 1}]</b>}</b></p>
    <p style="margin-left: 40px;"><br>
      <b><b>{</b>T</b><b>[m n] ← </b><b><b>T</b><b>[m {n + 1}]</b>}</b></p>
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
    <p>Here is an example of the previous features:  </p>

{% highlight scheme %}
```scheme

(use-modules (Scheme+))


(declare L-init t-init ls dyna cpt)

{L-init <- '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init <- 35267}
{ls <- (length L-init)}
{dyna <- (make-array 0 {ls + 1} {t-init + 1})}

(define (one-two b)
  (if b 1 2))

{cpt <- 0}


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
{% endhighlight %}

<pre>
(subset-sum-dynamic  L-init t-init)
#t
<br>
</pre>
<br>
<br>
<p>
  Warnings about code highlighting tag due to bugs in Github Markdown
  system:<br>
  <br>
  -if you read this page on github.com the code examples display
  encapsuled between tags 'highlight scheme' and 'endhighlight'
  composed also with {} and % characters.<br>
  <br>
  -if you read this page on github.io the code examples display
  starting with 3 backquotes char , the tag 'scheme' and ending again
  with 3 backquotes char. The curly brackets {} display in a
  rectangular with red background because the Github Jekill rouge
  theme (unfortunately the ony one available) does not understand the
  infix notation of Scheme.This is,of course, not what i expected but i
  can not find any issue to this problem.<br>
  <br>
  Those weirds Markdown and tags should not be displayed and are not part of Scheme+ examples.
  <a
    href="https://damien-mattei.github.io/Scheme-PLUS-for-Guile/Scheme+.html"
    target="_blank">for a better view click here (or do not care of
  highlighting tag and triple backquotes when code is displayed!)</a><br>
    </p>

<br>
<br>

  <p>Another example with Guile Scheme in the REPL (Read Eval Print Loop):</p>
    <p> </p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">mattei@MBP-Touch-Bar ~ % cd
        Dropbox/git/Scheme-PLUS-for-Guile</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">mattei@MBP-Touch-Bar
        Scheme-PLUS-for-Guile % guile</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">GNU Guile 3.0.7</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">Copyright (C) 1995-2021 Free
        Software Foundation, Inc.</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt;
        (use-modules (Scheme+))</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; note: source file
        ./Scheme+.scm</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; <span class="Apple-converted-space">&nbsp;
          &nbsp; &nbsp; </span>newer than compiled<span class="Apple-converted-space">
          <br>
        </span></font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; compiling ./Scheme+.scm</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">;;; compiled<span class="Apple-converted-space">&nbsp;</span></font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt;
        (declare L)</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt; {L
        &lt;- '(1 2 3 4)}</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">$1 = (1 2 3 4)</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt; L</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">$2 = (1 2 3 4)</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">scheme@(guile-user)&gt; {L
        &lt;- (list 1 2 3 4 5)}</font></p>
    <p style="margin: 0.0px 0.0px 0.0px 0.0px; background-color:
      #990033"><font style="font: 18.0px Menlo; font-variant-ligatures:
        no-common-ligatures" size="5" face="Menlo">$3 = (1 2 3 4 5)</font></p>
    <br>
<p>Hash Tables support :</p>
    <pre>(use-modules (Scheme+))
(use-modules (srfi srfi-69))  ;; support for SRFI 69 <meta http-equiv="content-type"
content="text/html; charset=utf-8">Basic hash tables

{my-hash-tbl &lt;+ (make-hash-table)} ;; add an object in the current environment, here a hash table
(hash-table? my-hash-tbl)
#t
<br>{my-hash-tbl["three"] &lt;- 3}
3<br><br>{my-hash-tbl["three"]}
3

</pre>
    <p>for the rest read below and the documentation.</p>
    <br>
    <h2>2.<u>Download Scheme+:</u></h2>
    <p>The source code of the <b>Scheme+ module</b>, currently only for <a href="https://www.gnu.org/software/guile/"
        target="_blank">Guile implementation of Scheme</a> can be retrieved
      here: </p>
    <p><br>
    </p>
<h3 style="    text-align: center;">&gt;&gt;&gt;&nbsp;&nbsp; <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile/zipball/main"
        target="_blank">Download Scheme+ as .zip</a>&nbsp;&nbsp; &lt;&lt;&lt;<br>
			</h3>
<p><br>
<h3 style="    text-align: center;">&gt;&gt;&gt;&nbsp;&nbsp; <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile/tarball/main"
        target="_blank">Download Scheme+ as .tar.gz</a>&nbsp;&nbsp; &lt;&lt;&lt;<br>
		</h3>
<p><br>
    </p>
    <h3 style="    text-align: center;">&gt;&gt;&gt;&nbsp;&nbsp; <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile"
        target="_blank">https://github.com/damien-mattei/Scheme-PLUS-for-Guile
        --&gt; Code --&gt; Download ZIP</a>&nbsp;&nbsp; &lt;&lt;&lt;<br>
    </h3>
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
    <a href="https://www.gnu.org/software/guile/"  target="_blank"><img moz-do-not-send="true" src="guile-logo.png"
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
    <p>Guagno - Island of Corsica
    </p>
    <br>
    <p>To subit an issue, a bug or something else related with the Scheme+ code
      use the <a href="https://github.com/damien-mattei/Scheme-PLUS-for-Guile/issues"
        target="_blank">Github issues system</a>.<br>
    </p>
    <p><br>
    </p>
    <h2>5.<u>Installation:</u></h2>
    <p>Of course you can use Scheme+ module directly from the directory where
      the downloaded and expanded Scheme files of Scheme+ are but Scheme Guile
      have rules to install module sites.</p>
    <p>Those rules are documented here: <a href="https://www.gnu.org/software/guile/manual/html_node/Installing-Site-Packages.html"
        target="_blank">https://www.gnu.org/software/guile/manual/html_node/Installing-Site-Packages.html</a></p>
    <p>For your convenience here is how you can install Scheme+ in a Guile
      Scheme distribution:</p>
    <p style="margin-left: 40px;">Launch guile and find the directory of the
      site modules: <br>
    </p>
    <p style="margin-left: 40px;"><code>scheme@(guile-user)&gt; (%site-dir)</code><br>
      <code>"/usr/share/guile/site/3.0"</code></p>
    <p style="margin-left: 40px;"><br>
    </p>
    <p style="margin-left: 40px;">Create the directory (recursively) if it does
      not already exist: </p>
    <p style="margin-left: 40px;"><code>sudo mkdir -p /usr/share/guile/site/3.0</code><br>
    </p>
    <p style="margin-left: 40px;"><br>
    </p>
    <p style="margin-left: 40px;">Copy the files:</p>
    <p style="margin-left: 40px;"><code>sudo cp *.scm /usr/share/guile/site/3.0</code></p>
    <p style="margin-left: 40px;"><br>
    </p>
    <p style="margin-left: 40px;">Then you can use Scheme+ from
      every directory you want as it is site installed:</p>
    <p style="margin-left: 40px;"><code>(base) mattei@pc-mattei:~/Dropbox/git$
        guile</code><br>
        <code>GNU Guile 3.0.1</code><br>
        <code>Copyright (C) 1995-2020 Free Software Foundation, Inc.</code><br>
        <br>
	<code>scheme@(guile-user)&gt; (use-modules (Scheme+))</code><br>
        <code>;;; note: auto-compilation is enabled, set GUILE_AUTO_COMPILE=0</code><br>
        <code>;;;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; or pass the --no-auto-compile
        argument to disable.</code><br>
        <code>;;; compiling /usr/share/guile/site/3.0/Scheme+.scm</code><br>
        <code>;;; compiling /usr/share/guile/site/3.0/growable-vector.scm</code><br>
        <code>;;; compiled /home/mattei/.cache/guile/ccache/3.0-LE-8-4.2/usr/share/guile/site/3.0/growable-vector.scm.go</code><br>
	<code>;;; compiled /home/mattei/.cache/guile/ccache/3.0-LE-8-4.2/usr/share/guile/site/3.0/Scheme+.scm.go</code><br><code>scheme@(guile-user)&gt;
	{x &lt;+ 7}</code><br>
        <code>scheme@(guile-user)&gt; x</code><br>
        <code>7</code></p>
		<br>
		<br>
<h2><u>6. Idea and rationale:</u></h2>
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
    <p>This intuitive notation works also with Hash Tables.</p>
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
    <h2><u>7. Features:</u></h2>
    <p>The list of new features is subject to change and will grow by time, this
      version allows:</p>
    <ul>
      <li>use of infix SRFI 105 "Curly Infix" for some syntax forms</li>
      <li>Assignment operator : <b>&lt;-</b> (can be used in infix syntax and
        from left to right and right to left)</li>
      <li>Square Bracket operator<b> []</b> working with Scheme <b>Vectors,
          Arrays,Multidimentional Arrays</b>,Dynamic Arrays (my growable vector
        class),<b>Hash Tables</b>...</li>
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
    <h2><u>8. First Examples:</u></h2>
    <h3 style=" margin-left: 40px;">The <a href="https://en.wikipedia.org/wiki/Fibonacci_number"
        target="_blank">Fibonacci sequence</a>:</h3>
    <p style="margin-left: 40px;">This example is written in Scheme with and
      without infix notation just to allow the reader to be familiar with "curly
      infix" notation which is not part of Scheme+ but used by it:</p>
    <p style="margin-left: 40px;">Here is the definition in classic prefix
      Scheme:</p>

{% highlight scheme %}
```scheme


(define (fibonacci n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))


```
{% endhighlight %}

<p style="margin-left: 40px;">And here is a definition using "Curly Infix"
      SRFI 105:</p>

{% highlight scheme %}
```scheme


(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))


```
{% endhighlight %}

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



{% highlight scheme %}
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
{% endhighlight %}


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
<p>

{% highlight scheme %}
```scheme


(use-modules (Scheme+))

{size <+ 10000}
{memo <+ (make-vector size 0)} 

(define (fibdyna n)
   (cond ({n < 2} n)
         ({memo[n] <> 0} {memo[n]})
         (else {memo[n] <- {(fibdyna {n - 1}) + (fibdyna {n - 2})}}) ))


```
{% endhighlight %}

</p>
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


{% highlight scheme %}
```scheme


(use-modules (Scheme+))

(declare size1 memo1)

{size1 <- 10000}
{memo1 <- (make-vector size 0)} 
  
(define (fibdyna1 n)
  (cond ({n < 2} n)
	({memo1[n] <> 0} {memo1[n]})
	(else {memo1[n] <- {(fibdyna1 {n - 1}) + (fibdyna1 {n - 2})}}) ))


```
{% endhighlight %}

<h2><br>
    </h2>
    <h2><u>9.Short documentation</u></h2>
    <p>Square bracket operator <b>[] </b>is used for vectors,arrays,growable
      vectors,hash tables,etc.</p>
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
      <li><b><b>{</b>T</b><b>[m n] &lt;- </b><b><b>T</b><b>[m {n +
    1}]</b>}</b></li>
	<li><b>{my-hash-tbl["three"] &lt;- 3}</b></li>
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
    <br>
    <p><u>How to load a Scheme+ program:</u></p>
    <p>Scheme+ programs are loaded the usual way,example:</p>
    <pre>(load "start-λογικι-guile+.scm")<br><br>(infix-symb-min-dnf '{ {(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)} } )
disj-norm-form = (or (and c (not d)) (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) (not c) (not d)) (and a (not b) (not c) d))
<br>((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))</pre>
    <p>We can use a convention to name the Scheme+ programs and function with a
      + at end, keeping the .scm extension unchanged for compatibility.<br>
    </p>
    <p><br>
    </p>
    <h2>10<u>.Advanced examples:</u><br>
    </h2>
    <p>Here is another example, from the <a href="https://en.wikipedia.org/wiki/Subset_sum_problem#Pseudo-polynomial_time_dynamic_programming_solution"
        target="_blank">Subset Sum Problem</a> that show the use of <b>&lt;+</b>
      and <b>&lt;-</b> (due to the impossibility to have easily a single
      operator :-( ) :</p>

{% highlight scheme %}
```scheme


(use-modules (Scheme+))

{L-init <+ '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init <+ 35267}
{ls <+ (length L-init)}
{dyna <+ (make-array 0 {ls + 1} {t-init + 1})}

{cpt <+ 0} ;; define cpt to 0

(define (subset-sum-guile L t)

  {ls <+ (length L)}
  {dyn <+ dyna[ls t]}

  {cpt <- {cpt + 1}} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <+ (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <+ (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <+ (subset-sum-guile R t)}
	          {dyna[ls t] <- (one-two s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <+ {(subset-sum-guile R {t - c}) or (subset-sum-guile R t)}}
	       {dyna[ls t] <- (one-two s)}
	       s])) ;; return boolean value


```
{% endhighlight %}

<p> </p>
    <br>
    <p><b>scheme@(guile-user)&gt; (subset-sum-guile&nbsp; L-init t-init)</b></p>
    <p><b>#t</b><br>
      <br>
    </p>
    <p style="text-align: center;">note: in the example above condx is a macro
      that allow execution of arbitrary code between conditionals
      clauses,general syntax is:</p>


{% highlight scheme %}
```scheme


(condx [condition statements ...]

            [exec statements ... ]

            ...

            [else statements ...])


```
{% endhighlight %}

<div style="margin-left: 40px;"> </div>
    <p style="text-align: center;"><br>
    </p>
    <p style="text-align: left;">In classic Scheme the code would be like that
      which is longer than in Scheme+:</p>
	  
{% highlight scheme %}
```scheme


(define cpt 0)

(define (ssigma-proto L t)

  (set! cpt {cpt + 1})
 
  (define ls (length L))
  (define dyn (array-ref dyna ls t))
    
  ;; dyna[ls][t] means 0: unknown solution, 1: solution found, 2: no solution

  (cond [(not (zero? dyn)) (one? dyn)]
	[(null? L) (array-set! dyna 2 ls t) #f] ;; return #f
	
	[else (let [(c (first L))]
		
		(if {c = t} ;; c is the solution
		      
		    (begin
		      (array-set! dyna 1 ls t)
		      #t)  ;; return #t

		    ;; else
		    (let [(R (rest L))]
		      
		      (if {c > t}   ;; continue searching a solution in the rest
			    
			  (let [(s (ssigma-proto R t))]
			    (array-set! dyna
					(one-two s)
					ls t)
			      
			    s) ;; return s
			
			  ;; else
			  ;; c < t at this point
			  ;; c is part of the solution or his approximation
			  ;; or c is not part of solution
			  (let [(s {(ssigma-proto R {t - c}) or (ssigma-proto R t)})]
			    (array-set! dyna (one-two s)
					ls t)
			    s)))))
	      ] ))


```
{% endhighlight %}



<p style="text-align: left;"> <br>
    </p>
    <p style="text-align: left;"><br>
    </p>
    <p style="text-align: left;">If you want to use a single assignment operator
      <b>&lt;-</b> it is possible using a declarative programming style with the
      <b>declare </b>macro:</p>
    <p> </p>
	
{% highlight scheme %}
```scheme


(use-modules (Scheme+))

(declare L-init t-init ls dyna cpt)

{L-init <- '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init <- 35267}
{ls <- (length L-init)}
{dyna <- (make-array 0 {ls + 1} {t-init + 1})}

(define (one-two b)
  (if b 1 2))

{cpt <- 0}

(define (subset-sum-guile-dec L t)

  (declare ls dyn c R s)
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  {cpt <- {cpt + 1}} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-guile-dec R t)}
	          {dyna[ls t] <- (one-two s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <- {(subset-sum-guile-dec R {t - c}) or (subset-sum-guile-dec R t)}}
	       {dyna[ls t] <- (one-two s)}
	       s])) ;; return boolean value


```
{% endhighlight %}


<p style="text-align: left;">It is also possible to use <b>return</b>
      keyword in function definition by <b>def </b>macro and have again
      another programming style with <b>if then else</b> that looks like
      traditionals language such as Python,Javascript,C/C++,etc... :</p>
    <p style="text-align: left;">This example mix many Scheme+ Style and
      illustrate the use of <b>return </b>in a Python/C++ style:</p>
    <p style="text-align: left;"><br>
    </p>
	

{% highlight scheme %}
```scheme


(use-modules (Scheme+))

(def (subset-sum-dyna L t)

  (declare ls dyn) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  (if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)
      (return (one? dyn)))

  (when (null? L)
    {dyna[ls t] <- 2}
    (return #f))

  {c <+ (first L)}

  (when {c = t}  ;; c is the solution
    {dyna[ls t] <- 1}
    (return #t))

  {R <+ (rest L)} ;; continue searching a solution in the rest

  (declare s)
  (if {c > t}  ;; c is to big to be a solution
    {s <- (subset-sum-dyna R t)}
    ;; c is part of the solution or c is not part of solution
    {s <- {(subset-sum-dyna R {t - c}) or (subset-sum-dyna R t)}})

  {dyna[ls t] <- (one-two s)}
  s) ;; return boolean value


```
{% endhighlight %}

    
<h2><br>
    </h2>
    <h2><br>
    </h2>
    <h2>11<u>. History of project:</u></h2>
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
    <h2>12.<u>Additional documentation:</u></h2>
    <p><b>condx</b> is a macro that allow execution of arbitrary code between
      conditionals clauses,syntax is :</p>

{% highlight scheme %}
```scheme


(condx [condition statements ...]

            [exec statements ... ]

            ...

            [else statements ...])


```
{% endhighlight %}

<p style="text-align: left;"><b>condx</b> is not a major feature of <b>Scheme+</b>
      but it can replace a lot of 'if then <b>elif'</b>, i dislike and still
      misunderstand sometimes, '<b>else if</b>' since BASIC !<br>
    </p>
    <p style="text-align: left;">&nbsp;<br>
    </p>
    <p style="text-align: left;">defining new functions with <b>def
      </b>instead of the classic define we can now use <b>return</b> to escape immediately
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

{% highlight scheme %}
```scheme


(define foo (begin (define x 1)
                   (define y 2)
                   (lambda (t) (+ x y t))))

(define bar (let ()
              (define x 1)
              (define y 2)
              (lambda (t) (+ x y t))))


```
{% endhighlight %}

<p style="text-align: left;">error: define: not allowed in an expression
      context in: (define x 1)<br>
    </p>
    <p style="text-align: left;">for this reason i provide a <b>&amp; </b>macro
      that is defined with a <b>let </b>as this:<br>
    </p>
    <p style="text-align: left;"> </p>

{% highlight scheme %}
```scheme


(define-syntax &
  (syntax-rules ()
    ((_ ev)  ev)
    ((_ ev ...) (let () ev ...))))


```
{% endhighlight %}

 
<p>We can now define the previous bar function in Scheme+ like this:<br>
    </p>
    <p> </p>


{% highlight scheme %}
```scheme


(def bar (& (declare x y)
            {x <- 1} 
            {y <- 2}
            (lambda (t) (+ x y t))))

(bar 7)
10


```
{% endhighlight %}


<p style="text-align: left;"> </p>
    <h2 style=" text-align: left;">13.<u>Caveats:</u></h2>
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
    <h2>14.<u>Implementation:</u></h2>
    <p>Mainly with <b>Scheme macros</b> which are not recursive (except in some
      obsolete features),so expansion is fast and code also. It also allows a
      great portabilty from one Scheme implementation to another one.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2>15.<u>Future:</u></h2>
    <p>Scheme+ will be implemented for other Scheme. Racket (former DrScheme)
      first,....</p>
    <p>Next version will integrate more features, concerning <b>multiple values</b>
      return for example.</p>
    <p><br>
    </p>
    <p><br>
    </p>
    <h2><u>16.Extra features:</u> </h2>
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
    <h2>17.<u>Obsolete features:</u></h2>
    <p>As mentioned early the project idea was initially to enhance the set of
      LET special form. The use of the LET set is no more need with the new
      assignment operator but i release anyway those set of enhanced and
      simplified LETs, i just give some examples as names are self-explanatory.</p>
    <p>The way to be able to use the new set of LET special form is to load the
      file let.scm which is available in the Scheme+ directory:</p>
    <p><b>(load "let.scm")</b></p>
    <p>Examples:</p>
    <p><!-- HTML generated using hilite.me --></p>

{% highlight scheme %}
```scheme


(let<-rec* [x <- 1
            y <- (+ x 1)
            z <- (+ 2 y)]
         z)
4


```
{% endhighlight %}

<p> </p>
    <p>and here is the source code of this <b>recursive macro</b> defined with
      an <b>accumulator</b>:</p>
    <p><!-- HTML generated using hilite.me --></p>


{% highlight scheme %}
```scheme


(define-syntax let<-rec*

  (syntax-rules (<-)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    ((_ (var1 <- val1) expr  ...) (letrec* ((var1 val1)) expr ...)) ;; case single binding
    ((_ (var1 <- val1 ;; multiple binding
	 var2 <- val2 ...)
	expr ...)
     (%parse-letrec-bindings-and-evaluate-expressions ((var1 val1))  (var2 <- val2 ...) expr ...))))

;; recursive macro with accumulator
(define-syntax %parse-letrec-bindings-and-evaluate-expressions

  (syntax-rules (<-)

    ((_ (bindings ...) (var1 <- val1) expr ...)  (letrec* (bindings ... (var1 val1)) ;; last binding
							  expr ...)) ;; we evaluate expressions
    ((_ (bindings ...) (var1 <- val1 ;; multiple binding
			var2 <- val2 ...) expr ...)
     ;; store (var1 val1) binding in accumulator and continue parsing
       (%parse-letrec-bindings-and-evaluate-expressions (bindings ... (var1 val1)) (var2 <- val2 ...) expr ...))))


```
{% endhighlight %}

<p> </p>
    <br>
    <p>a few others special forms simplifying the LETs (but still obsolete in my
      opinion):</p>
    <p>let-arrow* :</p>
    <p><!-- HTML generated using hilite.me --></p>

{% highlight scheme %}
```scheme


(let-arrow* (x ← 1
 	     y ← {x + 1})
           y)
2


(let-arrow* ({x ← 1}
             {y ← {x + 1}})
           x
           y)

2


(let-arrow* [ x 1
              y (+ x 1)
              z (+ 2 y) ]
            z y)

2

```
{% endhighlight %}

<p> </p>
    <p><br>
    </p>
    <p>letrec-arrow* , the same as let-arrow* but for recursive functions:</p>

{% highlight scheme %}
```scheme


(letrec-arrow* [ fact ← (lambda (n)
 			  (if  {n = 1}
 			       1
                               {n * (fact {n - 1})} )) ]
 	       (fact 5))

120

```
{% endhighlight %}

<br>
    <h2><br>
    </h2>
    <h2><u>18.Comments:</u></h2>
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
   <p><br>
    </p>
    <h2>19. <u>Special characters (for special people):</u></h2>
    <p>This is about the optional replacement symbols for some operators:</p>
    <br>
    <p> <img src="https://upload.wikimedia.org/wikipedia/commons/9/9f/APL-keybd2.svg"
        alt="IBM 2741 keyboard layout" title="IBM 2741 keyboard layout">IBM 2741
      Terminal keyboard layout</p>
    <br>
    <p>The hexadécimal unicode can be entered under Linux with : Ctrl-Shift u
      following with the hexadecimal unicode and Enter or Space bar,<meta http-equiv="content-type" content="text/html; charset=UTF-8">
on <a
href="https://apple.stackexchange.com/questions/183045/how-can-i-type-unicode-characters-without-using-the-mouse"
moz-do-not-send="true">Mac OS use </a> <a
href="https://apple.stackexchange.com/questions/183045/how-can-i-type-unicode-characters-without-using-the-mouse"
moz-do-not-send="true">CTRL+CMD+space to bring up the characters
popover, </a> <a
href="https://apple.stackexchange.com/questions/183045/how-can-i-type-unicode-characters-without-using-the-mouse"
moz-do-not-send="true">then type in u + unicode and hit Enter to
get it</a>.</p>
    <ul>
      <li>&lt;- : ← , unicode 2190 (this doesn't looks like but it is an
        hexadecimal number !) This operator assignment was used in <a href="https://en.wikipedia.org/wiki/APL_%28programming_language%29"
          target="_blank">APL</a> </li>
      <li>-&gt; : → ,unicode 2192</li>
      <li>&lt;+ : ⥆ , unicode 2946 (Add a variable in the current environment)</li>
      <li>+&gt; : ⥅ , unicode 2945</li>
      <li>&lt;&gt; : ≠ , unicode 2260</li>
    </ul>
    <p>Example:</p>
    <pre>scheme@(guile-user)&gt; {v ⥆ 7}
scheme@(guile-user)&gt; v
7
</pre>
<div style="text-align: center;"><img src="https://upload.wikimedia.org/wikipedia/commons/2/2c/IBM2741.JPG"
        alt="IBM 2741" title="IBM 2741" width="450"></div>
    <div style="text-align: center;">IBM 2741 Terminal </div>
    <p> </p>
