;; run-sweet-testsuite.scm : the test suite of the sweet-expression reader (SRFI 110) of the Readable project
;; (sweet-testsuite.txt : pairs of the expected S-expression , read by the read of Guile , and the sweet-expression , read
;; by the reader (SRFI-110 kernel)) , in the strict SRFI 105 mode (as the original reader) , then in the mode of Scheme+
;; (the differences come only from the curly brackets).
;; Translated from run-sweet-testsuite.rkt of SRFI 110 for Racket.
;; usage , in the directory tests/ :   guile -L .. run-sweet-testsuite.scm

(use-modules (SRFI-110 kernel)
	     (ice-9 rdelim)
	     ((Scheme+ parser-declarations) #:select (reset-declarations-for-parser!)))

;; the expected results are written in R7RS : |-v| is the symbol -v (by default Guile reads the vertical bars as
;; characters of the symbol)
(read-enable 'r7rs-symbols)

(define testsuite-file
  (string-append (dirname (car (command-line))) "/sweet-testsuite.txt")) ; next to this script

(define (skip-to-empty-line port)
  (let loop ()
    (let ((l (read-line port)))
      (unless (or (eof-object? l)
		  (string=? l ""))
	(loop)))))

;; the text of the port between the positions start and end
(define (text-between port start end)
  (let ((here (ftell port)))
    (seek port start SEEK_SET)
    (let ((s (let loop ((n (- end start)) (acc '()))
	       (if (<= n 0)
		   (list->string (reverse acc))
		   (let ((c (read-char port)))
		     (if (eof-object? c)
			 (list->string (reverse acc))
			 (loop (- n (char-utf8-length c)) (cons c acc))))))))
      (seek port here SEEK_SET)
      s)))

(define (char-utf8-length c)
  (let ((n (char->integer c)))
    (cond ((< n #x80) 1) ((< n #x800) 2) ((< n #x10000) 3) (else 4))))

(define (run-testsuite title)
  (define port (open-input-file testsuite-file))
  (define n 0) ; tests
  (define ok 0)
  (define bad 0)
  (define unreadable 0) ; expected result not readable by read
  (reset-declarations-for-parser!) ; the procedures defined by the previous run are forgotten
  (display title) (newline)
  (let loop ()
    (let ((expected (catch #t (lambda () (read port)) (lambda args 'unreadable))))
      (cond ((eof-object? expected)
	     (format #t "~a : ~a tests , ~a passed , ~a failed , ~a expected results not readable by read\n\n"
		     title n ok bad unreadable))
	    ((eq? expected 'unreadable)
	     (set! unreadable (+ unreadable 1))
	     (skip-to-empty-line port)
	     (loop))
	    (else
	     (let* ((start (ftell port))
		    (got (catch #t
			   (lambda () (sweet-read port))
			   (lambda (key . args) (list 'sweet-read-error key args))))
		    (end (ftell port)))
	       (set! n (+ n 1))
	       (cond ((equal? expected got)
		      (set! ok (+ ok 1)))
		     (else
		      (set! bad (+ bad 1))
		      (format #t "FAILED test ~a\n  sweet-expression : ~s\n  expected : ~s\n  got      : ~s\n"
			      n (text-between port start end) expected got)))
	       (when (and (pair? got)
			  (eq? (car got) 'sweet-read-error))
		 (skip-to-empty-line port))
	       (loop))))))
  (close-input-port port)
  bad)

(sweet-read (open-input-string "BEGIN-STRICT-SRFI-105-REGION\nx\n"))
(define bad-strict (run-testsuite "Strict SRFI 105 mode (as the original reader)"))

(sweet-read (open-input-string "END-STRICT-SRFI-105-REGION\nx\n"))
(run-testsuite "Mode of Scheme+ (differences expected only for the curly brackets)")

(exit (if (= bad-strict 0) 0 1))
