;; run-test.scm

;; Guile Scheme version

;; Runs one converted test file of Scheme+ (see run-tests.sh) under a runner of SRFI 64 , prints each failed check
;; with the expected and the actual values , and exits with the number of failures as status (0 : all passed).
;; usage : guile -L <Scheme+ for Guile> run-test.scm test-name converted-file.scm

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

(use-modules (srfi srfi-64) (system base compile))

(define name (cadr (command-line)))
(define file (caddr (command-line)))

(define (report-failure runner)
  (when (memq (test-result-kind runner) '(fail))
    (let ((r (test-result-alist runner)))
      (format #t "FAIL ~a:~a : ~s\n     expected : ~s\n     actual   : ~s\n"
	      (assq-ref r 'source-file) (assq-ref r 'source-line) (assq-ref r 'source-form)
	      (assq-ref r 'expected-value) (assq-ref r 'actual-value)))))

(define runner (test-runner-null))
(test-runner-on-test-end! runner report-failure)
(test-runner-current runner)

(test-begin name)
(save-module-excursion (lambda () (load-compiled (compile-file file #:output-file (string-append file ".go")))))
(define passes (test-runner-pass-count runner))
(define fails (test-runner-fail-count runner))
(test-end name)
(format #t "~a : ~a checks passed , ~a failed\n" name passes fails)
(exit (min fails 255))
