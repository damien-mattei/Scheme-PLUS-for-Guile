;; condx: cond(itionals) with optional execution of statements before
					;

; This file is part of Scheme+

;; Copyright 2021 Damien MATTEI

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


; example:
;(define x 1)
;(condx ((= x 7) 'never)
;        (exec
;          (define y 3)
;          (set! x 7))
;        ((= y 1) 'definitely_not)
;        (exec
;          (set! y 10)
;          (define z 2))
;        ((= x 7) (+ x y z))
;        (else 'you_should_not_be_here))
;
; 19

(define-syntax condx
  (syntax-rules (exec else)
    ((_)
     (error 'condx "No else clause"))
    ((_ (else e ...))
     (let () e ...))
    ((_ (exec s ...) d1 ...)
     (let () s ... (condx d1 ...)))
    ((_ (t e ...) tail ...)
     (if t
         (let () e ...)
         (condx tail ...)))))

;; warning this ones behaves differently (can not remember the problem)
(define-syntax condx-begin
  (syntax-rules (exec else)
    ((_)
     (error 'condx-begin "No else clause"))
    ((_ (else e ...))
     (begin e ...))
    ((_ (exec s ...) d1 ...)
     (begin s ... (condx-begin d1 ...)))
    ((_ (t e ...) tail ...)
     (if t
         (begin e ...)
         (condx-begin tail ...)))))

;; (define x 1)
;; (condx ((= x 7) 'never)
;;         (exec
;;           (define y 3)
;;           (set! x 7))
;;         ((= y 1) 'definitely_not)
;;         (exec
;;           (set! y 10)
;;           (define z 2))
;;         ((= x 7) (+ x y z))
;;         (else 'you_should_not_be_here))

;; (define y 0)
;; (define z 0)
;; (set! x 1)
;; (condx-begin ((= x 7) 'never)
;;         (exec
;;           (set! y 3)
;;           (set! x 7))
;;         ((= y 1) 'definitely_not)
;;         (exec
;;           (set! y 10)
;;           (set! z 2))
;;         ((= x 7) (+ x y z))
;;         (else 'you_should_not_be_here))

