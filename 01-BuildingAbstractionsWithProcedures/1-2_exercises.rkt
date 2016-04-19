#lang racket
; 1.9

; Substitution Model
; Evaluate the operator to get procedure
; Evaluate the operands to get arguments
; Apply the procedure to the arguments
; Copy the body of the procedure substituting the arguments supplied for the
;   formal parameters of the procedure.
; Evaluate the resulting new body.

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (plus-1 a b)
  (if (= a 0)
      b
      (inc (plus-1 (dec a) b))))

; (plus-1 4 5):
; (inc (plus-1 3 5))
;; (inc (inc (plus-1 2 5)))
;;; (inc (inc (inc (plus-1 1 5))))
;;;; (inc (inc (inc (inc (plus-1 0 5)))))
;;;;; (inc (inc (inc (inc 5))))
;;;; (inc (inc (inc 6)))
;;; (inc (inc 7))
;; (inc 8)
; 9
; Recursive
(plus-1 4 5)


(define (plus-2 a b)
  (if (= a 0)
      b
      (plus-2 (dec a) (inc b))))

; (plus-2 4 5):
;; (plus-2 3 6)
;;; (plus-2 2 7)
;;;; (plus-2 1 8)
;;;;; (plus-2 0 9)
;;;;;; 9
; Iterative

(plus-2 4 5)
