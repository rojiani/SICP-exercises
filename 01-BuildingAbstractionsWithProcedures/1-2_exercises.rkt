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


; 1.10: Ackermann's function
(define (A x y)
  (cond ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1)
          (A x (- y 1))))))

(A 1 10)  ; 1024
(A 2 4)   ; 65536
(A 3 3)   ; 65536

(define (f n) (A 0 n))
(A 0  8)
(f 8)

; f(n) = 2y

(define (g n) (A 1 n))
(g 5)

; g(n) = / 0      if n = 0
;        \ 2^n    if n > 0

(define (h n) (A 2 n))
(h 0)
(h 1)
(h 2)
; h(n) = / 0            if n = 0
;        \ 2^(h(n-1))   if n > 0
