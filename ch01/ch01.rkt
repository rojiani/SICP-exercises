#lang racket

(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(sum-of-squares 4 7)

(define (f a)
    (sum-of-squares (+ a 1) (* a 2)))
    ; (a + 1)^2 + (2a)^2

(f 3) ; (3+1)^2 + (2*3)^2 = (16) + (36) = 52

(define (absolute-value x)
    (cond ((< x 0) (- x))
        ((= x 0) 0)
        ((> x 0) x)))

(absolute-value (- 20))
(absolute-value  0)
(absolute-value 20)

(define (abs-2 x)
    (cond ((< x 0) (- x))
        (else x)))

(abs-2 (- 20))
(abs-2  0)
(abs-2 20)

; if can be used if there are 2 cases
; acts like the ternary operator:
; (if <predicate> <consequent> <alternative>)
(define (abs-3 x)
    (if (< x 0)
        (- x)
        x))

(abs-3 (- 30))
(abs-3  0)
(abs-3 30)

(define (>= x y)
    (or (> x y) (= x y)))

(>= 9 9)
(>= 9 5)
(>= 6 9)

; 1.1.7 Square Roots by Newton's Method
; sqrt of N = 1/2 * ( N/G + G )
; iteratively call until number basically not changing

(define (improve guess x)
	(* 0.5 (+ (/ x guess) guess)))

(improve 15 169)

(define (good-enough guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(sqrt-iter 15 169)

(define (square-root x)
  (sqrt-iter 1.0 x))

; (square-root 9999999999998)       ; 3162277.660168063 (never returns)
(square-root 1000000)
(square-root 169)
(square-root 144)
(square-root 100)
(square-root 64)
(square-root 60) ; 7.746
(square-root 9)
(square-root 2)
(square-root 1)
(square-root (/ 1 9))
(square-root 0.1)
(square-root 0.01)
(square-root 0.001)
(square-root 0.0001)
(square-root 0.00001)
(square-root 0.000001)
(square-root 0.0000001)
(square-root 0.00000000001)
