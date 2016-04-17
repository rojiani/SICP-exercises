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
