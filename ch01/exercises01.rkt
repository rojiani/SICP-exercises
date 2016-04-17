#lang racket
; 1.1
10
(+ 5 3 4)               ; 12
(- 9 1)                 ; 8
(/ 6 2)                 ; 3
(+ (* 2 4) (- 4 6))     ; 6
(define a 3)            ; a = 3
(define b (+ a 1))      ; b = 4
(+ a b (* a b))         ; (+ 3 4 (* 3 4)) = (+ 3 4 (12)) = 19
(= a b)                 ; #f
(if (and (> b a) (< b (* a b)))
    b
    a)                  ; 4
(cond ((= a 4) 6)       ; #f
    ((= b 4) (+ 6 7 a)) ; #t (+ 6 7 3)
    (else 25))          ; 16
(+ 2 (if (< b a) b a))  ; (+ 2 a) = 5
(* (cond ((> a b) a)
        ((< a b) b)
        (else (- 1)))
    (+ a 1))
; (* (b) (4)) = 16

; 1.2
; (5 + 4 + (2 - (3 - (6 + (4/5))))) / ((3)(6-2)(2-7))
(/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
    (* 3 (- 6 2) (- 2 7)))
; = -37/150 = -0.2466

; 1.3
; as "one procedure"
(define (sos-larger-bad x y z)
  (cond ((and (> x z) (> y z))
          (+ (* x x) (* y y)))
        ((and (> x y) (> z y))
          (+ (* x x) (* z z)))
        ((and (> y x) (> z x))
          (+ (* y y) (* z z)))))

(sos-larger-bad 2 3 4) ; 25

; better way
(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (sos-larger x y z)
  (cond ((and (> x z) (> y z))
          (sum-of-squares x y))
        ((and (> x y) (> z y))
          (sum-of-squares x z))
        ((and (> y x) (> z x))
          (sum-of-squares y z))))

(sos-larger 1 2 3) ; 13

(define (min a b) (if (< a b) a b))
(define (max a b) (if (> a b) a b))

(min 7 9)
(min 9 7)
(min 9 9)
(max 7 9)
(max 9 7)
(max 9 9)

(define (sos-larger-2 x y z)
  (sum-of-squares
      (max x y)
      (max (min x y) z)))

(sos-larger-2 1 2 3) ; 13
(sos-larger-2 3 5 4) ; 41
(sos-larger-2 (- 5) 4 8) ; 80

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; b > 0: (+ a b) = a + b
; b <= 0: (- a b) = a - b
; operator can be result of an evaluation

(a-plus-abs-b 7 4)
(a-plus-abs-b 7 (- 4))

; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

;(test 0 (p))

; applicative:
; all parameters evaluated, p recursively calls itself
; normal order:
; parameters not evaluated until needed, and since x = 0, the recursive call
; is never evaluated

; 1.6
(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
    (else else-clause)))

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)

(define (improve guess x)
  (* 0.5 (+ (/ x guess) guess)))

(define (good-enough guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (square-root x)
  (sqrt-iter 1.0 x))

; Will infinitely recurse, because the new-if doesn't have the special behavior
; that only if the predicate is true, only one of the then/else clauses will
; be evaluated (applicative evaluation). The else clause is recursively called
; with same values (never halting execution).
