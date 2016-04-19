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



;(define (sqrt-iter guess x)
;  (new-if (good-enough guess x)
;    guess
;    (sqrt-iter (improve guess x) x)))

; (define (improve guess x)
;   (* 0.5 (+ (/ x guess) guess)))

; (define (good-enough guess x)
;   (< (abs (- (square guess) x)) 0.001))

;(define (square-root x)
;  (sqrt-iter 1.0 x))


; Will infinitely recurse, because the new-if doesn't have the special behavior
; that only if the predicate is true, only one of the then/else clauses will
; be evaluated (applicative evaluation). The else clause is recursively called
; with same values (never halting execution).

; 1.7
(define (under-cutoff guess change)
  (< (abs change) (/ guess 10000000)))

(define (improved-guess guess radicand)
   (* 0.5 (+ (/ radicand guess) guess)))

(define (sqrt-iter guess radicand change)
  (if (under-cutoff guess change)
       guess
       (sqrt-iter (improved-guess guess radicand)
                  radicand
                  (- guess (improved-guess guess radicand)))))

(define (square-root n)
  (sqrt-iter 1.0 n 1.0))

; 1.7 tests
(square-root 9999999999998)       ; 3162277.660168063
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
(newline)
(newline)
(newline)


; 1.8: Newton's method for cube roots
; cube root of x approx.: (x/y^3 + 2y)/3 where y is the guess

(define (under-cutoff-2 guess change) ; handle negatives
  (< (abs change) (/ (abs guess) 10000000)))

(define (improved-cube-guess x y)
   (* (/ 1 3)
      (+ (/ x (square y))
         (* 2 y))))

(define (cubic-root-iter x guess change)
  (if (under-cutoff-2 guess change)
      guess
      (cubic-root-iter x
                       (improved-cube-guess x guess)
                       (- guess (improved-cube-guess x guess)))))

(define (cubic-root x)
  (cubic-root-iter x 1.0 1.0))

(cubic-root 8)
(cubic-root (- 8))
(cubic-root 125)
(cubic-root (- 125))
(cubic-root 997002999)          ; 999
(cubic-root 1000030000300001)   ; 100001
(cubic-root 0.125)              ; 0.5
(cubic-root 0.015625)           ; 0.25
(cubic-root (/ 1 50653))        ; 1/37 = 0.02702702702702703
(cubic-root (- (/ 1 50653)))    ; -(1/37) = -0.02702702702702703
