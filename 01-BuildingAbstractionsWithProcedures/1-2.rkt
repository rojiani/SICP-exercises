#lang racket
; 1.2: Procedures and the Processes They Generate
; 1.2.1: Linear Recursion & Iteration

(newline)
(display "linear recursive")
(newline)

; linear: O(n)
; linear recursive
; n! = n * [(n - 1) * (n - 2) * ... 3 * 2 * 1] = n * (n - 1)!
(define (fact-r n)
  (if (= n 1)
      1
      (* n (fact-r (- n 1)))))

(fact-r 1)
(fact-r 2)
(fact-r 3)
(fact-r 4)
(fact-r 5)
(fact-r 6)   ; 720
(fact-r 10)  ; 3628800
(newline)
(newline)
; We maintain a running product, & a counter that counts from 1 up to n
; The counter & the product simultaneously change from one step to the next
; according to the rule:
;       product <- counter * product
;       counter <- counter + 1
; where n! is the value of the product when the counter exceeds n

(newline)
(display "(linear) iterative")
(newline)

; also a 'recursive *procedure*', but is an 'iterative *process*'
(define (fact-i n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
          product
          (fact-iter (* product counter) (+ counter 1) max-count)))

; equivalent, but not as nice
;(define (fact-iter product counter max-count)
;  (if (= counter max-count)
;          (* product counter)
;          (fact-iter (* product counter) (+ counter 1) max-count)))

(fact-i 1)
(fact-i 2)
(fact-i 3)
(fact-i 4)
(fact-i 5)
(fact-i 6)   ; 720
(fact-i 10)  ; 3628800
(newline)
(newline)

(newline)
(display "iterative 2")
(newline)


; another equivalent - max-count parameter not necessary, but helpful
; for comparing iter to recursive.
(define (fact-i-2 n)
  (fact-iter-descend 1 n))

(define (fact-iter-descend product counter)
  (if (= counter 1)
          product
          (fact-iter-descend (* product counter) (- counter 1))))

; converting iterative version to recursive version:
; mainly just passing local state variables as parameters

(fact-i-2 1)
(fact-i-2 2)
(fact-i-2 3)
(fact-i-2 4)
(fact-i-2 5)
(fact-i-2 6)   ; 720
(fact-i-2 10)  ; 3628800
