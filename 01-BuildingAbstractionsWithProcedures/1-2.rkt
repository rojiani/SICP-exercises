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


; 1.2.2 Tree Recursion
(newline)(display "fibonacci - recursive")(newline)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(fib 0)   ; 0
(fib 1)   ; 1
(fib 2)   ; 1
(fib 3)   ; 2
(fib 4)   ; 3
(fib 5)   ; 5
(fib 6)   ; 8
(fib 7)   ; 13
(fib 8)   ; 21
(fib 9)   ; 34
(fib 10)  ; 55

(newline)(display "fibonacci - iterative")(newline)
; iterative process fibonacci
; a: initially 1 => Fib(1) = 1
; b: initially 2 => Fib(0) = 0
; each iteration:
;   a <-- a + b
;   b <-- a
; after n transformations, a = Fib(n+1), b = Fib(n)
(define (fib-iter n)
  (fibonacci-iter 1 0 n))

(define (fibonacci-iter a b count)
  (if (= count 0)
      b
      (fibonacci-iter (+ a b) a (- count 1))))

;(fib-iter 0)   ; 0
(fib-iter 1)   ; 1
(fib-iter 2)   ; 1
(fib-iter 3)   ; 2
(fib-iter 4)   ; 3
(fib-iter 5)   ; 5
(fib-iter 6)   ; 8
(fib-iter 7)   ; 13
(fib-iter 8)   ; 21
(fib-iter 9)   ; 34
(fib-iter 10)  ; 55


; Example: Counting change
; # of ways to make change of $1.00 (or some other amount)
; coins: 0.50, 0.25, 0.10, 0.05, 0.01
;
; a = amount to make change from
; n = number of kinds of coins
; d = the denomination of the 1st kind of coin
; x = number of ways to make change of a
; Relation:
;    = [# of ways to change a using all but the first kind of coin] +
;       [the # of ways to change amount (a-d) using all n kinds of coins]
; Base cases:
;   1. if a == 0. we count that as 1 way to make change
;   2. if a < 0, we should count that as 0 ways to make change
;   3. if n is 0, we should count that as 0 ways to make change

(define (count-change amount)
  (cc amount 5))

; first-denomination:
;   input: # of kinds of coins available
;   returns: the denomination of the first kind
;   'first' is the largest coin.
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)          ; base case 1
    ((or (< amount 0)             ; base cases 2,3
         (= kinds-of-coins 0))
      0)
      (else
        (+ (cc amount (- kinds-of-coins 1)) ; without first coin
           ; + with first coin
           (cc (- amount (first-denomination
                          kinds-of-coins))
               kinds-of-coins)))))


(newline)(display "ways to count change")(newline)
(count-change 10)
(count-change 100)
