#lang racket

(require racket/trace)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (square a)
  (* a a))

(define (even? n)
  (= (remainder n 2) 0))

(define (next k)
  (+ k 1))

(define (factorial n)
  (product (lambda (x) x)  1 next n))

(define (wallis n)
  (define (term n)
    (/
     (* 4.0 n n)
     (*
      (- (* 2.0 n) 1.0)
      (+ (* 2.0 n) 1.0))))
  
  (* 2.0 (product term 1.0 next n)))

(product square 2 next 3)

(trace factorial)
(factorial 5)
(factorial 10)

(wallis 100.0)
(wallis 10000.0)
(wallis 10000000.0)