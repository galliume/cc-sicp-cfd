#lang racket

(require racket/trace)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (next k)
  (+ k 1))

(define (square a)
  (* a a))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(accumulate * 1 square 1 next 3)
(accumulate + 0 square 1 next 3)