#lang racket

(require racket/trace)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (divides? a b)
(= (remainder b a ) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

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

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result)
            )
        )
    )
  (iter a null-value))

(filtered-accumulate + 0 square 2 next 11 prime?)

(define N 10)
(filtered-accumulate * 1
                     (lambda(x) x)
                     2
                     next
                     (- N 1)
                     (lambda(x) (= (gcd x N) 1)))