#lang racket

(define (square x)
  (* x x))

(define (compose a b)
  (lambda (x)
    (a (b x))))

(define (repeated func n)
  (define (iter result n)
    (if (= 1 n)
          result
          (iter (compose func result) (- n 1))))
  (iter func n))

((repeated square 2) 5)