#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (compose a b)
  (lambda (x)
    (a (b x))))

(define (repeated func n)
  (define (iter result n)
    (if (= 1 n)
          result
          (iter (compose func result) (- n 1))))
  (iter func n))

(define (func x)
  (+ x x))

(define (smooth f )
  (let ((dx 0.0001))
  (lambda (x)
    (/ ( + (f (- x dx))
           (f x)
           (f (+ x dx)))
       3))))

(((repeated smooth 5) func) 10)