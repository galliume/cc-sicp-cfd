#lang racket

(require racket/trace)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube a)
  (* a a a))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (even? n)
  (= (remainder n 2) 0))

(define (simpson f a b n)
  (define h (/ (- b a ) n))

  (define (y k)
    (f (+ a (* k h))))
  
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
           ((even? k) 2)
           (else 4))
    (y k)))

  (define (next k)
    (+ k 1))
  
  (* (/ h 3.0) (sum term 0 next n)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(simpson cube 0 1 1000)
(simpson cube 0 1 10000)