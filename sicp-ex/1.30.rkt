#lang racket

(require racket/trace)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube a)
  (* a a a))

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

(simpson cube 0 1 100)
(trace sum)
(simpson cube 0 1 1000)