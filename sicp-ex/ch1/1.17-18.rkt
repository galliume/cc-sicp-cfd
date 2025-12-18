#lang racket

(require racket/trace)

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(trace *)
(* 1 3)

(define (double a)
  (* a 2))

(define (halve a)
  (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (multi-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (multi-iter (double b) (halve n) a))
        (else (multi-iter b (- n 1) (+ a b)))))

(define (multi a b)
  (multi-iter a b 0))

(trace multi)
(multi 1 3)