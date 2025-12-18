#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)

  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))

  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))

(define (cont-frac n d k)
  (define (iter k result)
    (cond ((= 0 k) result)
          (else (iter (- k 1) ( / (n k) (+ (d k) result))))))
    (iter k 0))

(define (euler i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1))

(cont-frac (lambda (i) 1.0)
           euler
           11)