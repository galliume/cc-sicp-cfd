#lang racket

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

(define dx 0.00001)

(define (compose a b)
  (lambda (x)
    (a (b x))))

(define (repeated func n)
  (define (iter result n)
    (if (= 1 n)
          result
          (iter (compose func result) (- n 1))))
  (iter func n))

(define (average-damp f)
  (lambda (x) (average x (f x))))

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

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
    dx)))

(define (nth-root x) (fixed-point (lambda (y) (/ x y) repeated average-damp 5) 1.0))

(nth-root 10)