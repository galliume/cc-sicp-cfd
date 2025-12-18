#lang racket

(require racket/trace)
;(current-milliseconds) replace (runtime)

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (divides? a b)
(= (remainder b a ) 0))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;(trace prime?)
(fast-prime? 199 10)
(fast-prime? 1999 12)
(fast-prime? 19999 2)

;(smallest-divisor 199);prime
;(smallest-divisor 1999);prime
;(smallest-divisor 19999);not prime

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime n (- (current-milliseconds) start-time))
      #f))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

;(trace timed-prime-test)
;(timed-prime-test 199)

(define (search-for-primes begin n)
  (cond ((= n 0) (newline) (display "done"))
         ((timed-prime-test begin)
          (search-for-primes (+ begin 1) (- n 1)))
         (else
          (search-for-primes (+ begin 1) n))))
  

;(trace search-for-primes)
(search-for-primes 2 5)
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)