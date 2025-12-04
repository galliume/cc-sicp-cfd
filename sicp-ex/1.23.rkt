#lang racket

(require racket/trace)
;(current-milliseconds) replace (runtime)

(define (square a)
  (* a a))

(define (next n)
  (cond ((= n 2) 3)
        (else (+ n 2))))

(define (divides? a b)
(= (remainder b a ) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

;(trace prime?)
(prime? 199)
(prime? 1999)
(prime? 19999)

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
  (if (prime? n)
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
(search-for-primes 1 5)
(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
(search-for-primes 10000000 3)
(search-for-primes 100000000 3)