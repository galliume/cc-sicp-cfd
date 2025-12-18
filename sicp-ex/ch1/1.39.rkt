#lang racket

(define (tan-cf x k)
  (define (iter i result)
    (if (= i 0)
        result
        (let ((Ni (if (= i 1) x (- (* x x))))
              (Di (- (* 2 i) 1)))
          
          (iter (- i 1)
                (/ Ni (+ Di result))))))
  
  (iter k 0))

(tan-cf 10.0 100)