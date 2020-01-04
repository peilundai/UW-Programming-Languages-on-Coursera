#lang racket
(provide (all-defined-out))

(define (fac x)
  (if (= x 1)
      1
      (* x (fac (- x 1)))))


(define x (fac 5))