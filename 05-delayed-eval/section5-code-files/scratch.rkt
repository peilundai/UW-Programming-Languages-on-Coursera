#lang racket

;(provide (all-defined-out))
;
;(define (my-if-bad x y z)
;  (if x y z))
;
;(define (my-if x y z)
;  (if x (y) (z)))
;
;(define (fact n)
;  (my-if (= n 0)
;         (lambda () 1)
;         (lambda () (* n (fact (- n 1))))))
;
;
;(define (my-delay th)
;  (mcons #f th))
;
;(define (my-force p)
;  (if (mcar p)
;      (mcdr p)
;      (begin (set-mcar! p #t)
;             (set-mcdr! p ((mcdr p)))
;             (mcdr p))))
;

(define (number-until stream tester)
  (letrec
      ([f (lambda (stream ans)
            (let ([pr (stream)])
              (if (tester (car pr))
                  ans
                  (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

; streams

(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

; macros

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3)
     (if e1 e2 e3)]))


(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e)
     (mcons #f (lambda () e))]))














  





  





  
  