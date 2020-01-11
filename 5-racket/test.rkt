#lang racket
(provide (all-defined-out))



; sum all the numbers in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; append function
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))


(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))


(define cube
  (lambda (x) (* x x x)))


(define (cube1 x) (* x x x))


; Parentheses Matter in Rackets

(define (fact n)
  (if (= n 0)
      1 ; wrong if we use (1)
      (* n (fact (- n 1)))))


; Dynamic Typing
(define xs (list 4 5 6))
(define ys (list (list 4 5) 6 7 (list 8)))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

; Cond
(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [(list? (car xs)) (+ (sum3 (car xs)) (sum3 (cdr xs)))]
        [#t (sum3 (cdr xs))]))

; the condition expression in if and cond doesn't have to be #t or #f.
; Anything not #f is #t


; Local bindings
(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given error")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                     tlans
                     (car xs)))]))


; Racket has four ways to define local variables
; 1) let
; 2) let*
; 3) letrec
; 4) define

; let expression
(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)]) ; x refers to the argument x passed into the function
    (+ x y - 5)))

; let* expressions

(define (silly-double* x)
  (let ([x (+ x 3)]
        [y (+ x 2)]) ; x refers to the x in the let environment, same as in ML
    (+ x y -5)))

; lecrec expression: the expression are evaluated in the env that includes all the bindings
; the earlier ones and also later ones. 
; needed for mutual recursion
; advised using it only when mutual recursion
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda(z) (+ z y w x))]
           [w (+ x 7)]) 
    (f -9)))

; mutual recursion
(define (silly-mod2 x)
  (letrec
      ([even? (lambda(x) (if (zero? x) #t (odd? (- x 1))))]
       [odd?  (lambda(x) (if (zero? x) #f (even? (- x 1))))])
    (if (even? x) 0 1)))

; don't use undefined binding
(define (bad-letrec x)
  (letrec ([y z]
           [z 13])
    (if x y z)))


; define expression, same effect as letrec, better style in Racket. 

(define (silly-mod x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))


; top-level bindings
; refer to earlier as well as later bindings
; cannot define the same variables in the same env

;(define (f x) (+ x (* x b))) ; ok to refer to later binding b
;(define b 3)
;(define x (+ b 4)) ; also ok to refer backward binding even not in function

;(define d (+ e 5)); doesn, t work, it is not in a function body
;(define e 4)
;(define f 15) ; not ok, because f is already defined in this module


; each file is implicity a module
; we can shadow (in own module) other functions, e.g. +



; Mutation with set!
; not used a lot
;(set! x 3) ; pronounced set bang
; saimilar to x = e in Java, C, Python

;(begin e1 e2 en); sequencing, evluations the last en, others are for side effects

; make a local copy of something if you don't want something to change
(define b 3)
(define (f11 x)
  (let ([b b]) ; make local copies, define doesn't work here
    (* 1 (+ x b))))


; proper list and pairs

; mcons cells
(define x (cons 14 null))
(define y x)
(set! x (cons 42 null)) ; this only changes x, but y values stays the same

; we cannot do the following
(define z x)
;(set! (car x) 45); this is not allowed

(define mpr (mcons 1 (mcons 2 null)))



; delayed evaluation and thunks


(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))















































