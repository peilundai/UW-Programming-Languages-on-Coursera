
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
(require rackunit)
;; put your code below


; q1
(define (sequence low high stride)
  (if (> low high)
       null
       (cons low (sequence (+ low stride) high stride))))

;q2
(define (string-append-map xs suffix)
  (let ([add-suffix (lambda (x) (string-append x suffix))]) (map add-suffix xs)))


;q3
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))


; q4
(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (let ([ans (s)])
        (cons (car ans) (stream-for-n-steps (cdr ans) (- n 1))))))

; q5
(define funny-number-stream
  (letrec ([f (lambda (n)
               (if (= (remainder n 5) 0)
                   (cons (- n) (lambda () (f (+ n 1))))
                   (cons n (lambda () (f (+ n 1))))))])
    (lambda () (f 1))))

; q6
(define dan-then-dog
  (letrec ([even? (lambda (n) (= (remainder n 2) 0))]
           [f (lambda (n)
                (if (even? n)
                    (cons "dan.jpg" (lambda () (f (+ n 1))))
                    (cons "dog.jpg" (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

; q7
(define (stream-add-zero s)
  (define ans (s))
  (lambda () (cons (cons 0 (car ans)) (stream-add-zero (cdr ans)))))

; q8
(define (cycle-lists xs ys)
  (define (f xs ys n)
    (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f xs ys (+ n 1)))))
  (lambda () (f xs ys 0)))


; q9
(define (vector-assoc v vec)
  (define (f v vec n)
    (if (>= n (vector-length vec))
        #f
        (begin (let ([value (vector-ref vec n)])
               (if (pair? value)
                   (if (equal? v (car value))
                       value
                       (f v vec (+ n 1)))
                   (f v vec (+ n 1)))))))
  (f v vec 0))
        
; q10
(define (cached-assoc xs n)
  (define cache (make-vector n #f)); cache is a vector of (key val) or #f
  (define next 0) ; index to store next entry of cache
  (define (next-ind k) (remainder (+ k 1) n))
  (define (search-cache v indx)
    (if (>= indx (vector-length cache))
            #f
            (search-cache v (+ indx 1))))
  (lambda (v)
    (define in-cache (search-cache v 0)) ; search in cache
    (if in-cache
        in-cache
        (let ([new-return (assoc v xs)])
          (if new-return ; exist in xs
              (begin (vector-set! cache next new-return)
                     (set! next (next-ind next))
                     new-return)
              #f)))))
    
; q11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([ans e1]
              [f (lambda ()
                   (letrec ([ans2 e2])
                   (if (>= ans2 ans)
                       #t           
                       (f))))]) (f))]))
           







          