;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo") ; done 
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17); done
(struct add  (e1 e2)  #:transparent)  ;; add two expressions; done
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4; done
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) ; done
(struct apair (e1 e2)     #:transparent) ;; make a new pair; done
(struct fst  (e)    #:transparent) ;; get first part of a pair: done
(struct snd  (e)    #:transparent) ;; get second part of a pair: done
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list: done 
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0; done

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))


(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Problem 2
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))




;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               ((error "MUPL addition applied to non-number"))))]
        
        ;; CHANGE add more cases here
        [(int? e) e]

        [(fun? e) (closure env e)] ; when evaluating function, return closure. 
        
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-numbers")))]

        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)]
               [s (mlet-var e)])
           (eval-under-env (mlet-body e) (cons (cons s v) env)))]

        [(call? e)
         (let ([funexp-val  (eval-under-env (call-funexp e) env)]
               [actual-val  (eval-under-env (call-actual e) env)])
           
           (if (closure? funexp-val)
               (let ([fn_env (closure-env funexp-val)]
                        [fn_exp (closure-fun funexp-val)])
                 (if (fun-nameopt fn_exp); not #f
                     (eval-under-env (fun-body fn_exp)
                                     (cons (cons (fun-nameopt fn_exp) funexp-val)
                                           (cons (cons (fun-formal fn_exp) actual-val) fn_env)))
                     (eval-under-env (fun-body fn_exp)
                                     (cons (cons (fun-formal fn_exp) actual-val) fn_env))))
               (error "MUPL call first argument not closure")))]

        
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(fst? e)
         (let ([value (eval-under-env (fst-e e) env)])
         (if (apair? value)
             (eval-under-env (apair-e1 value) env) ; redundant since value is already evaluated
             (error "fst not a pair")))]

        [(snd? e)
         (let ([value (eval-under-env (snd-e e) env)])
         (if (apair? value)
             (eval-under-env (apair-e2 value) env) ; redundant since value is already evaluated
             (error "snd not a pair")))]

        
        [(aunit? e) e]
        
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]

        [(closure? e) e]
        

        [#t (error (format "bad MUPL expression: ~v" e))]))




;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Testing call
;; (eval-exp (mlet "x" (int 4) (mlet "y" (int 7) (call (fun #false "x" (add (var "x") (var "y"))) (int 9)))))
;; (eval-exp (call (call (fun #false "x" (fun #false "x" (var "x"))) (int 5)) (int 2)))


;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifeq (isaunit e1) (int 1) e2 e3))


(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (ifgreater (ifgreater e1 e2 (int 0) (int 2)) (ifgreater e2 e1 (int 3) (int 1)) e3 e4))

  
;; Problem 4                          

(define mupl-map
  (fun #f "f" (fun "proc" "lst"
                   (ifaunit (var "lst")
                            (aunit)
                            (apair (call (var "f") (fst (var "lst")))
                                   (call (var "proc") (snd (var "lst"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "x" (fun #f "mlist"
                         (call (call (var "map")
                                     (fun #f "i" (add (var "i") (var "x")))) (var "mlist"))))))


;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument
; function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))


