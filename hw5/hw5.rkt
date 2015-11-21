 ;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

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
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here

        [(fun? e)
         (closure (if (fun-nameopt e) env env) e)]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
                (error "MUPL ifgreater e1 or e2 applied to non-number")))]

        [(mlet? e)
         (let* ([v (eval-under-env (mlet-e e) env)]
                [env (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) env))]

        [(call? e)
         (let ([closz (eval-under-env (call-funexp e) env)])
           (if (closure? closz)
               (let* ([lexical-env (closure-env closz)]
                     [body (fun-body (closure-fun closz))]
                     [fun-name (fun-nameopt (closure-fun closz))]
                     [argument-name (fun-formal (closure-fun closz))]
                     [fun-env (cons (cons argument-name (eval-under-env (call-actual e) env)) lexical-env)]
                     [recur-env (if fun-name (cons (cons fun-name closz) fun-env) fun-env)])
                 (eval-under-env body recur-env))
               (begin (pretty-print closz)
                      (error "MUPL call funexp applied to non-closure"))))]

        [(apair? e)
         (let ([head (eval-under-env (apair-e1 e) env)]
               [tail (eval-under-env (apair-e2 e) env)])
           (apair head tail))]

        [(fst? e)
         (let ([exp (eval-under-env (fst-e e) env)])
           (if (apair? exp)
               (apair-e1 exp)
               (error "MUPL fst applied to non-apair")))]

        [(snd? e)
         (let ([exp (eval-under-env (snd-e e) env)])
           (if (apair? exp)
               (apair-e2 exp)
               (error "MUPL snd applied to non-apair")))]

        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]

        [(aunit? e)
         (aunit)]

        [(int? e)
         (if (number? (int-num e))
             e
             (error "MUPL int applied to non-number"))]
         
           
        [#t (error "bad MUPL expression")]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (if (aunit? e1)
      e2
      e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                                e4
                                (ifgreater (var "_y") (var "_x") e4 e3))))
  

;; Problem 4

(define mupl-map
  (fun #f "f"
      (fun "mupl-map-list" "list"
           (ifeq (int 1) (isaunit (var "list"))
                 (aunit)
                 (apair (call (var "f") (fst (var "list"))) (call (var "mupl-map-list") (snd (var "list"))))))))
      

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mapAddN" "n" (mlet "f" (fun #f "item" (add (var "n") (var "item"))) (call (var "map") (var "f")))))) 

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
