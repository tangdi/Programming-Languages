#lang racket

(require "hw5.rkt")

(racketlist->mupllist null)
(racketlist->mupllist (list  1 2 3))
(mupllist->racketlist (aunit))
(mupllist->racketlist (apair 1 (apair 2 (apair 3 (aunit)))))

(mlet* (list (cons "_x" (int 3))) (var "_x"))
 
(ifeq (int 1) (int 1) (int -1) (int 1))

;(call mupl-mapAddN (int 7))

(racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))

;(eval-exp (call (call mupl-mapAddN (int 7)) (aunit)))

;(eval-exp (call (call mupl-mapAddN (int 7)) (apair (int 7) (aunit))))
; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

(define compute-free-vars-test1 (compute-free-vars (fun #f "a" (var "a"))))

(define compute-free-vars-test2 (compute-free-vars (fun #f "a" (var "v"))))

(define compute-free-vars-test3 (compute-free-vars 
        (fun "mapAddN" "n" (mlet "f" (fun #f "item" (add (var "n") (var "item"))) (call (var "map") (var "f"))))))

(compute-free-vars mupl-mapAddN)

(define test2
  (mupllist->racketlist
   (eval-exp-c (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

(eval-exp-c (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))

(eval-exp-c (ifeq (int 1) (int 2) (int 3) (int 4)))

(eval-exp (ifeq (int 1) (int 2) (int 3) (int 4)))
