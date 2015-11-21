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