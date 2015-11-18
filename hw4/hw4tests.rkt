#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))


(define nums_2 (sequence 0 5 6))

(define nums_3 (sequence 0 5 3))

(define nums_4 (sequence 7 5 3))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define nth_value (list-nth-mod (list 1 2 3) 100))


(define funny-test (stream-for-n-steps funny-number-stream 16))

(define dan-dog-test (stream-for-n-steps dan-then-dog 16))

(define cycle-lists-test1 (stream-for-n-steps (cycle-lists (list 1 2 3 4 5) (list "a" "b" "c")) 10))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
 (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))

; should be #f
(define vector-assoc-test1 (vector-assoc 1 (vector 1 2 3)))

; should be (cons 1 2)
(define vector-assoc-test2 (vector-assoc 1 (vector (cons 1 2) 3)))


; should be (cons 4 5)
(define vector-assoc-test3 (vector-assoc 4 (vector (cons 1 2) 3 (cons 4 5))))

(define cached-assoc-test1 (cached-assoc (list (list 1 2) (list 3 4) (list 5 6)) 6))