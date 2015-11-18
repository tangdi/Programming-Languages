
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))
  
(define (string-append-map xs suffix)
  (map (lambda(string) (string-append string suffix)) xs))

(define (list-nth-mod xs n)
  (letrec ([helper (lambda (x xs) (if(= x 0) (car xs) (helper (- x 1) (cdr xs))))])
  (cond [(null? xs) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (helper (remainder n (length xs)) xs)])))

(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (s n accu)
                      (let ([v (s)])
                        (if (= n 0) accu (helper (cdr v) (- n 1) (append accu (list (car v)))))))])
    (helper s n (list))))

(define funny-number-stream
  (letrec ([helper (lambda (n)
                     (let ([v (if (= 0 (remainder n 5)) (- 0 n) n)])
                       (cons v (lambda () (helper (+ 1 n))))))])
  (lambda() (helper 1))))

(define dan-then-dog
   (letrec ([helper (lambda (n)
                     (let ([v (if (= 0 (remainder n 2)) "dan.jpg" "dog.jpg")])
                       (cons v (lambda () (helper (+ 1 n))))))])
  (lambda() (helper 0))))

(define (stream-add-zero s)
  (let ([p (s)])
  (lambda () (cons (cons 0 (car p)) (stream-add-zero (cdr p))))))

(define (cycle-lists xs ys)
  (letrec ([next-list (lambda (x origin) (if (null? x) origin x))]
         [helper (lambda (x y)
                   (let ([x (next-list x xs)]
                         [y (next-list y ys)])
                     (cons (cons (car x) (car y)) (lambda () (helper (cdr x) (cdr y))))))])
    (lambda () (helper xs ys))))

(define (vector-assoc v vec)
  (letrec ([v-length (vector-length vec)]
        [helper (lambda (i) (if (>= i v-length) #f (if (and (pair? (vector-ref vec i)) (equal? v (car (vector-ref vec i)))) (vector-ref vec i) (helper (+ 1 i)))))])
    (helper 0)))

(define (cached-assoc xs n)
  (let* ([i 0]
        [cache (make-vector n)]
        [update-cache (lambda (result) (begin (vector-set! cache i result)
                                              (set! i (remainder (+ 1 i) n))))]
        )
    (lambda (v)
      (let ([cache-result (vector-assoc v cache)])
        (if cache-result
            (begin
              (print "get from cache ")
              (cdr cache-result))
            (let ([calculated-result (assoc v xs)])
              (begin
                (print "get from calc")
                (update-cache (cons v calculated-result))
                calculated-result)))))))
              
        
        