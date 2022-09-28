#lang racket

(define (combine a r)
  (cond
    [(null? r) empty]
    [else (cons (first r)
                (cons (cons a (first r))
                      (combine a (cdr r))))]))
(define (powerset lst)
  (cond
    [(null? lst) (list empty)]
    [else (combine (car lst)
                   (powerset (cdr lst)))]))        

;; test for power set
(powerset '(1 3 5))  ;; '(() (1) (3) (1 3) (5) (1 5) (3 5) (1 3 5))
(powerset '())  ;; '(())

