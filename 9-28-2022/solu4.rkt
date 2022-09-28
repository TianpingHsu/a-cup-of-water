#lang racket

(define (atom? x) (not (list? x)))
(define (member-atom? x l)
  (cond
    [(null? l) #f]
    [(equal? x (car l)) #t]
    [else (member-atom? x (cdr l))]))

(define (member-list? l1 l2)
  (cond
    [(null? l2) (equal? l1 l2)]
    [(atom? (car l2)) (member-list? l1 (cdr l2))]
    [else (or (set-equal? l1 (car l2))
              (member-list? l1 (cdr l2)))]))
    
(define (member? x l)
  (cond
    [(atom? x) (member-atom? x l)]
    [else (member-list? x l)]))

(define (intersection set1 set2)
  (cond
    [(null? set1) set1]
    [(member? (car set1) set2) (cons (car set1) (intersection (cdr set1) set2))]
    [else (intersection (cdr set1) set2)]))

;; test for intersection
(intersection '(1 2 3 4) '(2 3 4 5))  ;; '(2 3 4)
(intersection '(1 2) '(3 4)) ;; '()

