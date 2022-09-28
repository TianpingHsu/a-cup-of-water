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
    [else (or (nested-set-equal? l1 (car l2))
              (member-list? l1 (cdr l2)))]))
    
(define (sub-set? set1 set2)
  (cond
    [(null? set1) #t]
    [(atom? (car set1)) (and (member-atom? (car set1) set2)
                             (sub-set? (cdr set1) set2))]
    [else (and (member-list? (car set1) set2)
               (sub-set? (cdr set1) set2))]))
     
(define (nested-set-equal? set1 set2)
  (and (sub-set? set1 set2) (sub-set? set2 set1)))

;; test for nested-set-equal?
(nested-set-equal? '(1 2 3 4) '(4 3 2 1))  ;; #t
(nested-set-equal? '(1 2 3 4) '(4 1))  ;; #f
(nested-set-equal? '(1 2 (3 4 5)) '(2 (4 3 5) 1))  ;; #t
(nested-set-equal? '(2 1 (3 4 5)) '(2 (4 3 5) 1))  ;; #t
(nested-set-equal? '(1 (3 4 5)) '(2 (4 3 5) 1))  ;; #f
(nested-set-equal? '(1 2 (3 5)) '(2 (4 3 5) 1))  ;; #f
