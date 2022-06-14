#lang racket

;;=============================
;; solution of question 1
;;=============================

;; n > 0
(define (times-to-divide-2 n)
  (cond 
    [(= n 0) +inf.0]  ;; if n is 0, we set it infinity
    [(= (modulo n 2) 0) (+ 1 (times-to-divide-2 (/ n 2)))]  ;; recursive call
    [else 0]))  ;; if n is odd, we just return 0

(define (comp x y)
  (cond
    [(= (times-to-divide-2 x) (times-to-divide-2 y)) (< x y)]
    [else (> (times-to-divide-2 x) (times-to-divide-2 y))]))

(define (2-adic-sort l)
  (sort l comp))  ;; just sort it

;; this is used for test
(2-adic-sort (list 0 1 2 3 4 5 6 7 8 9))


