#lang racket

;;=============================
;; solution of question 2
;;=============================

(define (search-target target assoc-target-list)
  (cond
    [(empty? assoc-target-list) (error "empty list")]  ;; empty list, we return 0, but this will never happen
    [(equal? target (caar assoc-target-list)) (cdar assoc-target-list)]
    [else (search-target target (cdr assoc-target-list))]
    ))

(define (search-unit unit units)
  (search-target unit units))

(define (search-ingredient ingredient ingredients)
  (search-target ingredient ingredients))

(define (volume->grams amount unit ingredient units-dict ingredients-dict)
  (* amount 
     (search-unit unit units) 
     (search-ingredient ingredient ingredients-dict)))

;; this is used for test
(define (make-pair x y) (cons x y))
(define units
  (list (make-pair "cups" 237)
        (make-pair "teaspoons" 5)
        (make-pair "tablespoons" 15)))
(define ingredients
  (list (make-pair "flour" 0.53)
        (make-pair "sugar" 0.85)
        (make-pair "butter" 0.96)))

(volume->grams 0.5 "cups" "sugar" units ingredients)
(volume->grams 3 "tablespoons" "butter" units ingredients)
(volume->grams 120 "teaspoons" "flour" units ingredients)


