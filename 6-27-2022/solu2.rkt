(define (search-target target assoc-target-list)
  (cond
    [(empty? assoc-target-list) (error "empty list")]  ;; empty list, we return 0, but this will never happen
    [(equal? target (first (first assoc-target-list))) (second (first assoc-target-list))]
    [else (search-target target (rest assoc-target-list))]))

(define (volume->grams amount unit ingredient units ingredients)
  (* amount 
     (search-target unit units) 
     (search-target ingredient ingredients)))

;; this is used for test
(define (make-pair x y) (list x y))
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

