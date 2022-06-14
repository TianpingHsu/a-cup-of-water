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
  (let
    ([xx (times-to-divide-2 x)]
     [yy (times-to-divide-2 y)])
    (if (= xx yy)
      (< x y)
      (> xx yy))))

(define (2-adic-sort l)
  (sort l comp))  ;; just sort it

;; this is used for test
(2-adic-sort (list 0 1 2 3 4 5 6 7 8 9))



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


;;=============================
;; solution of question 3
;;=============================
(define elements-weight
  (list (cons 'H 1)
        (cons 'C 12)
        (cons 'N 14)
        (cons 'O 16)))
;; reuse the `search-target` function
(define (get-element-weight element)
  (search-target element elements-weight))

;; peek next element of compound and check if it is a number
(define (peek compound)
  (cond 
    [(or (empty? compound) (symbol? (car compound))) 1]
    [else (car compound)]))

;; calculate the molar mass of the given compound
(define (molar-mass compound)
  (cond
    [(empty? compound) 0]
    [(not (symbol? (car compound))) (molar-mass (cdr compound))]
    [else (+ (* (get-element-weight (car compound)) (peek (cdr compound)))
             (molar-mass (cdr compound)))]))

;; just sort the compound list decreasingly
(define (heaviest compounds)
  (car (sort compounds 
             (lambda (x y) (> (molar-mass x)
                              (molar-mass y))))))

;; this is used for test
(define methane (list 'C 'H 4))
(define glucose (list 'C 6 'H 12 'O 6))
(define acetic-acid (list 'C 'H 3 'C 'O 'O 'H))
(define hydrazine (list 'H 2 'N 'N 'H 2))
(define caffeine (list 'C 8 'H 10 'N 4 'O 2))
(define ethanol (list 'C 'H 3 'C 'H 2 'O 'H))

(heaviest (list methane glucose acetic-acid hydrazine caffeine ethanol))

