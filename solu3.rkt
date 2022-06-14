#lang racket

;;=============================
;; solution of question 3
;;=============================
(define elements-weight
  (list (cons 'H 1)
        (cons 'C 12)
        (cons 'N 14)
        (cons 'O 16)))
(define (search-target target assoc-target-list)
  (cond
    [(empty? assoc-target-list) (error "empty list")]  ;; empty list, we return 0, but this will never happen
    [(equal? target (caar assoc-target-list)) (cdar assoc-target-list)]
    [else (search-target target (cdr assoc-target-list))]
    ))
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

