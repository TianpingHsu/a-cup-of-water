;;=============================
;; solution of question 3
;;=============================

(define elements-weight
  (list (list 'H 1)
        (list 'C 12)
        (list 'N 14)
        (list 'O 16)))
(define (search-target target assoc-target-list)
  (cond
    [(empty? assoc-target-list) (error "empty list")]  ;; empty list, we return 0, but this will never happen
    [(equal? target (first (first assoc-target-list))) (second (first assoc-target-list))]
    [else (search-target target (rest assoc-target-list))]
    ))
(define (get-element-weight element)
  (search-target element elements-weight))

;; peek next element of compound and check if it is a number
(define (peek compound)
  (cond 
    [(or (empty? compound) (symbol? (first compound))) 1]
    [else (first compound)]))

;; calculate the molar mass of the given compound
(define (molar-mass compound)
  (cond
    [(empty? compound) 0]
    [(not (symbol? (first compound))) (molar-mass (rest compound))]
    [else (+ (* (get-element-weight (first compound)) (peek (rest compound)))
             (molar-mass (rest compound)))]))

(define (comp x y)
  (cond 
    [(> (molar-mass x) (molar-mass y)) #t]
    [else #f]))

(define (insert n lon)
  (cond 
    [(empty? lon) (list n)]
    [(comp n (first lon)) (cons n lon)]
    [else (cons (first lon)
                (insert n (rest lon)))]))

(define (my-sort lon)
  (cond
    [(empty? lon) empty]
    [else (insert (first lon) (my-sort (rest lon)))]))

;; just sort the compound list decreasingly
(define (heaviest compounds)
  (first (my-sort compounds)))

;; this is used for test
(define methane (list 'C 'H 4))
(define glucose (list 'C 6 'H 12 'O 6))
(define acetic-acid (list 'C 'H 3 'C 'O 'O 'H))
(define hydrazine (list 'H 2 'N 'N 'H 2))
(define caffeine (list 'C 8 'H 10 'N 4 'O 2))
(define ethanol (list 'C 'H 3 'C 'H 2 'O 'H))

(heaviest (list methane glucose acetic-acid hydrazine caffeine ethanol))

