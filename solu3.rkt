(define elements-weight
  (list (list 'H 1)
        (list 'C 12)
        (list 'N 14)
        (list 'O 16)))
;; search target from the assoc-target-list
(define (search-target target assoc-target-list)
  (cond
    [(empty? assoc-target-list) (error "empty list")]  ;; empty list, we return 0, but this will never happen
    [(equal? target (first (first assoc-target-list))) (second (first assoc-target-list))]
    [else (search-target target (rest assoc-target-list))]))
(define (get-element-weight element)  ;; given the element, we search its weight from `elements-weight` dictionary
  (search-target element elements-weight))

;; peek next element of the given compound and check if it is a number
(define (peek compound)
  (cond 
    [(or (empty? compound) (symbol? (first compound))) 1]  ;; if next element is not a number, we just return 1
    [else (first compound)]))  ;; otherwise, we return the number

;; calculate the molar mass of the given compound
(define (molar-mass compound)
  (cond
    [(empty? compound) 0]  ;; if it is empty, we just return 0
    [(not (symbol? (first compound))) (molar-mass (rest compound))]  ;; we come across a number, just skip it
    [else (+ (* (get-element-weight (first compound)) (peek (rest compound)))  ;; do recursion, and sum all of them
             (molar-mass (rest compound)))]))

(define (comp x y)  ;; compare the molar mass of compound `x` and `y`
  (cond 
    [(> (molar-mass x) (molar-mass y)) #t]  ;; to check which compound has bigger molar mass
    [else #f]))

;; helper function `insert`
(define (insert n lon)
  (cond 
    [(empty? lon) (list n)]
    [(comp n (first lon)) (cons n lon)]  ;; pay attention, we use `comp` function here, instead of `<`
    [else (cons (first lon)
                (insert n (rest lon)))]))

;; write our own sort function with insertion sort algorithm
(define (my-sort lon)
  (cond
    [(empty? lon) empty]
    [else (insert (first lon) (my-sort (rest lon)))]))

;; just sort the compound list decreasingly, and return the first one, which is the desired result
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

