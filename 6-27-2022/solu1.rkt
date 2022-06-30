;; n > 0
;; this function is used to calculate the number of factor 2 of `n`
(define (times-to-divide-2 n)
  (cond 
    [(= n 0) +inf.0]  ;; if n is 0, we set it infinity
    [(even? n) (+ 1 (times-to-divide-2 (/ n 2)))]  ;; if n is even, then we know it can be divided by 2.
    [else 0]))  ;; if n is odd, we just return 0

;; compare the numbers of factor 2 of `x` and `y`
(define (comp x y)
  (cond
    [(= (times-to-divide-2 x) (times-to-divide-2 y)) (< x y)]
    [else (> (times-to-divide-2 x) (times-to-divide-2 y))]))

(define (insert n lon)
  (cond 
    [(empty? lon) (list n)]
    [(comp n (first lon)) (cons n lon)]  ;; pay attention, we use `comp` here instead of `<`
    [else (cons (first lon)
                (insert n (rest lon)))]))

(define (my-sort lon)
  (cond
    [(empty? lon) empty]
    [else (insert (first lon) (my-sort (rest lon)))]))

(define (2-adic-sort l)
  (my-sort l))  ;; just sort it

;; this is used for test
(2-adic-sort (list 0 1 2 3 4 5 6 7 8 9))


