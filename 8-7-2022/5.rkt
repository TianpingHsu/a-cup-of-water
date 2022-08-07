
;; 5
(define (select-sublist lst indices)
  (local
    [(define (select-rec lst indices k)
       (cond
         [(empty? indices) empty]
         [(= (first indices) k) (cons 
                                  (first lst) 
                                  (select-rec (rest lst) 
                                              (rest indices) 
                                              (+ k 1)))]
         [else (select-rec (rest lst) indices (+ k 1))]))]
    (select-rec lst indices 0)))
