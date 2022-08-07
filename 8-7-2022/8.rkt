;; 8.d
(define (nums-strs lst)
  (append (sort (filter number? lst) <)
          (sort (filter string? lst) string<?)))


(define l (list 1 "hello" 2 "world"))

(nums-strs l)
