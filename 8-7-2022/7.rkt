;; A leaf-labelled tree (LLT) is one of:
;; * empty
;; * (cons Atom LLT)
;; * (cons LLT LLT)
;; Requires: the LLT contains no nested empty lists

;; An Atom is an (anyof Num Str Sym)

;; 7.a
(define (llt-member? atom llt)
  (cond
    [(empty? llt) false]
    [(not (list? (first llt))) 
     (cond
       [(equal? atom (first llt)) true]
       [else (llt-member? atom (rest llt))])]
    [else 
      (or (llt-member? atom (first llt)) 
          (llt-member? atom (rest llt)))]))

;; 7.b
(define (map-llt-nums f llt)
  (cond
    [(empty? llt) empty]
    [(not (list? (first llt))) 
     (cond
       [(number? (first llt)) (cons (f (first llt)) (map-llt-nums f (rest llt)))]
       [else (cons (first llt) (map-llt-nums f (rest llt)))])]
    [else (cons (map-llt-nums f (first llt)) (map-llt-nums f (rest llt)))]))
