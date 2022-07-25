
(define (parens llt)
  (cond
    [(empty? llt) ""]
    [else (god llt)]))

(define (substr s)
  (cond
    [(> (string-length s) 2) (substring s 1 (- (string-length s) 1))]
    [else ""]))

(define (god llt)
  (cond
    [(empty? llt) ""]
    [(list? (first llt))  ;; let's do the recursion
     (string-append
       "("
       (god (first llt))
       (substr (god (rest llt)))  ;; strip outmost "(" and ")"
       ")")]
    [else
      (cond
        [(string=? (god (rest llt)) "") "()"]  ;; if "", let it be "()"
        [else (god (rest llt))])]))

(parens empty)  ;; ""
(parens (list 1 2 3 4))  ;; ""
(parens (list 'a (list 'b) 'c 'd))  ;; "(())"
(parens (list "f" "g" (list (list "h"))))  ;; "((()))"
(parens (list (list "a" (list "b") "c" "d") "e"
              (list "f" "g" (list (list "h")))))  ;; "((())((())))"
(parens (list (list (list 'a) (list (list 1 2))) 3 4 (list 5) 6))  ;; "((()(()))())"
;; input like this is invalid: (list empty) => nested empty list is not permitted according to the material
