
(define-struct node (key left right))
;; make-node, node-key, node-left, node-right

;; 6.a
(define my-bst
        (make-node 8
                   (make-node 3
                              empty
                              (make-node 6 empty empty))
                   (make-node 11 empty empty)))

;; 6.c
(define (minimum bst)
  (cond
    [(empty? (node-left bst)) (node-key bst)]
    [else (minimum (node-left bst))]))
(define (maximum bst)
  (cond
    [(empty? (node-right bst)) (node-key bst)]
    [else (maximum (node-right bst))]))

(define (bst-range bst)
  (list (minimum bst) (maximum bst)))


;; 6.d contract of bst-range
;; (bst-range bst) -> (Num Num)
