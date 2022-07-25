
(define-struct node (key left right))
;; make-node, node-key, node-left, node-right

;; start of rotate right
(define (copy-bst bst)
  (cond
    [(empty? bst) empty]
    [else (make-node (node-key bst) 
                     (copy-bst (node-left bst))
                     (copy-bst (node-right bst)))]))

(define (rotate-right n bst)
  (cond
    [(empty? bst) empty]  ;; and basic case
    [(< n (node-key bst))
     (make-node (node-key bst) (rotate-right n (node-left bst)) (copy-bst (node-right bst)))]
    [(> n (node-key bst))
     (make-node (node-key bst) (copy-bst (node-left bst)) (rotate-right n (node-right bst)))]
    [else
      (cond
        [(empty? (node-left bst))  ;; case 2
         (make-node (node-key bst)
                    (node-left bst)
                    (node-right bst))]
        [else  ;; case 3
          (local
            [(define m (node-left bst))
             (define alpha (node-left (node-left bst)))
             (define beta (node-right (node-left bst)))
             (define gamma (node-right bst))]
            (make-node (node-key m)
                       (copy-bst alpha)
                       (make-node (node-key bst)
                                  (copy-bst beta)
                                  (copy-bst gamma)))
            )])]))
;; end of rotate-right

(define (set-insert n set)
  (cond
    [(empty? set) (make-node n empty empty)]
    [(= n (node-key set)) set]
    [(< n (node-key set)) 
     (make-node (node-key set) (set-insert n (node-left set)) (node-right set))]
    [else 
      (make-node (node-key set) (node-left set) (set-insert n (node-right set)))]))

(define (list->bst lon) (foldr set-insert empty lon))
(define (bst->list set)
  (cond
    [(empty? set) empty]
    [else (append (bst->list (node-left set))
                  (list (node-key set))
                  (bst->list (node-right set)))]))
(define (treesort lon) (bst->list (list->bst lon)))

(define (preorder set)
  (cond
    [(empty? set) empty]
    [else (append (list (node-key set))
                  (preorder (node-left set))
                  (preorder (node-right set)))]))

(define (postorder set)
  (cond
    [(empty? set) empty]
    [else (append  (postorder (node-left set))
                   (postorder (node-right set))
                   (list (node-key set)))]))


  (define x empty)
  (define y (set-insert 10 x))
  (define z (set-insert 8 y))
  (define a (set-insert 6 z))
  (define b (set-insert 7 a))
  (define c (set-insert 5 b))
  (define d (set-insert 9 c))
  (define ee (set-insert 11 d))
  ;;(println (preorder y))
  ;;(println (preorder z))
  ;;(println (preorder a))
  ;;(println (preorder b))
  ;;(println (preorder c))
  ;;(println (preorder d))
  (preorder ee)  ;; before rotate
  (preorder (rotate-right 8 ee))  ;; after rotate

