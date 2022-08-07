(define-struct team (name division wins losses))
;; 4.a
(define (create-division div los)
  (local {(define (helper name) (make-team name div 0 0))}
         (map helper los)))
(define los (list "Laker" "Racket" "Net"))
;; (list (make-team "Laker" 'West 0 0) (make-team "Racket" 'West 0 0) (make-team "Net" 'West 0 0))
(create-division 'West los)

;; 4.b contract
;; (create-division Sym (listof Str))

;; 4.c
(define (record-game winner loser teams)
  (cond
    [(empty? teams) empty]
    [(string=? (team-name (first teams)) winner) ;; if it's the winner
     (cons (make-team (team-name (first teams))   ;; build a new team
                      (team-division (first teams))
                      (+ 1 (team-wins (first teams)))  ;; win +1
                      (team-losses (first teams)))
           (record-game winner loser (rest teams)))]  ;; here the recursion
    [(string=? (team-name (first teams)) loser) ;; if it's the loser
     (cons (make-team (team-name (first teams))   ;; build a new team
                      (team-division (first teams))
                      (team-wins (first teams))
                      (+ 1 (team-losses (first teams))))  ;; loss +1
           (record-game winner loser (rest teams)))]  ;; here the recursion
    [else (cons (first teams) (record-game winner loser (rest teams)))]))

(record-game "Laker" "Racket" (list (make-team "Laker" 'West 1 0)
                                    (make-team "Racket" 'West 1 0)
                                    (make-team "Net" 'East 3 3)))


(define (comp team1 team2)
  (> (team-wins team1) (team-wins team2)))

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


;; 4.d
 (define (comp team1 team2)
   (> (team-wins team1) (team-wins team2)))

 (define (champion teams)
   (team-name (first (sort teams comp))))



