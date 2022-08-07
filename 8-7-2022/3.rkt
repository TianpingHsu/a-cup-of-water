#lang racket

(define-struct Posn (x y))
;; make-Posn Posn-x Posn-y
(define-struct Segment (point1 point2))
;; make-Segment Segment-point1 Segment-point2

(define (midpoint seg)
  (make-Posn (/ (+ (Posn-x (Segment-point1 seg))
                   (Posn-x (Segment-point2 seg))) 2)
             (/ (+ (Posn-y (Segment-point1 seg))
                   (Posn-y (Segment-point2 seg))) 2)))

;; test => {(1, 2), (3, 4)} -> (2, 3)
(define test-seg
  (make-Segment (make-Posn 1 2) (make-Posn 3 4)))

(define mid (midpoint test-seg))
(Posn-x mid)  ;; 2
(Posn-y mid)  ;; 3
