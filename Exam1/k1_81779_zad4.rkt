#lang racket

(define (get-first-column m)
  (map car m))

(define (get-dimension m)
  (length (get-first-column m)))




(define (triangular? m)
  (if (null? m)
      #f
      (if (= 0 (car (car m) )) #f
                 (if (not (= (cadr (get-first-column m)) 0))
                     #f
                     #t))))


