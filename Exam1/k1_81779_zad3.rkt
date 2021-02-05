#lang racket

;zad3

(define (insert x n l)
  (if (null? l)
      (cons x l)
      (if (= n 0)
          (cons x  l)
          (cons (car l)(insert x (- n 1) (cdr l))))))

;vzimame vtoriq spisyk
(define (get-sec xs)
  (define (helper n count xs)
  (cond [(null? xs) '()]
        [(>= count n)(cons (car xs) (helper n (+ count 1) (cdr xs)))]
        [else (helper n (+ count 1) (cdr xs))]))
  (helper (/ (length xs) 2) 0 xs))

;vzimame pyrviq spisyk
(define (get-first xs)
  (define (helper n count xs)
  (cond [(null? xs) '()]
        [(< count n)(cons (car xs) (helper n (+ count 1) (cdr xs)))]
        [else (helper n (+ count 1) (cdr xs))]))
  (helper (/ (length xs) 2) 0 xs))



(define (shuffle xs)
  (define (helper l m)
    (if (and (null? l) (null? m))
             '()
             (append (list (car l)(car m))(helper (cdr l) (cdr m)))))
  (helper (get-first xs) (get-sec xs)))
             