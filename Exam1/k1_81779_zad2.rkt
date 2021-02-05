#lang racket



;zad3

(define (sortedL l)
  (sort l <))

(define (remove x l)
  (if (null? l)
      '()
      (if (not (= x (car l))) (cons (car l)(remove x (cdr l)))
      (remove x (cdr l)))))

(define (uniq l)
  (if (null? l)
      '()
      (cons (car l)(uniq (remove (car l) l)))))


(define (kth-max-min xs)
  (define (helper x xs)
    (if (>= (car xs) 0 )
        "No such number"
        (if (= x 1)
            (car xs)
            (helper (- x 1) (cdr xs)))))
          (lambda (x) (helper x (uniq(sortedL xs)))))


     