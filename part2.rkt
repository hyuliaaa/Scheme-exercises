#lang racket
(define l '((1 (2)) (((3) 4) (5 (6)) () (7)) 8))
(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms l)
  (cond [(null? l) 0]
        [(atom? l) 1]
        [else (+ count-atoms (car l))(count-atoms cdr l)]))

(define (flatten l)
  (cond [(null? l) '()]
        [(atom? l)(list l)]
        [else (append (flatten (car l))(flatten (cdr l)))]))


(define (deep-reverse l)
  (cond [(null? l) '()]
        [(atom? l) l]
        [else (append (deep-reverse (cdr l))(list(deep-reverse (car l))))]))
(define (maximum l)
  (if (null? (cdr l))
      (car l)
      (max (car l)(maximum (cdr l)))))



(define (minimum l)
  (if (null? (cdr l))
      (car l)
      (min (car l) (minimum (cdr l)))))

(define (remove x l)
  (if (null? l)
      '()
      (if (not (= (car l) x))
          (cons (car l)(remove x (cdr l)))
          (remove x (cdr l)))))

(define (selection-sort l)
  (if (null? l) '()
      (cons (minimum l)(selection-sort(remove (minimum l)l )))))


(define (zip l m)
  (if (or (null? l)(null? m))
      '()
     (cons (cons (car l)(car  m)) (zip (cdr l)(cdr m)))))



(define (unique l)
  (if (null? l)
      '()
      (cons (car l)(unique (remove (car l) l)))))





(define (intersection l m)
  (filter (lambda (x) (member x m)) l))


(define (union l m)
  (unique (append l m)))


;filtrirame tezi elementi v l, koito ne sa ot m
(define (set-minus l m)
  (filter (lambda (x) (not (member x m)))l))


(define (take n L)
  (if (or (null? L)
          (= n 0))
      '()
      (cons (car L) (take (- n 1) (cdr L)))))

(define (drop n L)
  (if (or (null? L)
          (= n 0))
      L
      (drop (- n 1) (cdr L))))

(define (chunk n L)
  (if (null? L)
      L
      (cons (take n L)
            (chunk n (drop n L)))))




(define m '((1 2 3) (4 5 6)(7 8 9 )))

(define (get-first-row m) (car m))
(define (get-first-column m) (map car m ))


(define (delete-first-row m) (cdr m))
(define (delete-first-column m)(map cdr m))

(define (get-row i m)
  (list-ref m i))
(define (get-column i m)
  (map (lambda (row) (list-ref row i)) m))

(define (transpose m)
  (if (null? (get-first-row m)) '()
      (cons (get-first-column m)(transpose (delete-first-column m)))))

(define (sum-vectors v1 v2) (map + v1 v2))
(define (sum-matr m1 m2)(map sum-vectors m1 m2))


(define (map-matrix f m)
  (map (lambda (row) (map (lambda (x) (f x)) row)) m))

(define (get-columns m)
  (length (car m)))

(define (get-rows m)
  (length(get-first-column m)))

(define (dimensions m)
  (cons (get-rows m)(get-columns m)))


(define (reverse-rows m)
  (map (lambda (row) (reverse row)) m))





(define (inc? n)
  (if ( < n 10)
      #t
      (if (> (remainder (quotient n 10) 10) (remainder n 10))
          #f
          (inc? (quotient n 10)))))

(define (dec? n)
  (if (< n  10)
      #t
      (if (< (remainder (quotient n 10) 10)(remainder n 10))
          #f
          (dec? (quotient n 10)))))

(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a) (sum (next a) b term next))))

(define (sum-numbers a b)
  (sum a b (lambda (i) (if (dec? i) i 0)) (lambda (x) (+ x 1)))) 



(define (number-of-greater-el x l)
 (length (filter (lambda (i) (> i x)) l)))
                              

(define (num-bigger-elements l)
  (if (null? l)
      '()
      (cons (list (car l) (number-of-greater-el (car l) l)) (num-bigger-elements (cdr l)))))



(define (addY x) (λ (y) (+ x y)))
(define addFive (addY 5))
(define (add x y) (λ (a b) (+ a b)))

(define (negate pred?)
  (lambda (x) (not (pred? x))))
((negate (λ (x) (> x 10))) 5)



(define (my-twice f)
  (lambda (x) (f (f x))))


(define (compose f g)
  (lambda (x) (f (g x))))


(define (switchsum f g n)
  (define (helper count prev result)
    (cond [(= count n)  result]
          [(odd? count) (helper (+ count 1) (f prev) (+ result prev))]
          [else         (helper (+ count 1) (g prev) (+ result prev))]))
  (lambda (x) (helper 0 (f x) 0)))



(define (repeater str)
  (define (helper count glue)
    (if (= count 1)
        str
        (string-append str glue(helper (- count 1) glue))))
  (lambda(count glue)(helper count glue)))

(define(naive-levenshtein w1 w2)
 (cond [(null? w1) (length w2)]
       [(null? w2) (length w1)]
       [(equal? (car w1) (car w2)) (naive-levenshtein (cdr w1) (cdr w2))]
       [else                       (+ 1 (min (naive-levenshtein (cdr w1) w2)
                                              (naive-levenshtein w1       (cdr w2))
                                              (naive-levenshtein (cdr w1) (cdr w2))))]))
    
(define (tabulate f)
  (define (helper a b)
    (if (> a b)
        '()
        (cons(cons a (f a))(helper (+ a 1) b))))
  (lambda (a b) (helper a b)))

(define function-list (list sqr
                            (λ (x) (* x x x))
                            (λ (x) (+ x 1))
                            (λ (x) (- x 1))
                            (λ (x) (+ x 2))))
(define (pair-compose f)
  (define (composed-list l)
    (cond [(null? l) '()]
          [(null? (cdr l)) (list (compose (car l)identity))]
          [else            (cons (compose (car l)(cadr l))(composed-list(cddr l)))]))
  (lambda (x) (foldr + 0 (map (lambda (f) (f x)) (composed-list f)))))



          
