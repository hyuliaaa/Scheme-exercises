#lang racket



(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a) (sum(next a) b term next))))
      


(define (sum-odd a b)
  (cond [(> a b) 0]
        [(odd? a)(+ a (sum-odd (+ a 1)b))]
        [else (sum-odd (+ a 1) b)]))


(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (length-iter l)
  (define (helper counter l)
    (if (null? l)
        counter
        (helper (+ counter 1) (cdr l))))
  (helper 0 l))

(define (my-member? x l)
  (if (null? l)
      #f
      (if (= x (car l)) #t (my-member? x (cdr l)))))

(define (push-back x l)
  (if (null? l)
      (list x)
      (cons (car l)(push-back x (cdr l)))))

(define (reverse xs)
  (define (helper xs result)
    (if (null? xs)
        result
        (helper (cdr xs) (cons (car xs) result))))
  (helper xs '()))

(define (minimum xs)
  (define (helper xs min)
    (cond [(null? xs)       min]
          [(< (car xs) min) (helper (cdr xs) (car xs))]
          [else             (helper (cdr xs) min)]))
  (helper (cdr xs) (car xs)))

(define (maximum xs)
  (define (helper xs max)
    (cond [(null? xs) max]
          [(>(car xs) max) (helper (cdr xs)(car xs))]
          [else            (helper (cdr xs) max)]))
  (helper (cdr xs) (car xs)))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op(term a)(accumulate op nv (next a) b term next))))

(define (accum combiner init l)
  (if (null? l)
      init
      (combiner (car l)(accum combiner init (cdr l)))))

(define (sum-l l)
  (accum + 0 l))


(define (delete x l) ;iztriva vsichki sreshtaniq
  (if (null? l)
      '()
      (if (not(= x (car l))) (cons (car l)(delete x (cdr l)))
              (delete x (cdr l)))))
             

             
(define (delete-first x l)
  (cond [(null? l) '()]
        [(= (car l) x) (cdr l)]
        [else (cons (car l)(delete-first x (cdr l)))]))

(define (insert x n l)
  (if (null? l)
      (list x)
      (if(= n 0) (cons x l)
         (cons (car l) (insert x (- n 1)(cdr l))))))

(define (concat l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (concat (cdr l1) l2))))

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

(define (count-digits n)
  (if (= n 1)
      1
      (+ 1 (count-digits (quotient n 10)))))

;vij
(define (sum-digits n)
  (if ( = n 0)
      0
      (+ (* (count-digits n)(remainder n 10))(sum-digits (quotient n 10)))))

(define (narcissistic? n)
  (= n (sum-digits n)))


;razdelqme spisyk spored pred?

(define (split l pred?)
  (list (filter pred? l)
        (filter (compose not pred?) l)))

(define (explode-digits-rev n)
  (if (< n 10)
      (list n)
      (cons (remainder n 10) (explode-digits-rev (quotient n 10)))))

(define (explode-digits n)
  (reverse (explode-digits-rev n)))

(define (take n l)
  (if (or (null? l)(= n 0))
      '()
      (cons (car l)(take (- n 1) (cdr l)))))


(define (drop n l)
  (define (helper counter l)
    (if (>= counter n)
        l
        (helper (+ counter 1)(cdr l))))
  (helper 0 l))


(define (list-ref l n)
  (cond [(null? l)]
        [(= n 0) (car l)]
        [else (list-ref (cdr l) (- n 1))]))


(define (digit-occurence d n)
  (length (filter (lambda (x) (= x d)) (explode-digits n))))

(define (foldr op nv l)
  (if (null? l)
      nv
      (op(car l) (foldr op nv (cdr l)))))

(define (my-identity x) x)

(define (my-compose f g)
  (lambda (x) (f (g x))))


(define (my-negate p?)
  (lambda (x) (not(p? x))))


(define (difference f a b)
  (- (f a) (f b)))


