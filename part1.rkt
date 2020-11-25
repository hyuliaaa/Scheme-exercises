#lang racket

(define (count-divisors n)
  (define (helper count d)
    (cond [(> d n) count]
          [(= 0 (remainder n d)) (helper (+ 1 count) (+ d 1))]
          [else (helper count (+ d 1))]))
  (helper 0 1))

;find sum of divisors
(define (find-sum n)
  (define (helper sum d)
    (cond [(>= d n) sum]
          [(= 0 (remainder n d))(helper (+ sum d) (+ d 1))]
          [else (helper sum (+ d 1))]))
  (helper 0 1))

(define (perfect? n)
  (= (find-sum n ) n))

(define (inc-digits? n)
 (or (< n 10) (and (< (remainder (quotient n 10) 10) (remainder n 10))
                   (inc-digits? (quotient n 10)))))


(define (inc? n)
  (if ( < n 10)
      #t
      (if (> (remainder (quotient n 10) 10) (remainder n 10))
          #f
          (inc? (quotient n 10)))))





(define (minDivisor n)
  (define (helper current)
    (if (= (remainder n current) 0)
        current
        (helper (+ current 1))))
  (helper 2))


(define (maxDivisor n)
  (define (helper current)
    (if (=(remainder n current) 0)
        current
        (helper (- current 1))))
  (helper (- n 1)))


(define (sum-odds a b)
  (cond [(> a b) 0]
        [(odd? a) (+ a (sum-odds (+ a 2) b))]
        [else (sum-odds (+ a 1) b)]))


(define (sum-o a b)
  (define (helper a b sum)
    (> a b)
    sum
    (helper (+ a 2) b (+ a sum)))
  (if (odd? a)
      (helper a b 0)
      (helper (+ a 1) b 0)))



(define (divides? m n)
(=(remainder n m) 0))

(define (prime? n)
  (define (helper i )
   (cond [(= i n) #t]
         [(=(remainder n i) 0) #f]
         [else (helper (+ i 1))]))
  (if(< n 2)
     #f
     (helper 2)))


(define (reverse-num n)
  (define (helper result  curr)
    (if (= curr 0)
        result
        (helper (+ (remainder curr 10) (* 10 result)) (quotient curr 10))))
  (helper 0 n))


(define (palyndrome? n)
  (=(reverse-num n) n))


(define (count-palyndromes a b)
  (cond [(> a b) 0]
        [(palyndrome? a)(+ 1 (count-palyndromes (+ a 1) b))]
        [else (count-palyndromes (+ a 1) b)]))

(define (count-pal a b)
  (sum a b (lambda (i) (if (palyndrome? i) 1 0)) (lambda (x) (+ x 1))))


(define (sum-palyndromes a b)
  (cond [(> a b) 0]
        [(palyndrome? a)(+ a(sum-palyndromes (+ a 1) b))]
        [else (sum-palyndromes (+ a 1) b)]))

(define (sum-pal a b)
  (sum a b (lambda (i) (if (palyndrome? i) i 0)) (lambda (x) (+ x 1))))
        
(define (sum a b term next)
  (if (> a b)
      0
      (+ (term a) (sum (next a) b term next))))

(define (count-div n)
  (sum 1 n (lambda (i) (if (divides? i n) 1 0)) (lambda(x) (+ x 1))))


(define (substr? a b)
  (if (= a 0)
      #t
      (if (= b 0)
          #f
      (if(=(remainder a 10)(remainder b 10))
           (substr? (quotient a 10)(quotient b 10))
           (substr? a (quotient b 10))))))


(define (difference F a b)
  (- (F b) (F a)))




;едноаргументна функция, която връща друга едноаргументна фунцкия
;добавяща своя аргумент към тази на първата
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


(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        (f ((repeated f (- n 1))x)))))


(define (my-repeat f n)
  (if (= n 0)
      identity
      (compose f (my-repeat f (- n 1)))))
((my-repeat (lambda (x) (+ x 1)) 3) 5)

;currying-частично прилагане на функции - прилагане на ф-я само към част от аргументите й
;резултатът е нова функция с по-малък брой аргументи- все едно сме фикс. първите няколко
(define (my-curry f x)
  (lambda (y) ( f x y)))

(define twoToThePowerOf (my-curry expt 2)); все едно сме фиксирали х да е 2 в (expt x y)
(define (f1 x y z) (+ x y z))
(define f2 (curry f1 5))


;c let ще даде грешка unbound indetifier butLast, зашото let се свързва едновременно,
;а лет* летрек последователно

(define (orderedBy pred? n)
  (letrec;let*
      ([butLast (quotient n 10)] ;всички цифри без последната
       [last (remainder n 10)];последна цифра
       [secondToLast (remainder butLast 10)])
    (cond [( < n 10)     #t]
          [(pred? secondToLast last)(orderedBy pred? butLast)]
          [else #f])))

      
(define (sum-div n)
  (sum 1 n (lambda (i) (if(and (divides? i n)) i 0)) (lambda (x) (+ x 1))))

