#lang racket
(define (fact n)
  (if (= n 0)
      1
      {* n (fact(- n 1))}))

(define (fact-iter n)
  (define (helper product counter)
    (if (> counter n)
        product
        (helper (* product counter) (+ 1 counter))))
  (helper 1 1))




(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (gcd a b)
  (if (= a b)
      a
      (if (> a b)
          (gcd (- a b) b)
          (gcd a (- b a)))))

(define (mymaxdivisor x)
  (define (helper d x)
    (if (= (remainder x d) 0)
        d
        (helper (- d 1) x)))
  (helper (- x 1) x))


(define (sum-interval a b)
  (if( > a b)
     0
     (+ a (sum-interval (+ a 1) b))))

(define (sum-iter a b)
  (define (helper sum a)
    (if (> a b)
        sum 
        (+ a (helper sum (+ a 1)))))
  (helper 0 a))

(define (count-digits n)
  (if (<= n 9)
      1
      (+ 1 (count-digits (quotient n 10)))))


(define (count-digits-iter n)
  (define (helper counter n)
    (if (<= n 9)
         counter
        (+ 1 (helper counter (quotient n 10)))))
  (helper 1 n))

(define (sum-odds a b)
  (if (> a b)
      0
      (if(odd? a) (+ a (sum-odds (+ a 2) b))
         (sum-odds (+ a 1) b))))

(define (sum-odds-iter a b)
  (define (helper sum a)
    (if (> a b)
        sum
        (if(odd? a) (+ a (helper sum (+ a 2)))
           (helper sum (+ a 1)))))
  (helper 0 a))

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))


(define (reverse-num n)
  (define (helper number result)
    (if (= number 0)
        result
        (helper (quotient number 10) (+ (* result 10) (remainder number 10)))))
  (helper n 0))

(define (palyndrome? n)
  (= (reverse-num n) n))


(define (sum-digits n)
  (define (helper sum n)
  (if (= n 0)
      sum
      (helper (+ sum (remainder n 10)) (quotient n 10))))
  (helper 0 n))

(define (prime? n)
  (define (helper i )
   (cond [(= i n) #t]
         [(=(remainder n i) 0) #f]
         [else (helper (+ i 1))]))
  (if(< n 2)
     #f
     (helper 2)))


(define (count-palyndromes a b)
  (cond
    [(= a b) (if (palyndrome? a) 1 0)]
    [(palyndrome? a) (+ 1 (count-palyndromes (+ a 1)b))]
    [else (count-palyndromes (+ a 1) b)]))
    
        
(define (count-divisors n)
  (define (helper i counter)
  (if(> i n)
     counter
     (if( =(remainder n i) 0) (helper (+ 1 i) (+ 1 counter))
        (helper (+ 1 i) counter))))
  (helper 1 0))
                                  
