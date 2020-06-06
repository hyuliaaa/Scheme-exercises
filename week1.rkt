(define (my-even? n)
  (=(remainder n 2) 0))

(define (my-odd? n)
  (not(my-even? n)))


(define (fact n)
  (if (< n 1)
      1
         (* n (fact (- n 1)))))

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib(- n 2)) (fib (- n 1)))))

(define (sum a b)
  (if (> a b)
      0
      (+ a (sum (+ a 1) b))))

(define (sum-overall a b)
  (sum (ceiling a)(floor b)))

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

(define (bool-to-num b)
  (if b 1 0))



(define (digit-occurance d n)
  (if (< n 10)
      (bool-to-num (= n d))
      (+ (bool-to-num (= d (remainder n 10)))
         (digit-occurance d (quotient n 10)))))
