#lang racket
;zad1

;stepen
(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))



;pravim spisyk

(define (makeList  n)
  (if (<= n 9)
      (list n)
      (cons (remainder n 10)(makeList (quotient n 10)))))

;obryshtame spisyka
(define (rev-list n)
  (reverse (makeList n)))

;namirame chisloto

(define (find-num  n p) 
 (define (helper l p)
  (if (null? l)
                  0
                  (+ (pow (car l) p) (helper (cdr l)(+ p 1)))))
   (helper (rev-list n) p))

;glavnata funkciq
(define (dig-pow n p)
  (if (= (remainder (find-num n p) n) 0)
      (/ (find-num n p) n)
      -1))


  
