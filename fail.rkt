#lang racket

(define l '(1 2 3 4))

(define (len l)
  (if (null? l)
      0
      (+ 1 (len(cdr l)))))

(define (sum l)
  (if (null? l)
      0
      (+ (car l)(sum(cdr l)))))

(define (prod l)
  (if (null? l)
      1
      (* (car l)(prod(cdr l)))))

(define (accum combiner init l)
  (if (null? l)
      init
      (combiner (car l) (accum combiner init (cdr l)))))

(define (sum-l l)
  (accum + 0 l))

(define (member? x xs)
  (cond [(null? xs) #f]
        [(equal? x (car xs) )#t]
        [else (member? x (cdr xs))]))


(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs)(my-append (cdr xs) ys))))

(define (reverse xs)
  (define (helper xs result)
    (if (null? xs)
        result
        (helper (cdr xs) (cons (car xs) result))))
  (helper xs '()))

(define (minimum xs)
  (define (helper min xs)
    (cond [(null? xs)       min]
          [(< (car xs) min) (helper (car xs) (cdr xs))]
          [else             (helper  min (cdr xs))]))
  (helper (cdr xs) (car xs)))
                  


(define (maximum xs)
  (define (helper max xs)
    [cond [(null? xs) max]
          [(> (car xs) max) (helper (car xs) (cdr xs))]
          [else             (helper max (cdr xs))]])
  (helper (car xs)(cdr xs)))


(define (ordered l pred?)
  (cond [(null? l) #t]
        [(null? (cdr l)) #t]
        [(pred?(car l) (cadr l)) (ordered (cdr l) pred?)]
        [else #f]))

(define(take n l)
   (if (or (null? l)(= n 0)) 
       '()
       (cons (car l)(take (- n 1) (cdr l)))))

(define (drop n l)
  (define (helper num l)
    (if (= num n)
        l
        (helper (+ num 1)(cdr l))))
  (helper 0 l))


(define (zip xs ys)
  (cond [(null? xs) '()]
        [(null? ys) '()]
        [else (cons (cons (car xs) (car ys)) (zip (cdr xs ) (cdr ys)))]))
(define (newZip-l xs ys)
  (map cons xs ys))
                  
  
(define (any? pred? l)
  (if (null? l)
      #f
      (if (pred? (car l))
          #t
          (any? pred? (cdr l)))))
         
(define (all? pred? l)
  (if (null? l)
      #t
      (if (pred? (car l))
          (all? pred? (cdr l))
          #f)))


(map list '(1 2) '(1 3)); pravi -> ( (1 1) (2 3) )
(apply + '(1 2 3))


;obhojda ot dqsno na lqvo
(define (my-foldr op nv l)
  (if (null? l)
      nv
      (op (car l)(foldr op nv (cdr l)))))
(define (appendFold xs ys)
  (foldr cons ys xs))

(define (sublist a b l)
  (filter (lambda (x) (and (>= x a) (<= x b))) l))

(define (sublist-l a b l)
  (take b(drop a l)))

(define (insert n x l)
  (if (null? l)
      (list l)
      (if (= n 0) (cons x l)
          (cons (car l)(insert (- n 1) x (cdr l))))))

(define (remove-first x l)
        (if (null? l)
            '()
            (if (= x (car l)) (cdr l)
                (cons (car l) (remove-first x (cdr l))))))

(define (remove-all x l)
  (if (null? l)
      '()
      (if (not(= x (car l))) (cons (car l) (remove-all x (cdr l)))
          (remove-all x (cdr l)))))

(define (remove-dups l)
  (if (null? l)
      '()
      (cons (car l)(remove-dups (remove-all (car l)(cdr l))))))


(define (pref? subl l)
  (equal? subl (take (length subl) l)))


;todo finish isSubSeq
(define (isSubSeq subl l) ; dali elementite na 1 spisyk se sydrujat v drugiq
  (if (null? subl)
      #t
      (if (null? l)
          #f
          (if (equal?(car subl) (car l)) (isSubSeq (cdr subl)(cdr l))
              (isSubSeq subl (cdr l))))))



(define (count-occurences subl l)
  (define (helper curr counter)
    (cond [(null? curr) counter]
          [(pref? subl l) (helper (cdr curr)(+ 1 counter))]
          [else              (helper (cdr curr) counter)]))
  (helper l 0))


(define (orderedPrefix l)
  (cond [(null? l) '()]
        [(null? (cdr l))l ]
        [(<=(car l)(cadr l)) (cons (car l)(orderedPrefix (cdr l)))]
        [else                 (list (car l))]))





