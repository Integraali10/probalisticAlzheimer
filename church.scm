(define a 5)
(define f (lambda (a b) (* a b)))
;; recursion
(define (f n)
  (if (= n 0)
      1 
      (* n (f (- n 1)))))
(f 5)

(define (fi n)
  (if (<= n 1)
      1
      (+ (fi (- n 1)) (fi (- n 2))))
)  
(fi 8)


(define (factorial n)
  (define (fac-times n acc)
    (if (= n 0)
        acc
        (fac-times (- n 1) (* acc n))))
  (fac-times n 1))


(define (fib n)
  (define (fibt curr prev n)
    (if (= n 0)
        curr
        (fibt (+ curr prev) curr (- n 1))))
  (fibt 0 1 n))
    
(fib 8)

;; lists
;; car - head of list, cdr - tail , cons - concatination
(define xs '(1 2 -3 4))
'(+ 1 2)
(define (neg xs)
  (if (null? xs)
  '()
  (cons (- (car xs)) (neg (cdr xs)))))
(neg xs)

;; high-order functions
(define (map-list f xs)
  (if (null? xs)
  '()
  (cons (f (car xs)) (map-list f (cdr xs)))))

(map-list (lambda (x)(* x x)) xs)
;; there is map
(map (lambda (x)(* x x)) xs)
;; curry - projection
(define (proj f y)
  (lambda (x) (f x y)))
((proj * 2) 5)
  
(map (proj * (car xs)) xs)
;; summ of list
(define (sigma xs)
  (if (null? xs)
       0
      (+ (car xs) (sigma (cdr xs)))))
(sigma xs)
;; 
(define (sig xs)
  (define(op xs acc)
  (if (null? xs)
      acc
      (op (cdr xs) (+ acc (car xs)) )))
  (op xs 0))

(sig xs)
;;  reduce list in manner of f
(define (reduce-list f xs a)
  (if (null? xs)
      a
      (f (car xs) (reduce-list f (cdr xs) a))))
(reduce-list + '(1 2 3 4) 0)
(reduce-list (lambda (x y) (if (> x y) x y)) '(1 2 3 4) 0)
 
;;  foldl foldr
(foldl * 1 '(1 2 3 4)) 
;; acc sequently apllies to elems of list 
(car (foldr cons '() '(1 2 3 4)))
;; (((() . 4) . 3) . 2)
(car(foldl cons '() '(1 2 3 4)))
;; (((() . 1) . 2) . 3)

(reduce-list cons '(1 2 3 4) '())
;; let's go to probalistic models
(flip)

;; (hist (repeat 1000 flip))
((curry (lambda (y )(* 2 y)) )3)
(repeat 100 (curry neg))

;; repeat
(define (repeats n f)
  (if (<= n 0)
  '()
  (cons (f) (repeats (- n 1) f))))

;; (hist (repeats 100 flip))

;; conditional output
(define (sample)
(rejection-query
 (define E (flip 0.002))
 (define B (flip 0.001))
 (define A 
   (or (and  E (flip 0.29)) 
       (and B (flip  0.94)) 
       (flip 0.001)))
 ;;вероятность сигнализации
 A
 ;; P(A and B| A or B)
 ))

;;(hist(repeat 1000 sample ))
(foldl + 0 (map (lambda (x) (if x 1 0)) (repeat 1000 sample)))

;; conditional output
(define (sample)
(rejection-query
 (define E (flip 0.002))
 (define B (flip 0.001))
 (define A 
   (or (and  E (flip 0.29)) 
       (and B (flip  0.94)) 
       (flip 0.001)))
;; Вероятность кражи если сработала сигнализация и случилось землетрясение
;;  B
;;  (and A E)
;;Вероятность землятрясения если сработала сигнализация
 E
 A
 ))

;; (hist(repeat 1000 sample ))
(foldl + 0 (map (lambda (x) (if x 1 0)) (repeat 1000 sample)))

;;Парадокс о старшем ребенке (девочка мальчик) - отбрасываются семьи, где старший ребенок - девочка
(define (paradox)
(rejection-query
 (define ElderBoy(flip 0.5))
 (define Jungen(flip 0.5))
 (define YoungBoy 
   (if ElderBoy 
       Jungen 
       Jungen))
  YoungBoy
))
;;Парадокс о ребенке (девочка мальчик) - берется произвольная семья
(define(paradox)
  (rejection-query
 (define Boy(flip 0.5))
 (define Jungen(flip 0.5))
 (and Boy Jungen)
 (or Boy Jungen)
))
(hist(repeat 1000 paradox))
(foldl + 0 (map (lambda (x) (if x 1 0)) (repeat 1000 paradox)))
