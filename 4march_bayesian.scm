(define n 8.)
(define m 2.)

;;бесчестная реализация баейсовского вывода
(define (bernoulli)
(rejection-query
 (define q (flip (/ n  (+ m n))))
 (define unq (flip (/ m  (+ m n))))
 q
))
(foldl + 0 (map (lambda (x) (if x 1 0)) (repeat 1000 bernoulli)))


(define (sum elemList)
  (if
    (null? elemList)
    0
    (+ (if (car elemList) 1 0) (sum (cdr elemList)))
  )
)

; Собственный ужос
(define (mpp)
  (rejection-query
   (define q (uniform 0 1))
   (define probs (repeat (+ n m) (lambda() (flip (- 1 q)))))
   (<= (sum probs) n)   
))

(define data (repeat 1000 (lambda () (sum (map (lambda (x) (if x 1 0)) (repeat 100 mpp))))))
(hist data  "Distribution of q")


;нечестная монетка и байесовский вывод
(define (mpp1)
  (rejection-query
   (define q (uniform 0 1))
   (define xs (repeat (+ n m) (lambda() (if (flip q) 1 0))))
   (define n_ (sum xs))
   q
   (= n n_)
))

;; (hist (repeat 1000 mpp1))
;(define res (repeat 1000 mpp1))
;(hist( map (lambda(x) (second x)) res))



;честна ли монетка
(define (mpp2)
  (rejection-query
   (define p 0.5)
   (define q (uniform 0 1))
   (define trulycoin (flip 0.99))
   (define probs (repeat (+ n m) (lambda() (if trulycoin (if (flip p) 1 0) (if (flip q) 1 0)))))
   (define n_ (sum probs))
   trulycoin
   (= n n_)
))

(hist (repeat 10000 mpp2))


(define n 19)
(define m 1)
(define N (+ n m))
(define (sample)
  (rejection-query
   (define fair (flip 0.99))
   (define q (if fair 0.5 (uniform 0 1)))
   (define probs (repeat N (lambda() (if (flip q) 1 0))))
   (define n_ (sum probs))
   fair
   (= n n_)
   ))
(hist (repeat 100 sample))
(define res (repeat 1000 sample))
;;(display res)

(/ (sum (map (lambda(x) (if x 1 0)) res)) 1000)
