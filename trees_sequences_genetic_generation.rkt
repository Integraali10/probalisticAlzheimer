#lang racket

(define (flip p)
  (< (random) p))

(define list-op '(+ - *))

(define (choose-random xs)
  (list-ref xs (random (length xs))))

(define (gen-expr)
  (if (flip 0.45)
      (list (choose-random list-op)
            (gen-expr)
            (gen-expr))
      (let ([n (random 13)])
        (cond
          [(eq? n 10) 'n]
          [(eq? n 11) 'x1]
          [(eq? n 12) 'x2]
          [else n]))))

(define (count-leaves expr)
  (if (list? expr)
      (if (null? expr)
          0
          (apply + (map count-leaves expr)))
      1))

(define funcs
  (list (list '- -)
        (list '+ +)
        (list '* *)))

(define (interpret expr n x1 x2)
  (if (list? expr)
      (apply
       (second (assoc (car expr) funcs))
       ;(cond
       ;  [(eq? (car expr) '+) +]
       ;  [(eq? (car expr) '-) -]
       ;  [(eq? (car expr) '*) *])
       (map (lambda (e) (interpret e n x1 x2))
            (cdr expr)))
      (cond
        [(number? expr) expr]
        [(equal? expr 'n) n]
        [(equal? expr 'x1) x1]
        [(equal? expr 'x2) x2])))

;(define (gen-seq expr N)
;  (map ((curry interpret) expr) (range N)))

; REM: generating in the reverse order
; n can be get rid off, in fact, if we use (length seq)
(define (gen-seq-next expr seq n N)
  (if (= n N)
      seq
      (gen-seq-next expr
       (cons
        (interpret expr n (first seq) (second seq))
        seq)
       (+ n 1)
       N)))

;(define (reject expr target)
;  (if (equal? (gen-seq expr (length target)) target)
;      expr
;      (reject (gen-expr) target)))

;(define (reject-n results target n-tries)
;  (if (<= n-tries 0)
;      results
;      (let* ([expr (gen-expr)]
;             [ys (gen-seq expr (length target))])
;        (reject-n
;         (if (equal? ys target)
;             (cons expr results)
;             results)
;         target (- n-tries 1)))))

; (reject-n '() '(1 2 3) 1000)

(define (reject-n-seq results target n-tries)
  (if (<= n-tries 0)
      results
      (let* ([expr (gen-expr)]
             [ys (gen-seq-next
                  expr
                  (list (second target) (first target))
                  2 (length target))])
        (reject-n-seq
         (if (equal? (reverse ys) target)
             (cons expr results)
             results)
         target (- n-tries 1)))))

;;very cool, most impressive
;(define results
;  (reject-n-seq '() '(0 1 6 33 196 1335 10398) 10000000))
;(define predict
;  (map (lambda (e) (car (gen-seq-next e '(1 0) 2 9)))
;       results))
;(map cons results predict)
;(first results)


(define lol '(* (+ n (* x1 (+ (* x2 n) (* x1 x1)))) (+ (+ (* 2 x2) (* x1 n)) n)))
lol



(define (optree-node-count t)
  (cond ((null? t) 0)
        ((not (pair? t)) 0)
        (else (+ 1
                 (optree-node-count (cadr  t))
                 (optree-node-count (caddr t))))))


;;mutation 1: changing operations, saving structure
(define (mutation-nodes creature)
   (if (list? creature)
       (list (choose-random list-op)
            (mutation-nodes (cadr creature))
            (mutation-nodes (caddr creature)))
       (if (flip 0.15)
           creature
           (cond
             [(flip 0.3) 'n]
             [(flip 0.5) 'x1]
             [(flip 0.5) 'x2]
             [else  (random 10)]))))

(mutation-nodes lol)
;;mutation 2: cancer tree

(define (muta-depth depth creature)
  (let* ([n (optree-node-count creature)])
   (if (list? creature)
       (if (flip (/ (- n depth) n))
           (cond
             [(flip 0.5) (list (car creature)
                               (muta-depth (+ 1 depth) (cadr creature))
                               (gen-expr))]
             [(flip 0.5) (list (car creature)
                               (gen-expr)
                               (muta-depth (+ 1 depth) (caddr creature)))]
             [else creature])
           creature)
       creature)))

(define (mutation-tumors creature)
  (muta-depth 1 creature))

(optree-node-count lol)
(mutation-tumors lol)
;;скрещивание (смотри в книжке описание)

(define (crossbreeding cr1 cr2)
  (cond
    [(and (list? cr1)(list? cr2))
      (cond
        [(flip 0.45) (list (car cr1) (cadr cr1) (crossbreeding (cadr cr1) (caddr cr2)))]
        [(flip 0.45) (list (car cr2) (crossbreeding (cadr cr1) (caddr cr2)) (caddr cr2))]
        [(flip 0.5) (list (car cr1) (crossbreeding (cadr cr1) (caddr cr2)) (crossbreeding (caddr cr1) (cadr cr2)))]
        [else (list (car cr2) (crossbreeding (cadr cr1) (caddr cr2)) (crossbreeding (caddr cr1) (cadr cr2)))])]
    [(and (list? cr1)(not (list? cr2)))
      (cond
        [(flip 0.45) (list (car cr1) (cadr cr1) (crossbreeding (cadr cr1) cr2))]
        [(flip 0.45) (list (car cr1) (crossbreeding (cadr cr1) cr2) cr2)]
        [else (list (car cr1) (crossbreeding (cadr cr1) cr2) (crossbreeding (caddr cr1) cr2))])]
    [(and (list? cr2) (not (list? cr1)))
      (cond
        [(flip 0.45) (list (car cr2) cr1 (crossbreeding cr1 (caddr cr2)))]
        [(flip 0.45) (list (car cr2) (crossbreeding cr1 (caddr cr2)) (caddr cr2))]
        [else (list (car cr2) (crossbreeding cr1 (cadr cr2)) (crossbreeding cr1 (caddr cr2)))])]
    [else (if (flip 0.5) cr1 cr2)]))


(crossbreeding '(* n 2) '(+ (* x1 x2) n))

(define (crossmute cr1 cr2)
  (let* ([jongen (crossbreeding cr1 cr2)])
    (if (flip 0.1)
        (mutation-tumors jongen)
        jongen)
    ))
(crossmute '(* n 2) '(+ (* x1 x2) n))
;;фитнес-функция - СКО? Кто лучше всех, того и берем в родители?
(define (rms numsT numsM)
  (sqrt (/ (for/sum ([nM numsM][nT numsT]) (* (- nM nT) (- nM nT))) (length numsT))))

(rms '(0 1 2 3 4 5) '(0 1 2 3 4 5))

;;генерация малышей для соответсвтвия последовательности
(define (justpopuli num)
  (for/list ([i num]) (gen-expr)))
(define jp (justpopuli 4))
jp

(reverse (gen-seq-next 'n '(1 0) 2 6))

(define (sort-by-mse lst target)
  (sort lst < #:key (lambda (e) (rms target (reverse (gen-seq-next e (list (second target) (first target)) 2 (length target)))))))

(define loljp (sort-by-mse jp '(0 1 2 3 4 5)))
loljp
(for/list ([i loljp]) (rms '(0 1 2 3 4 5) (reverse (gen-seq-next i (list (second '(0 1 2 3 4 5)) (first '(0 1 2 3 4 5))) 2 (length '(0 1 2 3 4 5))))))

; (define (genetic-n-seq guys target MSE)
;   (if (<= MSE 0.00001)
;       (first guys)
;       (let* ([expr1 (first guys)]
;              [expr2 (second guys)]
;              [ys1 (gen-seq-next
;                   expr1
;                   (list (second target) (first target))
;                   2 (length target))]
;              [ys2 (gen-seq-next
;                   expr2
;                   (list (second target) (first target))
;                   2 (length target))]
;              [mse1 (rms target ys1)]
;              [mse2 (rms target ys2)])
;         (reject-n-seq
;          (if (equal? (reverse ys) target)
;              (cons expr results)
;              results)
;          target (- n-tries 1)))))


