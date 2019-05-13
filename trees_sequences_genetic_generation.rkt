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
    (if (flip 0.2)
        (mutation-tumors jongen)
        jongen)
    ))
(crossmute '(* n 2) '(+ (* x1 x2) n))
;;фитнес-функция - СКО? Кто лучше всех, того и берем в родители?
(define (rms numsT numsM)
  (/ (for/sum ([nM numsM][nT numsT]) (* (- nM nT) (- nM nT))) (length numsT)))

;;генерация малышей для соответсвтвия последовательности
;(define (ramped-half-and-half num)
;  (for/list ([i num]) (gen-expr)))

;jp

(define (ramped-half-and-half)
  (for*/vector ([quantity (range 0 50)])
    (gen-expr)))


;(reverse (gen-seq-next 'n '(1 0) 2 6))
;сортировка по mse
(define (sort-by-mse lst target)
  (sort lst < #:key (lambda (e) (rms target (reverse (gen-seq-next e (list (second target) (first target)) 2 (length target)))))))

;(define loljp (sort-by-mse jp '(0 1 2 3 4 5)));
;loljp
;(for/list ([i loljp]) (rms '(0 1 2 3 4 5) (reverse (gen-seq-next i (list (second '(0 1 2 3 4 5)) (first '(0 1 2 3 4 5))) 2 (length '(0 1 2 3 4 5))))))

(struct EvaluatedProgram
  (tree fitness))
;;our sequence to find
(define target '(0 1 6 33 196 1335 10398))
;(define target '(0 1 1 2 3 5 8 13))
;(define target '(1 1 2 6 24 120 720))

(define (evaluate-fitness tree)
  (let* ([f (gen-seq-next tree (list (second target) (first target)) 2 (length target))]
         [output (reverse f)]
         [c (optree-node-count tree)])
    (foldl (λ (x y acc) (+ acc
                           (* (- x y) (- x y))))
           0
           output
           target)))
  
(define (evaluate-programs vec)
  (vector-map! (λ (tree) (EvaluatedProgram tree (* (+ 1 (optree-node-count tree)) (evaluate-fitness tree))))
               vec))


(define (n-most-fit n vec)
  (let* ([ls (vector->list vec)]
         [sorted (sort ls (λ (p1 p2) (< (EvaluatedProgram-fitness p1)
                                        (EvaluatedProgram-fitness p2))))])
    (take sorted n)))

(define (distinct-randoms bound)
  (let ([x (random bound)]
        [y (random bound)])
    (if (= x y)
        (distinct-randoms bound)
        (values x y))))

(define (tournament-round prog1 prog2)
  (crossmute (EvaluatedProgram-tree prog1)
                                (EvaluatedProgram-tree prog2)))


(define (next-generation vec)
  (define foo (make-vector 200))
  (define champions (n-most-fit 40 vec))
  (for ([i (range 0 140)])
    (let-values ([(n1 n2) (distinct-randoms (vector-length vec))])
      (let ([tree (tournament-round (vector-ref vec n1) (vector-ref vec n2))])
        (vector-set! foo i tree))))
  (for ([i (range 160 200)])
    (vector-set! foo i (EvaluatedProgram-tree (list-ref champions (- i 160)))))
  (for ([i (range 120 160)])
    (vector-set! foo i (mutation-nodes (EvaluatedProgram-tree (vector-ref vec (random (vector-length vec)))))))
  foo)


(define run-gp
  (λ (#:max-generations [max-generations 700])
    (define (run population generation)
      (evaluate-programs population)
      (let* ([leaders (n-most-fit 10 population)]
             [peak-fitness (EvaluatedProgram-fitness (car leaders))]
             [best-program (EvaluatedProgram-tree (car leaders))])
        (displayln (string-append "Generation " (number->string generation)))
        (cond
          [(= peak-fitness 0) (displayln "Solution found!")
                              (displayln best-program)]
          [else (display "Top fitness scores: ")
                (displayln (map EvaluatedProgram-fitness leaders))
                (display "\n")
                (cond
                  [(= generation max-generations)
                   (displayln (string-append "No solution found in "
                                             (number->string max-generations)
                                             " generations."))
                   (display "Most fit solution: ")
                   (displayln best-program)]
                  [else (run (next-generation population) (+ generation 1))])])))
    (run (ramped-half-and-half) 1)))

(run-gp)