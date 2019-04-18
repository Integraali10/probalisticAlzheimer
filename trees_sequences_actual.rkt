#lang racket

(define (flip p) (< (random) p))

(define (bin-tree)
  (if (flip 0.45)
      (list (bin-tree) (bin-tree))
      (flip 0.5)))
;(bin-tree)

(define select-random
  (lambda (ls)
    (let ((len (length ls)))         
      (list-ref ls (random len)))))

;(define b (bin-tree))
;b

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t)) 
                 (count-leaves (cdr t))))))

;(count-leaves b)
(define (operbin-tree)
  (if (flip 0.20)
      (list (select-random '(+ - *))(operbin-tree) (operbin-tree));; division!!!
      (if (flip 0.5)
          (random 10)
          'n)))

(define (count-leaves2 expr)
  (if (list? expr)
      (if (null? expr)
          0
          (apply + (map count-leaves expr))) 1))

(define (optree-leaves-count t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (optree-leaves-count (cadr t)) 
                 (optree-leaves-count (caddr t))))))

(define (optree-node-count t)
  (cond ((null? t) 0)
        ((not (pair? t)) 0)
        (else (+ 1
                 (optree-node-count (cadr  t))
                 (optree-node-count (caddr t))))))

(define ns (make-base-namespace))

;(define extree (operbin-tree))
;(writeln "Expression")
;extree
;(writeln "Num of Nodes")
;(optree-node-count extree)
;(writeln "Num of Leaves")
;(optree-leaves-count extree)
;(writeln "Result of eval")
;(eval extree ns)

(define funcs
  (list (list '- -)
        (list '+ +)
        (list '/ /)
        (list '* *)))

(define (interpret expr n)
  (if (list? expr)
      (apply
       (second(assoc (car expr) funcs))
       (map interpret (cdr expr) n)) expr
                                     ))

(define (compute tree n)
  (if (not(list? tree))
      (if (symbol? tree)
          n
          tree)
      ((function-named-by (list-ref tree 0))
       (compute (cadr tree) n)
       (compute (caddr tree) n))))

(define (function-named-by oper)
  (cond ((equal? oper '+) +)
        ((equal? oper '-) -)
        ((equal? oper '*) *)
        ((equal? oper '/) /)
        (else (error "no such operator as" oper))))
;(writeln "Result of our computation")
;(interpret extree)
;(compute extree 4)


(define (mySum L)
  (apply + L))

;to-do!!!! multinomial destribution
;(multinomial '(1 2 3 4) '(1 1 1))

;(define (helper rs)
; (define r (random))
;if (< r ))

;(define (multi-random) 
; (lambda (ls rs)
;  (let ((len (length ls)))         
;   (list-ref ls (random len)))))


(define (seq-compute expr nelem)
  (for/list ([i (in-range 1 (+ 1 nelem))]) (compute expr i)))

;(seq-compute extree 10)
  
;(define (rejection-sampling lst)
;  (local {(define expr (operbin-tree))}
;  (if (equal? (seq-compute expr (length lst)) lst)
;      expr
;      (rejection-sampling lst)))
;)

(define (rejection-sampling-simple lst)
  (let* ([expr (operbin-tree)]
         [l  (seq-compute expr (length lst))])
    (if (equal? l lst)
        expr
        (rejection-sampling-simple lst))))

(define (append-element lst elem)
  (append lst (list elem)))

(define (rejection-N-samplings-simple lst N [acc '()])
  (let* ([expr (operbin-tree)]
         [l  (seq-compute expr (length lst))])
    (if (eq? N 0)
        acc
        (if (equal? l lst)
            (rejection-N-samplings-simple lst (- N 1) (append-element acc expr))
            (rejection-N-samplings-simple lst (- N 1) acc)))))

;(define x '(1 4 9 16 25))

;(rejection-sampling-simple x)
;(rejection-N-samplings-simple x 1000)


(define (oper2D-tree)
  (cond
    [(flip 0.4) (list (select-random '(+ - *))(oper2D-tree) (oper2D-tree))]
    [(flip 0.3) 'n]
    [(flip 0.5) 'x]
    [(flip 0.5) 'y]
    [else  (random 10)]))

(define (compute2 tree x y [N 3])
  (if (not(list? tree))
      (cond
        [(number? tree) tree]
        [(eq? tree 'x) x]
        [(eq? tree 'y) y]
        [(eq? tree 'n) N])
      ((second(assoc (car tree) funcs))
       (compute2 (cadr tree) x y N)
       (compute2 (caddr tree) x y N))))

(define exp2tree (oper2D-tree))
exp2tree
(compute2 exp2tree 1 2)

(define (seq-compute2 tree N)
  (for/list ([i (1 2)]) (compute2 tree (- i 1) (- i 2) i)))

      
  