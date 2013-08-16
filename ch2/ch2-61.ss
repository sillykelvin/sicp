(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (define (insert x head tail)
    (cond ((null? tail) (append head (list x)))
          ((< x (car tail)) (append head (list x) tail))
          (else (insert x (append head (list (car tail))) (cdr tail)))))
  (if (element-of-set? x set)
      set
      (cond ((null? set) (list x))
            ((< x (car set)) (cons x set))
            (else (insert x (list (car set)) (cdr set))))))


;;; test

(adjoin-set 1 '())
(adjoin-set 1 '(1 2))
(adjoin-set 1 '(2 3))
(adjoin-set 2 '(1 2 3))
(adjoin-set 2 '(1 3))
(adjoin-set 3 '(1 2 4 5))
(adjoin-set 5 '(1 2 3))
