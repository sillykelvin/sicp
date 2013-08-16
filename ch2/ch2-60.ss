(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))


;;; test

(element-of-set? 1 '())
(element-of-set? 1 '(2 3))
(element-of-set? 1 '(1 2 3))
(element-of-set? 1 '(2 1 3 1))

(intersection-set '() '())
(intersection-set '() '(1 2 3))
(intersection-set '(1 2 2 1 3) '(2 3 4 2 3))

(union-set '() '())
(union-set '() '(1 2 1 3))
(union-set '(1 2 2 3) '(2 3 4 3))
