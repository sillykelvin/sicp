(define (make-tree entry left right)
  (list entry left right))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (entry tree) (car tree))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-set set1 set2)
  (define (list-union-set set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
             (cond ((= x1 x2) (cons x1
                                    (list-union-set (cdr set1)
                                                    (cdr set2))))
                   ((< x1 x2) (cons x1
                                    (list-union-set (cdr set1)
                                                    set2)))
                   ((> x1 x2) (cons x2
                                    (list-union-set set1
                                                    (cdr set2)))))))))
  (list->tree (list-union-set (tree->list set1)
                              (tree->list set2))))

(define (intersection-set set1 set2)
  (define (list-intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
             (cond ((= x1 x2) (cons x1
                                    (list-intersection-set (cdr set1)
                                                           (cdr set2))))
                   ((< x1 x2) (list-intersection-set (cdr set1) set2))
                   ((> x1 x2) (list-intersection-set set1
                                                     (cdr set2))))))))
  (list->tree (list-intersection-set (tree->list set1)
                                     (tree->list set2))))


;;; test

(union-set '() '())
(union-set '() '(4 (2 (1 () ())
                      (3 () ()))
                   (5 () ())))
(union-set '(6 (4 (3 () ()) ())
               (8 (7 () ())
                  (9 () ())))
           '(4 (2 (1 () ())
                  (3 () ()))
               (5 () ())))

(intersection-set '() '())
(intersection-set '() '(1 () ()))
(intersection-set '(6 (4 (3 () ()) ())
                      (8 (7 () ())
                         (9 () ())))
                  '(4 (2 (1 () ())
                         (3 () ()))
                      (5 () ())))
