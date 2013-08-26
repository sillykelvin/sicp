(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (cadr leaf-set)
                                                    (car leaf-set))
                                    (cdr (cdr leaf-set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;;; test

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
