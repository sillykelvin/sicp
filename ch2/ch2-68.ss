(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (symbol-on-tree? symbol tree)
  (memq symbol (symbols tree)))

(define (encode-symbol symbol tree)
  (cond ((not (symbol-on-tree? symbol tree))
         (error "symbol is not on this tree"))
        ((leaf? tree) '())
        (else (let ((left (left-branch tree))
                    (right (right-branch tree)))
                (if (symbol-on-tree? symbol left)
                    (cons 0 (encode-symbol symbol left))
                    (cons 1 (encode-symbol symbol right)))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


;;; test

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-symbol-sequence '(A D A B B C A))

(encode sample-symbol-sequence sample-tree)
