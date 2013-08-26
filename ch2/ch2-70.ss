;;; test

(define symbol-frequency-pairs
  '((A 2) (NA 16)
    (BOOM 1) (SHA 3)
    (GET 2) (YIP 9)
    (JOB 2) (WAH 1)))

(define huffman-tree (generate-huffman-tree symbol-frequency-pairs))

(encode '(GET A JOB
          SHA NA NA NA NA NA NA NA NA
          GET A JOB
          SHA NA NA NA NA NA NA NA NA
          WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
          SHA BOOM) huffman-tree)

(length (encode '(GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  GET A JOB
                  SHA NA NA NA NA NA NA NA NA
                  WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                  SHA BOOM) huffman-tree))
