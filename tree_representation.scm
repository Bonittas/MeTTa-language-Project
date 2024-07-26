#lang racket

(define tree '(1 (2 (4 () ()) (5 () ())) (3 () ())))

(define (tree-height tree)
  (if (null? tree)
      0
      (+ 1 (max (tree-height (cadr tree)) (tree-height (caddr tree))))))

(define (main)
  (display "Tree: ")
  (display tree)
  (newline)
  (display "Height of the tree: ")
  (display (tree-height tree))
  (newline))

(main)
