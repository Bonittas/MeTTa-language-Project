#lang racket


(define (apply-law expr laws)
  (cond
    [(null? laws) expr]
    [else
     (let ((result ((car laws) expr)))
       (if (equal? result expr)
           (apply-law expr (cdr laws))
           result))]))

(define (identity-law expr)
  (cond
    [(equal? expr '(+ A 0)) 'A]
    [(equal? expr '(* A 1)) 'A]
    [else expr]))

(define (null-law expr)
  (cond
    [(equal? expr '(+ A 1)) 1]
    [(equal? expr '(* A 0)) 0]
    [else expr]))

(define (complement-law expr)
  (cond
    [(equal? expr '(+ A (not A))) 1]
    [(equal? expr '(* A (not A))) 0]
    [else expr]))

(define (idempotent-law expr)
  (cond
    [(equal? expr '(+ A A)) 'A]
    [(equal? expr '(* A A)) 'A]
    [else expr]))

(define (associative-law expr)
  (cond
    [(and (pair? expr) (equal? (car expr) '+) (pair? (cadr expr)) (equal? (caadr expr) '+))
     (let* ((A (cadadr expr))
            (B (cddadr expr))
            (C (cddr expr)))
       `(+ ,A (+ ,B ,C)))]
    [(and (pair? expr) (equal? (car expr) '*) (pair? (cadr expr)) (equal? (caadr expr) '*))
     (let* ((A (cadadr expr))
            (B (cddadr expr))
            (C (cddr expr)))
       `(* ,A (* ,B ,C)))]
    [else expr]))

(define (distributive-law expr)
  (cond
    [(and (pair? expr) (equal? (car expr) '*) (pair? (cadr expr)) (equal? (caadr expr) '+))
     (let* ((A (cadr expr))
            (B (caddr expr))
            (C (cddr expr)))
       `(+ (* ,A ,B) (* ,A ,C)))]
    [(and (pair? expr) (equal? (car expr) '+) (pair? (cadr expr)) (equal? (caadr expr) '*))
     (let* ((A (cadr expr))
            (B (caddr expr))
            (C (cddr expr)))
       `(* (+ ,A ,B) (+ ,A ,C)))]
    [else expr]))

(define (demorgans-law expr)
  (cond
    [(and (pair? expr) (equal? (car expr) 'not) (pair? (cadr expr)))
     (let ((inner-expr (cadr expr)))
       (cond
         [(equal? (car inner-expr) '+) `(* (not ,(cadr inner-expr)) (not ,(caddr inner-expr)))]
         [(equal? (car inner-expr) '*) `(+ (not ,(cadr inner-expr)) (not ,(caddr inner-expr)))]
         [else expr]))]
    [else expr]))

(define (simplify expr)
  (let ((laws (list identity-law null-law complement-law idempotent-law associative-law distributive-law demorgans-law)))
    (define (simplify-helper expr)
      (let ((simplified (apply-law expr laws)))
        (if (pair? simplified)
            (map simplify-helper simplified)
            simplified)))
    (simplify-helper expr)))

(define (main)
  (let ((expressions '((+ A 0) (* A 1) (+ A 1) (* A 0) (+ A (not A)) (* A (not A)) (+ A A) (* A A) 
                       (* A (+ B C)) (+ A (* B C)) (not (+ A B)) (not (* A B)))))
    (for-each
     (lambda (expr)
       (display "Expression: ")
       (display expr)
       (display " => Simplified: ")
       (display (simplify expr))
       (newline))
     expressions)))

(main)
